# include <RcppArmadillo.h>
# include <ClusterRHeader.h>
# include <affinity_propagation.h>
# include <OpenImageRheader.h>
// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::depends(ClusterR)]]
// [[Rcpp::depends(OpenImageR)]]
// [[Rcpp::plugins(cpp11)]]


# include <cmath>
# include <math.h>
# include <stdio.h>
# include <string>
# include <unordered_map>


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// similarity function [ negative euclidean -- which gives the best results in the article ]
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// [[Rcpp::export]]
double simil_A(arma::rowvec spx_vec1, arma::rowvec spx_vec2, int wL = 3, int wA = 10, int wB = 10) {

  double value = -( wL * std::pow((spx_vec1(0) - spx_vec2(0)), 2.0) + wA * std::pow((spx_vec1(1) - spx_vec2(1)), 2.0) + wB * std::pow((spx_vec1(2) - spx_vec2(2)), 2.0) );

  return value;
}


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// apply(im, 3, as.vector) in rcpp
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// [[Rcpp::export]]
arma::mat apply_rcpp(arma::cube &input) {

  arma::mat res(input.n_rows * input.n_cols, 3);

  for (unsigned int i = 0; i < (input.n_rows * input.n_cols); i++) {
    res(i,0) = input.slice(0)(i);
    res(i,1) = input.slice(1)(i);
    res(i,2) = input.slice(2)(i);
  }

  return res;
}


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// find which rows of the kmeans-centroids include NA's
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// [[Rcpp::export]]
arma::uvec NAs_matrix(arma::mat &x) {

  arma::uvec all_rows;
  int item = 0;

  if (x.has_nan()) {
    for (unsigned int j = 0; j < x.n_cols; j++) {
      arma::uvec find_row = arma::find_nonfinite(x.col(j));
      for (unsigned int k = 0; k < find_row.n_elem; k++) {
        item++;
        all_rows.resize(item);
        all_rows(item-1) = find_row(k);
      }
    }
    all_rows = arma::unique(all_rows);
  }
  return all_rows;
}


//------------------------------
// check if the matrix is finite
//------------------------------

// [[Rcpp::export]]
bool is_mt_finite(arma::mat x) {

  clustR::ClustHeader clsh;

  return clsh.check_NaN_Inf(x);
}


//----------------------------
// image segmentation function [ superpixels, affinity propagation, kmeans ]
//----------------------------

// [[Rcpp::export]]
Rcpp::List image_segmentation(arma::cube input_image,
                              std::string method = "slic",
                              int num_superpixel = 200,
                              std::string kmeans_method = "",
                              bool AP_data = false,
                              bool use_median = true,
                              int minib_kmeans_batch = 10,
                              double minib_kmeans_init_fraction = 0.5,
                              int kmeans_num_init = 3,
                              int kmeans_max_iters = 100,
                              std::string kmeans_initializer = "kmeans++",
                              std::string colour_type = "RGB",
                              double compactness_factor = 20,
                              bool adjust_centroids_and_return_masks = false,
                              bool return_labels_2_dimensionsional = false,
                              bool sim_normalize = false,
                              int sim_wL = 3,
                              int sim_wA = 10,
                              int sim_wB = 10,
                              int sim_color_radius = 20,
                              bool verbose = false) {

  Affinity_Propagation afpr;
  clustR::ClustHeader clsh;

  if (kmeans_method == "" && !AP_data) { Rcpp::stop("Either the 'kmeans_method' should be set to 'kmeans' / 'mini_batch_kmeans' or the 'AP' parameter should be set to TRUE!");  }
  if (adjust_centroids_and_return_masks && kmeans_method == "") { Rcpp::Rcout << "WARNING: Set the 'kmeans_method' parameter to either 'kmeans' or 'mini_batch_kmeans' to receive image-masks data!" << std::endl; }
  if (verbose) { Rcpp::Rcout << "Sequential computation starts ..." << std::endl; }

  oimageR::Utility_functions UTLF;
  Rcpp::List lst_obj = UTLF.interface_superpixels(input_image, method, num_superpixel, compactness_factor, true, true, true, "", false);

  if (verbose) { Rcpp::Rcout << "The super-pixel " << method << " method as pre-processing step was used / completed!" << std::endl; }

  arma::cube im_3d_obj = Rcpp::as<arma::cube>(lst_obj["slic_data"]);
  arma::mat labels_obj = Rcpp::as<arma::mat>(lst_obj["labels"]);
  arma::cube lab_obj = Rcpp::as<arma::cube>(lst_obj["lab_data"]);

  arma::colvec unq_labels = arma::unique(arma::vectorise(labels_obj));
  int ITEMS = unq_labels.n_elem;

  arma::mat lst_3d_vecs(ITEMS, 3, arma::fill::zeros);
  int inner_iters = 3;

  for (int i = 0; i < ITEMS; i++) {
    arma::uvec idx = arma::find(labels_obj == unq_labels(i));
    if (idx.n_elem > 0) {                                                           // it is possible that the unique number of labels is less than the ITEMS
      arma::rowvec inner_vec(3, arma::fill::zeros);
      for (int k = 0; k < inner_iters; k++) {
        arma::mat in_mt = lab_obj.slice(k);                                         // use 'rgb_2lab' as input, as the similarity function requires 'Lab' input data
        if (use_median) {
          inner_vec(k) = arma::median(arma::vectorise(in_mt(idx)));
        }
        else {
          inner_vec(k) = arma::mean(arma::vectorise(in_mt(idx)));
        }
      }
      lst_3d_vecs.row(unq_labels(i)) = inner_vec;                                   // keep track of the 'unq_labels' because after calculating the distance matrix I have to retrieve the initial labels
    }
  }

  arma::rowvec vec_avg((ITEMS * ITEMS) - ITEMS);                                                              // number of elements of the similarity matrix excluding the main diagonal

  arma::mat simil_mt(ITEMS,ITEMS, arma::fill::zeros);
  for (int i = 0; i < ITEMS; i++) {
    for (int j = 0; j < ITEMS; j++) {
      if (i != j) {                                                                                           // calculate the distance only for the off-diagonal items (items not in the main diagonal)
        double tmp_dist = simil_A(lst_3d_vecs.row(i), lst_3d_vecs.row(j), sim_wL, sim_wA, sim_wB);            // same note for 'unq_labels' as before
        vec_avg = tmp_dist;                                                                                   // use the similarities both to construct the similarity matrix and to extract the preference value
        simil_mt(i,j) = tmp_dist;
      }
    }
  }

  double norm_avg = 0;                                                                                        // recalculate the preference value based on the center-scaled data
  if (sim_normalize) {                                                                                        // normalize the similarity matrix
    simil_mt = arma::normalise(simil_mt);
    if (!use_median) {
      int norm_items = 0;
      for (unsigned int f = 0; f < simil_mt.n_rows; f++) {
        for (unsigned int ff = 0; ff < simil_mt.n_cols; ff++) {
          if (f != ff) {
            norm_avg += simil_mt(f,ff);
            norm_items++;
          }
        }
      }
      norm_avg /= norm_items;
    }
  }

  double avg_off_diag = 0;                                                            // set the preference value taking into account the 'colorradius' [ 'use_median' option too, in contrary to the mean value of the article ]
  if (use_median) {
    avg_off_diag = arma::median(arma::vectorise(simil_mt));
  }
  else {
    if (sim_normalize) {
      avg_off_diag = norm_avg;
    }
    else {
      avg_off_diag = arma::mean(vec_avg);                                             // the mean value is based on the off-diagonal elements only
    }
  }
  double set_preference_value = sim_color_radius * avg_off_diag;

  if (verbose) { Rcpp::Rcout << "The similarity matrix based on super-pixels was computed!" << std::endl; }


  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // affinity propagation with default values     [ use affinity propagation on super-pixels because it does not scale well in high dimensions ]
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  std::vector<double> preference;
  preference.push_back(set_preference_value);

  Rcpp::List afpr_res = afpr.affinity_propagation(simil_mt, preference, 1000, 100, 0.9, false, 0.0, 2.2204e-16, false);                        // the affinity propagation algorithm returns clusters for the superpixels, thus it's not possible to 'adjust_centroids_and_return_masks' as is the case in kmeans

  int affinty_num_clusters = Rcpp::as<int>(afpr_res["K"]);                                                                                     // number of clusters
  std::string aff_str_num = std::to_string(affinty_num_clusters);
  int AP_iterations = Rcpp::as<int>(afpr_res["iterations"]);
  std::string str_iterations = std::to_string(AP_iterations);
  std::vector<int> exemplars = Rcpp::as<std::vector<int> >(afpr_res["exemplars"]);                                                             // exemplars

  std::unordered_map<int, std::vector<int> > clusters;                                                                                         // workaround to get the clusters and their indices as an unordered_map because it can't be done directly using Rcpp::as<unorderd_map<...
  Rcpp::List tmp_clusts = Rcpp::wrap(afpr_res["clusters"]);
  Rcpp::CharacterVector clust_nams = tmp_clusts.names();
  for (int n = 0; n < clust_nams.size(); n++) {
    std::string str_in = Rcpp::as<std::string >(clust_nams[n]);
    std::vector<int> vec_in = Rcpp::as<std::vector<int> >(tmp_clusts[n]);
    int val_in = std::stoi(str_in);
    clusters[val_in] = vec_in;
  }

  if (verbose) { Rcpp::Rcout << "It took " + str_iterations + " iterations for affinity propagation to complete!" << std::endl; }
  if (verbose) { Rcpp::Rcout << aff_str_num + " clusters were chosen based on super-pixels and affinity propagation!" << std::endl; }

  arma::cube hsv_tmp;
  if (colour_type == "HSV") {
    hsv_tmp = UTLF.RGB_to_HSV(input_image);
  }

  arma::cube ap_new_im;

  if (AP_data) {

    ap_new_im.set_size(input_image.n_rows, input_image.n_cols, input_image.n_slices);
    std::vector<int> ap_keys;                                                                         // obtain the clusters
    int CLUSTS = clusters.size();
    ap_keys.reserve(CLUSTS);

    for(auto kv : clusters) {
      ap_keys.push_back(kv.first);
    }
    std::unordered_map<int, std::vector<int> > idx_clusters;
    for (unsigned int d = 0; d < ap_keys.size(); d++) {
      std::vector<int> sub_clust = clusters[ap_keys[d]];
      for (unsigned int dd = 0; dd < sub_clust.size(); dd++) {
        arma::uvec idx = arma::find(labels_obj == sub_clust[dd]);
        if (idx.size() > 0) {
          for (unsigned int ddd = 0; ddd < idx.size(); ddd++) {
            idx_clusters[ap_keys[d]].push_back(idx[ddd]);                                             // assign the indices of each sub-cluster
          }
        }
      }
    }
    for (int f = 0; f < CLUSTS; f++) {
      arma::uvec tmp_cl_idx = arma::conv_to<arma::uvec>::from(idx_clusters[ap_keys[f]]);
      for (unsigned int ff = 0; ff < input_image.n_slices; ff++) {
        double avg_clust_idx;                                                                         // return the transformed image based on the specified colour-type
        if (colour_type == "RGB") {
          if (use_median) {
            avg_clust_idx = arma::median(arma::vectorise(input_image.slice(ff)(tmp_cl_idx)));
          }
          else {
            avg_clust_idx = arma::mean(arma::vectorise(input_image.slice(ff)(tmp_cl_idx)));
          }
        }
        else if (colour_type == "LAB") {
          if (use_median) {
            avg_clust_idx = arma::median(arma::vectorise(lab_obj.slice(ff)(tmp_cl_idx)));
          }
          else {
            avg_clust_idx = arma::mean(arma::vectorise(lab_obj.slice(ff)(tmp_cl_idx)));
          }
        }
        else if (colour_type == "HSV") {
          if (use_median) {
            avg_clust_idx = arma::median(arma::vectorise(hsv_tmp.slice(ff)(tmp_cl_idx)));
          }
          else {
            avg_clust_idx = arma::mean(arma::vectorise(hsv_tmp.slice(ff)(tmp_cl_idx)));
          }
        }
        else {
          Rcpp::stop("Invalid 'colour_type' parameter!");
        }
        ap_new_im.slice(ff)(tmp_cl_idx).fill(avg_clust_idx);                                          // use the subset of cluster-indices to assign the average value
      }
    }

    if (verbose) { Rcpp::Rcout << "Image data based on Affinity Propagation clustering ('AP_image_data') will be returned!" << std::endl; }
  }


  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // process the initial data for vector quantization
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (input_image.empty()) { Rcpp::stop("the initial 3-dimensional data is an empty object!"); }

  arma::mat im2;

  if (colour_type == "RGB") {
    im2 = apply_rcpp(input_image);
  }
  else if (colour_type == "LAB") {
    im2 = apply_rcpp(lab_obj);
  }
  else if (colour_type == "HSV") {
    im2 = apply_rcpp(hsv_tmp);
  }
  else {
    Rcpp::stop("Invalid 'colour_type' parameter!");
  }


  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // use the affinity-propagation number of clusters as
  // input to the k-means or mini-batch-kmeans algorithm
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  Rcpp::List kmeans_obj;
  std::string which_km;
  arma::mat getcent;
  arma::rowvec getclust;
  arma::cube new_im;
  arma::field<arma::cube> masks_lst;

  if (kmeans_method != "") {

    if (kmeans_method == "mini_batch_kmeans") {

      kmeans_obj = clsh.mini_batch_kmeans(im2, affinty_num_clusters, minib_kmeans_batch, kmeans_max_iters, kmeans_num_init, minib_kmeans_init_fraction,
                                          kmeans_initializer, minib_kmeans_batch, false, R_NilValue, 1e-4, 0.5, 1);

      getcent = Rcpp::as<arma::mat>(kmeans_obj[0]);
      Rcpp::List preds_lst = clsh.Predict_mini_batch_kmeans(im2, getcent, false, 1.0e-6);
      getclust = Rcpp::as<arma::rowvec>(preds_lst[0]);
      which_km = "mini-batch-kmeans";
    }
    else if (kmeans_method == "kmeans") {
      kmeans_obj = clsh.KMEANS_rcpp(im2, affinty_num_clusters, kmeans_num_init, kmeans_max_iters,
                                    kmeans_initializer, false, false, R_NilValue, 1e-4, 1.0e-6, 0.5, 1);

      getcent = Rcpp::as<arma::mat>(kmeans_obj["centers"]);
      getclust = Rcpp::as<arma::rowvec>(kmeans_obj["clusters"]);
      which_km = "kmeans";
    }
    else {
      Rcpp::stop("Invalid method for the 'kmeans_method' parameter!");
    }
    if (verbose) { Rcpp::Rcout << "The " + which_km + " algorithm based on the number of affinity-propagation-clusters was completed!" << std::endl; }


    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // use centroids and clusters to create the output-kmeans-image
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    arma::uvec idx_NA = NAs_matrix(getcent);
    if (!idx_NA.empty()) {
      // arma::uvec all_idx = arma::regspace<arma::uvec>(0, 1, getcent.n_rows-1);                            // create vector of indices
      // arma::uvec keep_dat = afpr.matlab_setdiff(all_idx, idx_NA);                                         // find which indices do not have NA's
      // getcent = getcent.rows(keep_dat);                                                                   // overwrite initial centroids by keeping those who do not include NA's. NOT A GOOD APPROACH because getcent.n_rows != getclust.max() and this raises an error in "new_im.slice(0)(f) = getcent(getclust(f), 0);"
      getcent.rows(idx_NA).fill(0);                                                                          // better fill the centroid-rows including NA's with 0's rather than overwritting the initial centroids matrix (see comment previous line)
      std::string tmp_str = std::to_string(idx_NA.n_elem);
      Rcpp::Rcout << "WARNING: " + tmp_str + " row(s) of the output centroids include missing values (NA's)!. The missing values will be replaced / filled with 0's! To overcome this warning an option would be to experiment with a different 'kmeans_initializer'!" << std::endl;
    }

    if (adjust_centroids_and_return_masks) {                     // here I solely adjust the centroids
      if (verbose) { Rcpp::Rcout << "NOTE: The 'KMeans_image_data' will be returned in black & white colour due to the fact that the 'adjust_centroids_and_return_masks' parameter was set to TRUE!" << std::endl; }
      int true_clusters = getcent.n_rows;                        // it is possible that there are NA's in the centroids
      getcent.set_size(true_clusters, 3);                        // n_cols = 3 because I have an RGB image
      for (int i = 0; i < true_clusters; i++) {
        arma::rowvec tmp_row(3);
        tmp_row.fill(i);
        getcent.row(i) = tmp_row;                                // in case that 'adjust_centroids_and_return_masks' is set to TRUE the KMeans-centroids will be overwritten. Better quality of the 'KMeans_image_data' output can be obtained if 'adjust_centroids_and_return_masks' is set to FALSE.
      }
    }

    new_im.set_size(input_image.n_rows, input_image.n_cols, input_image.n_slices);        // see for a similar way to create the cluster-masks in R : https://github.com/mlampros/ClusterR/issues/14#issuecomment-457692420
    new_im.fill(0);
    for (unsigned int f = 0; f < getclust.n_elem; f++) {
      new_im.slice(0)(f) = getcent(getclust(f), 0);                               // each observation is associated with the nearby centroid
      new_im.slice(1)(f) = getcent(getclust(f), 1);
      new_im.slice(2)(f) = getcent(getclust(f), 2);
    }

    if (verbose) { Rcpp::Rcout << "Pre-processing of the " + which_km + "-output-image is completed!" << std::endl; }


    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // display segments of the 'new_im' image
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    masks_lst.set_size(affinty_num_clusters, 1);

    if (adjust_centroids_and_return_masks) {                                      // adjust the centroids and return the masks
      for (int IM = 0; IM < affinty_num_clusters; IM++) {
        arma::Cube<int> mask_3d(new_im.n_rows, new_im.n_cols, 3);
        for (int f = 0; f < 3; f++) {
          arma::Mat<int> mask1(new_im.n_rows, new_im.n_cols, arma::fill::zeros);
          arma::uvec tmp_uvec = arma::find(new_im.slice(f) == IM);
          mask1(tmp_uvec).fill(1);
          mask_3d.slice(f) = mask1;
        }

        arma::cube tmp_show;

        if (colour_type == "RGB") {
          tmp_show = input_image % mask_3d;
        }
        else if (colour_type == "LAB") {
          tmp_show = lab_obj % mask_3d;
        }
        else if (colour_type == "HSV") {
          tmp_show = hsv_tmp % mask_3d;
        }
        else {
          Rcpp::stop("Invalid 'colour_type' parameter!");
        }
        masks_lst(IM,0) = tmp_show;
      }
      if (verbose) { Rcpp::Rcout << "The centroids were adjusted and the image-masks will be returned!" << std::endl; }
    }
  }

  Rcpp::List tmp_lst = Rcpp::List::create(Rcpp::Named("KMeans_image_data") = new_im,
                                          Rcpp::Named("KMeans_clusters") = getclust,
                                          Rcpp::Named("masks") = masks_lst,
                                          Rcpp::Named("centr") = getcent,
                                          Rcpp::Named("AP_image_data") = ap_new_im);

  if (return_labels_2_dimensionsional) {
    tmp_lst["spix_labels"] = labels_obj;
    tmp_lst["AP_clusters"] = clusters;
  }

  return tmp_lst;
}

