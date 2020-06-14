

#' Segmentation of images based on Superpixels, Affinity propagation and Kmeans clustering
#'
#'
#' @param input_image a 3-dimensional input image (the range of the pixel values should be preferably in the range 0 to 255)
#' @param method a character string specifying the superpixel method. It can be either "slic" or "slico"
#' @param superpixel a numeric value specifying the number of superpixels
#' @param kmeans_method a character string specifying the kmeans method. If not empty ("") then it can be either "kmeans" or "mini_batch_kmeans"
#' @param AP_data a boolean. If TRUE then the affinity propagation image data will be computed and returned
#' @param use_median a boolean. If TRUE then the median will be used rather than the mean value for the inner computations
#' @param minib_kmeans_batch the size of the mini batches
#' @param kmeans_num_init number of times the algorithm will be run with different centroid seeds
#' @param kmeans_max_iters the maximum number of clustering iterations
#' @param minib_kmeans_init_fraction percentage of data to use for the initialization centroids (applies if initializer is \emph{kmeans++} or \emph{optimal_init}). Should be a float number between 0.0 and 1.0.
#' @param kmeans_initializer the method of initialization. One of, \emph{optimal_init}, \emph{quantile_init}, \emph{kmeans++} and \emph{random}. See details for more information
#' @param colour_type a character string specifying the colour type. It can be one of "RGB", "LAB" or "HSV"
#' @param compactness_factor a numeric value specifying the compactness parameter in case that \emph{method} is "slic"
#' @param adjust_centroids_and_return_masks a boolean. If TRUE and the \emph{kmeans_method} parameter is NOT empty ("") then the centroids will be adjusted and image-masks will be returned. This will allow me to plot the masks using the \emph{spixel_masks_show} method.
#' @param return_labels_2_dimensionsional a boolean. If TRUE then a matrix of labels based on the output superpixels in combination with the Affinity Propagation clusters will be returned
#' @param sim_normalize a boolean. If TRUE then the constructed similarity matrix will be normalised to have unit p-norm (see the armadillo documentation for more details)
#' @param sim_wL a numeric value specifying the weight for the \emph{"L"} channel of the image (see the details section for more information)
#' @param sim_wA a numeric value specifying the weight for the \emph{"A"} channel of the image (see the details section for more information)
#' @param sim_wB a numeric value specifying the weight for the \emph{"B"} channel of the image (see the details section for more information)
#' @param sim_color_radius a numeric value specifying the \emph{colorradius} (see the details section for more information)
#' @param delay_display_seconds a numeric value specifying the seconds to delay the display of the next image (It displays the images consecutively). This parameter applies only if the \emph{display_all} is set to FALSE (spixel_masks_show method)
#' @param display_all a boolean. If TRUE then all images will be displayed in a grid  (spixel_masks_show method)
#' @param margin_btw_plots a float number specifying the margins between the plots if the \emph{display_all} parameter is set to TRUE  (spixel_masks_show method)
#' @param verbose a boolean. If TRUE then information will be printed in the console  (spixel_masks_show method)
#' @param spix_labels a matrix. I can retrieve the "spix_labels" parameter by setting the "return_labels_2_dimensionsional" parameter to TRUE in the "spixel_segmentation" method  (spixel_clusters_show method)
#' @param color_palette one of the color palettes. Use  ?grDevices::topo.colors  to see the available color palettes
#' @param parameter_list_png either NULL or a list of parameters passed to the  ?grDevices::png  function, such as  list(filename = 'img.png', width = 100, height = 100, units = "px", pointsize = 12, bg = "white", type = "quartz")
#'
#' @export
#' @references
#'
#' https://pdfs.semanticscholar.org/6533/654973054b742e725fd433265700c07b48a2.pdf , "Image Segmentation using SLIC Superpixels and Affinity Propagation Clustering", Bao Zhou, 2013, International Journal of Science and Research (IJSR)
#' @details
#'
#' \emph{sim_wL}, \emph{sim_wA}, \emph{sim_wB} are the weights of the three channels. They keep balance so as to be consistent with human perception.
#'
#' The quantity \emph{colorradius} adjusts the number of clusters, and if its value is low, the number of targets would increase, which leads to more detailed segmentation results.
#'
#' If the \emph{adjust_centroids_and_return_masks} parameter is set to FALSE then the output \emph{kmeans_image_data} will be an RGB image, otherwise it will be a black-and-white image.
#'
#' \emph{colour_type} parameter: RGB (Red-Green-Blue), LAB (Lightness, A-colour-dimension, B-colour-dimension) or HSV (Hue, Saturation, Value) colour.
#'
#' Higher resolution images give better results.
#'
#' The \emph{affinity propagation} algorithm is used here with default parameter values.
#'
#' By setting the \emph{sim_normalize} parameter to TRUE, the affinity propagation algorithm requires less iterations to complete. However, the \emph{colorradius} parameter does not have an
#' effect if the similarity matrix is normalized.
#'
#'
#' ---------------kmeans initializers----------------------
#'
#' \strong{optimal_init}   : this initializer adds rows of the data incrementally, while checking that they do not already exist in the centroid-matrix
#'
#' \strong{quantile_init}  : initialization of centroids by using the cummulative distance between observations and by removing potential duplicates
#'
#' \strong{kmeans++}       : kmeans++ initialization. Reference : http://theory.stanford.edu/~sergei/papers/kMeansPP-soda.pdf AND http://stackoverflow.com/questions/5466323/how-exactly-does-k-means-work
#'
#' \strong{random}         : random selection of data rows as initial centroids
#'
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom OpenImageR NormalizeObject rotateFixed imageShow readImage
#' @importFrom grDevices png rainbow dev.off
#' @importFrom lattice levelplot
#'
#' @section Methods:
#'
#' \describe{
#'  \item{\code{Image_Segmentation$new()}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{spixel_segmentation()}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{spixel_masks_show()}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{spixel_clusters_show()}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  }
#' @usage # init <- Image_Segmentation$new()
#' @examples
#'
#' library(SuperpixelImageSegmentation)
#'
#' path = system.file("images", "BSR_bsds500_image.jpg", package = "SuperpixelImageSegmentation")
#'
#' im = OpenImageR::readImage(path)
#'
#' init = Image_Segmentation$new()
#'
#' num_spix = 10           # for illustration purposes
#' # num_spix = 600        # recommended number of superpixels
#'
#' spx = init$spixel_segmentation(input_image = im,
#'                                superpixel = num_spix,
#'                                AP_data = TRUE,
#'                                use_median = TRUE,
#'                                return_labels_2_dimensionsional = TRUE,
#'                                sim_color_radius = 10)
#'
#'
#' #...........................
#' # plot the superpixel labels
#' #...........................
#'
#' plt = init$spixel_clusters_show(spix_labels = spx$spix_labels,
#'                                 color_palette = grDevices::rainbow,
#'                                 parameter_list_png = NULL)
#'
#' # plt
#'
#'
#' #....................................................
#' # create a binary image for a specified cluster label
#' #....................................................
#'
#' pix_values = spx$spix_labels
#'
#' target_cluster = 3                               # determine clusters visually ('plt' variable)
#'
#' pix_values[pix_values != target_cluster] = 0     # set all other values to 0 (background)
#' pix_values[pix_values == target_cluster] = 1     # set the target_cluster to 1 (binary image)
#'
#' # OpenImageR::imageShow(pix_values)
#'

Image_Segmentation <- R6::R6Class("Image_Segmentation",

                                  lock_objects = FALSE,

                                  public = list(

                                    initialize = function() {

                                    },


                                    #.................................................................................
                                    # image segmentation using superpixels, Affinity propagation and Kmeans clustering
                                    #.................................................................................

                                    spixel_segmentation = function(input_image,
                                                                   method = "slic",
                                                                   superpixel = 200,
                                                                   kmeans_method = "",
                                                                   AP_data = FALSE,
                                                                   use_median = TRUE,
                                                                   minib_kmeans_batch = 10,
                                                                   minib_kmeans_init_fraction = 0.5,
                                                                   kmeans_num_init = 3,
                                                                   kmeans_max_iters = 100,
                                                                   kmeans_initializer = "kmeans++",
                                                                   colour_type = "RGB",
                                                                   compactness_factor = 20,
                                                                   adjust_centroids_and_return_masks = FALSE,
                                                                   return_labels_2_dimensionsional = FALSE,
                                                                   sim_normalize = FALSE,
                                                                   sim_wL = 3,
                                                                   sim_wA = 10,
                                                                   sim_wB = 10,
                                                                   sim_color_radius = 20,
                                                                   verbose = FALSE) {

                                      if (!kmeans_initializer %in% c('kmeans++', 'random', 'optimal_init', 'quantile_init')) stop("available initializer methods are 'kmeans++', 'random', 'optimal_init' and 'quantile_init'", call. = F)

                                      if (verbose) {
                                        t_start = proc.time()
                                      }
                                      if (adjust_centroids_and_return_masks) {
                                        private$masks_flag = T
                                      }
                                      private$lst_obj = image_segmentation(input_image = input_image,
                                                                           method = method,
                                                                           num_superpixel = superpixel,
                                                                           kmeans_method = kmeans_method,
                                                                           AP_data = AP_data,
                                                                           use_median = use_median,
                                                                           minib_kmeans_batch = minib_kmeans_batch,
                                                                           minib_kmeans_init_fraction = minib_kmeans_init_fraction,
                                                                           kmeans_num_init = kmeans_num_init,
                                                                           kmeans_max_iters = kmeans_max_iters,
                                                                           kmeans_initializer = kmeans_initializer,
                                                                           colour_type = colour_type,
                                                                           compactness_factor = compactness_factor,
                                                                           adjust_centroids_and_return_masks = adjust_centroids_and_return_masks,
                                                                           return_labels_2_dimensionsional = return_labels_2_dimensionsional,
                                                                           sim_normalize = sim_normalize,
                                                                           sim_wL = sim_wL,
                                                                           sim_wA = sim_wA,
                                                                           sim_wB = sim_wB,
                                                                           sim_color_radius = sim_color_radius,
                                                                           verbose = verbose)

                                      if (return_labels_2_dimensionsional) {
                                        lbs_out = matrix(0, nrow = nrow(private$lst_obj$spix_labels), ncol = ncol(private$lst_obj$spix_labels))    # initialize a new matrix because the pixel values of the 'spix_labels' matrix might overlap with the cluster numbers (iterations: 'i')
                                        for (i in 1:length(private$lst_obj$AP_clusters)) {
                                          idx = which(private$lst_obj$spix_labels %in% private$lst_obj$AP_clusters[[i]])                           # more efficient than using c++
                                          lbs_out[idx] = i
                                        }

                                        private$lst_obj$spix_labels = NULL                                                                         # remove the 'spix_labels' object
                                        private$lst_obj$AP_clusters = NULL                                                                         # remove the 'AP-clusters' object
                                        private$lst_obj$spix_labels = lbs_out                                                                      # assign the initialized and populated matrix to the output list
                                      }

                                      if (verbose) {
                                        t_end = proc.time()
                                        time_total = as.numeric((t_end - t_start)['elapsed'])
                                        time_ = private$elapsed_time(time_total)
                                        cat(time_, "\n")
                                      }

                                      return(private$lst_obj)
                                    },


                                    #.................................................................
                                    # plot the image segmentation masks (based on the output clusters)
                                    #.................................................................

                                    spixel_masks_show = function(delay_display_seconds = 3,
                                                                 display_all = FALSE,
                                                                 margin_btw_plots = 0.15,
                                                                 verbose = FALSE) {

                                      if (is.null(private$lst_obj)) { stop("First run the 'spixel_segmentation' method with the 'adjust_centroids_and_return_masks' parameter set to TRUE!", call. = F) }
                                      if (!private$masks_flag) { stop("The 'adjust_centroids_and_return_masks' parameter of the 'spixel_segmentation' method should be set to TRUE!", call. = F) }

                                      if (display_all) {

                                        grid_r_c = private$calc_grid_rows_cols(length(private$lst_obj$masks))
                                        par(mfrow = c(grid_r_c$rows, grid_r_c$cols), mar = c(margin_btw_plots, margin_btw_plots, margin_btw_plots, margin_btw_plots))

                                        for (plt in 1:length(private$lst_obj$masks)) {
                                          tmp_im = OpenImageR::NormalizeObject(private$lst_obj$masks[[plt]])
                                          has_nan = unlist(lapply(1:dim(tmp_im)[3], function(x) is_mt_finite(tmp_im[,,x])))
                                          if (sum(has_nan) != 3) {
                                            cat("WARNING: The image-mask", plt, "includes non-finite data and won't be plotted!", "\n")
                                          }
                                          else {
                                            tmp_im = grDevices::as.raster(tmp_im)
                                            graphics::plot(tmp_im)
                                          }
                                        }
                                      }
                                      else {
                                        for (i in 1:length(private$lst_obj$masks)) {
                                          has_nan = unlist(lapply(1:dim(private$lst_obj$masks[[i]])[3], function(x) is_mt_finite(private$lst_obj$masks[[i]][,,x])))
                                          if (verbose) { cat("Image:", i, "\n") }
                                          if (sum(as.vector(private$lst_obj$masks[[i]])) == 0) {
                                            cat("WARNING: The image mask", i, "is an all-0's-image!", "\n")
                                          }
                                          else if (sum(has_nan) != 3) {
                                            cat("WARNING: The image-mask", i, "includes non-finite data and won't be plotted!", "\n")
                                          }
                                          else {
                                            OpenImageR::imageShow(private$lst_obj$masks[[i]])
                                            Sys.sleep(delay_display_seconds)
                                          }
                                        }
                                      }
                                    },


                                    #......................................................................................................
                                    # plot 2-dimensional superpixel clusters along with a legend so that I'll be in place to distinguish
                                    # between the cluster labels
                                    #
                                    # I can retrieve the "spix_labels" parameter by setting the "return_labels_2_dimensionsional" parameter
                                    # to TRUE in the "spixel_segmentation" method of this R6 class
                                    #......................................................................................................

                                    spixel_clusters_show = function(spix_labels,
                                                                    color_palette = grDevices::rainbow,
                                                                    parameter_list_png = NULL) {

                                      if (!inherits(spix_labels, 'matrix')) stop("The 'spix_labels' parameter must be of type matrix (set the 'return_labels_2_dimensionsional' parameter to TRUE in the 'spixel_segmentation' method)!", call. = F)

                                      LEN_UNQ = length(unique(as.vector(spix_labels)))
                                      spix_labels = OpenImageR::rotateFixed(spix_labels, angle = 90)                          # rotate the output labels to visualize the image

                                      if (!is.null(parameter_list_png)) {
                                        do.call(grDevices::png, parameter_list_png)
                                      }

                                      lat_plt = lattice::levelplot(spix_labels,
                                                                   xlab = NULL,
                                                                   ylab = NULL,
                                                                   cuts = LEN_UNQ - 1,
                                                                   col.regions = do.call(color_palette, list(n = LEN_UNQ)),   # use a color-palette with default parameters
                                                                   useRaster = TRUE)                                          # for the 'lattice::levelplot()' see: https://stackoverflow.com/a/15188726/8302386   +  using 'useRaster' the plot is returned faster

                                      if (!is.null(parameter_list_png)) {
                                        print(lat_plt)
                                        grDevices::dev.off()
                                      }

                                      return(lat_plt)
                                    }

                                  ),

                                  private = list(

                                    lst_obj = NULL,
                                    masks_flag = F,

                                    #.....................................
                                    # divisors calculation
                                    # https://stackoverflow.com/a/19465753
                                    #.....................................

                                    divisors = function(x){
                                      y <- seq_len(x)
                                      y[ x%%y == 0 ]
                                    },


                                    #.................................................................................
                                    # calculate the grid-rows and grid-cols (for specific number of sub-matrices < 53)
                                    #.................................................................................

                                    calc_grid_rows_cols = function(num_masks) {

                                      if (length(num_masks) > 52) stop("Plotting of grid of images using the 'calc_grid_rows_cols' function is restricted to 52 objects!", call. = F)

                                      rows = sqrt(num_masks)
                                      cols = 0
                                      DIVS = private$divisors(num_masks)
                                      LEN = length(DIVS)
                                      if (LEN > 2 && (num_masks %% rows) != 0) {
                                        rows = DIVS[LEN/2]
                                        cols = DIVS[(LEN/2)+1]
                                      }
                                      else {
                                        if ((num_masks %% rows) == 0) {                    # square int's
                                          cols = rows
                                        }
                                        else {
                                          if ((num_masks %% rows) == 0) {
                                            rows = round(rows)
                                            cols = num_masks / rows
                                          }
                                          else {
                                            rows = floor(rows)
                                            cols = ceiling(num_masks / rows)
                                          }
                                        }
                                        if (((rows * cols) - num_masks) > 1) {             # neither square nor divisors [ primes ]
                                          DIF = ((rows * cols) - num_masks)
                                          rows = rows - DIF + 1
                                          if ((num_masks %% rows) == 0) {
                                            cols = rows
                                          }
                                          else {
                                            if ((num_masks %% rows) == 0) {
                                              rows = round(rows)
                                              cols = num_masks / rows
                                            }
                                            else {
                                              rows = floor(rows)
                                              cols = ceiling(num_masks / rows)
                                            }
                                          }
                                        }
                                      }
                                      return(list(rows = rows, cols = cols))
                                    },


                                    #..........................................
                                    # elapsed time in hours & minutes & seconds
                                    #..........................................

                                    elapsed_time = function(secs) {
                                      tmp_hours = as.integer((secs / 60) / 60)
                                      tmp_hours_minutes = (secs / 60) %% 60
                                      tmp_seconds = secs %% 60
                                      res_out = paste(c("Elapsed time: ", tmp_hours, " hours and ", as.integer(tmp_hours_minutes), " minutes and ", as.integer(tmp_seconds), " seconds."), collapse = "")
                                      return(res_out)
                                    }
                                  )
)
