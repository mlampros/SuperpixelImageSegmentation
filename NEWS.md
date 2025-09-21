
## SuperpixelImageSegmentation 1.0.6

* I updated the `Makevars` and `Makevars.win` files by adding `-DARMA_USE_CURRENT` (see issue: https://github.com/RcppCore/RcppArmadillo/issues/476)
* I removed the `-mthreads` compilation option from the "Makevars.win" file
* I removed the "CXX_STD = CXX11" from the "Makevars" files, and the "[[Rcpp::plugins(cpp11)]]" from the "superPx_AP_Kmeans_Segment.cpp" file due to the following NOTE from CRAN, "NOTE Specified C++11: please drop specification unless essential" (see also: https://www.tidyverse.org/blog/2023/03/cran-checks-compiled-code/#note-regarding-systemrequirements-c11)


## SuperpixelImageSegmentation 1.0.5

* I updated the documentation


## SuperpixelImageSegmentation 1.0.4

* I've added the *CITATION* file in the *inst* directory


## SuperpixelImageSegmentation 1.0.3

* I've included the Affinity Propagation Clustering parameters in the *spixel_segmentation* method of the *Image_Segmentation* R6 class and especially the *ap_maxits* (maximum number of iterations), *ap_convits* (convits iterations), *ap_dampfact* (update equation damping level) and *ap_nonoise* (small amount of noise to prevent degenerate cases). Although the default parameter values work for the majority of Image Segmentation tasks, adjustments might be necessary for specific use cases.


## SuperpixelImageSegmentation 1.0.2

* I've added the *return_labels_2_dimensionsional* parameter to the *spixel_segmentation* R6 Method, so that if TRUE then the 2-dimensional (matrix) superpixel labels will be returned
* I've added the *spixel_clusters_show* method to visualize the superpixel clusters in case the *return_labels_2_dimensionsional* parameter is set to TRUE


## SuperpixelImageSegmentation 1.0.1

* I've added an error case if the *kmeans_initializer* parameter is not one of 'kmeans++', 'random', 'optimal_init' or 'quantile_init'


## SuperpixelImageSegmentation 1.0.0

* The *spixel_segmentation* method allows also the user to return the kmeans clusters too (in case that the *kmeans_method* is set to "kmeans" or "mini_batch_kmeans")
