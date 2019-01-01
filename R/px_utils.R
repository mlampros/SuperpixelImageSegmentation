

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
#' @param sim_normalize a boolean. If TRUE then the constructed similarity matrix will be normalised to have unit p-norm (see the armadillo documentation for more details)
#' @param sim_wL a numeric value specifying the weight for the \emph{"L"} channel of the image (see the details section for more information)
#' @param sim_wA a numeric value specifying the weight for the \emph{"A"} channel of the image (see the details section for more information)
#' @param sim_wB a numeric value specifying the weight for the \emph{"B"} channel of the image (see the details section for more information)
#' @param sim_color_radius a numeric value specifying the \emph{colorradius} (see the details section for more information)
#' @param delay_display_seconds a numeric value specifying the seconds to delay the display of the next image (It displays the images consecutively). This parameter applies only if the \emph{display_all} is set to FALSE (spixel_masks_show method)
#' @param display_all a boolean. If TRUE then all images will be displayed in a grid  (spixel_masks_show method)
#' @param margin_btw_plots a float number specifying the margins between the plots if the \emph{display_all} parameter is set to TRUE  (spixel_masks_show method)
#' @param verbose a boolean. If TRUE then information will be printed in the console  (spixel_masks_show method)
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
#' @importFrom OpenImageR NormalizeObject
#' @importFrom OpenImageR imageShow
#' @importFrom OpenImageR readImage
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
#'                                sim_color_radius = 10)
#'
#' \dontrun{
#' plt_im = init$spixel_masks_show(display_all = TRUE)
#' }

Image_Segmentation <- R6::R6Class("Image_Segmentation",

                                  lock_objects = FALSE,

                                  public = list(

                                    initialize = function() {

                                    },


                                    #--------------------------------------------------------------------------------------------------
                                    # image segmentation using superpixels, Affinity propagation and Kmeans clustering
                                    #--------------------------------------------------------------------------------------------------

                                    spixel_segmentation = function(input_image, method = "slic", superpixel = 200, kmeans_method = "", AP_data = FALSE, use_median = TRUE,
                                                                   minib_kmeans_batch = 10, minib_kmeans_init_fraction = 0.5, kmeans_num_init = 3, kmeans_max_iters = 100,
                                                                   kmeans_initializer = "kmeans++", colour_type = "RGB", compactness_factor = 20,
                                                                   adjust_centroids_and_return_masks = FALSE, sim_normalize = FALSE, sim_wL = 3, sim_wA = 10, sim_wB = 10,
                                                                   sim_color_radius = 20, verbose = FALSE) {

                                      if (verbose) {
                                        t_start = proc.time()
                                      }
                                      if (adjust_centroids_and_return_masks) {
                                        private$masks_flag = T
                                      }
                                      private$lst_obj = image_segmentation(input_image, method, superpixel, kmeans_method, AP_data, use_median, minib_kmeans_batch,
                                                                           minib_kmeans_init_fraction, kmeans_num_init, kmeans_max_iters, kmeans_initializer,
                                                                           colour_type, compactness_factor, adjust_centroids_and_return_masks, sim_normalize,
                                                                           sim_wL, sim_wA, sim_wB, sim_color_radius, verbose)
                                      if (verbose) {
                                        t_end = proc.time()
                                        time_total = as.numeric((t_end - t_start)['elapsed'])
                                        time_ = private$elapsed_time(time_total)
                                        cat(time_, "\n")
                                      }
                                      return(private$lst_obj)
                                    },


                                    #-----------------------------------------------------------------
                                    # plot the image segmentation masks (based on the output clusters)
                                    #-----------------------------------------------------------------

                                    spixel_masks_show = function(delay_display_seconds = 3, display_all = FALSE,
                                                                 margin_btw_plots = 0.15, verbose = FALSE) {

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
                                    }

                                  ),

                                  private = list(

                                    lst_obj = NULL,
                                    masks_flag = F,

                                    #-------------------------------------
                                    # divisors calculation
                                    # https://stackoverflow.com/a/19465753
                                    #-------------------------------------

                                    divisors = function(x){
                                      y <- seq_len(x)
                                      y[ x%%y == 0 ]
                                    },


                                    #---------------------------------------------------------------------------------
                                    # calculate the grid-rows and grid-cols (for specific number of sub-matrices < 53)
                                    #---------------------------------------------------------------------------------

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
                                    
                                    
                                    #------------------------------------------
                                    # elapsed time in hours & minutes & seconds
                                    #------------------------------------------
                                    
                                    elapsed_time = function(secs) {
                                      tmp_hours = as.integer((secs / 60) / 60)
                                      tmp_hours_minutes = (secs / 60) %% 60
                                      tmp_seconds = secs %% 60
                                      res_out = paste(c("Elapsed time: ", tmp_hours, " hours and ", as.integer(tmp_hours_minutes), " minutes and ", as.integer(tmp_seconds), " seconds."), collapse = "")
                                      return(res_out)
                                    }
                                  )
)
