
#--------------------------------------------------------------------------------------------------

#-----------
# image data
#-----------

path = system.file("images", "BSR_bsds500_image.jpg", package = "SuperpixelImageSegmentation")
im = OpenImageR::readImage(path)

#--------------------------------------------------------------------------------------------------


context('superpixel image segmentation')



testthat::test_that("the 'spixel_segmentation' function returns an error if the 'kmeans_method' parameter is an empty string and the 'AP_data' parameter is FALSE", {

  init = Image_Segmentation$new()

  testthat::expect_error( init$spixel_segmentation(input_image = im, method = "slic", superpixel = 200, kmeans_method = "", AP_data = FALSE, use_median = TRUE,
                                                  minib_kmeans_batch = 10, minib_kmeans_init_fraction = 0.5, kmeans_num_init = 3, kmeans_max_iters = 100,
                                                  kmeans_initializer = "kmeans++", colour_type = "RGB", compactness_factor = 20,
                                                  adjust_centroids_and_return_masks = FALSE, sim_normalize = FALSE, sim_wL = 3, sim_wA = 10, sim_wB = 10,
                                                  sim_color_radius = 20, verbose = FALSE) )
})


testthat::test_that("the 'spixel_segmentation' function returns the expected output if AP_data is TRUE", {

  init = Image_Segmentation$new()

  dat = init$spixel_segmentation(input_image = im, method = "slic", superpixel = 200, kmeans_method = "", AP_data = T, use_median = TRUE,
                                 minib_kmeans_batch = 10, minib_kmeans_init_fraction = 0.5, kmeans_num_init = 3, kmeans_max_iters = 100,
                                 kmeans_initializer = "kmeans++", colour_type = "RGB", compactness_factor = 20,
                                 adjust_centroids_and_return_masks = FALSE, sim_normalize = FALSE, sim_wL = 3, sim_wA = 10, sim_wB = 10,
                                 sim_color_radius = 20, verbose = FALSE)

  testthat::expect_true( length(dat$KMeans_image_data) == 0 && dim(dat$AP_image_data)[3] == 3 )
})



testthat::test_that("the 'spixel_segmentation' function returns the expected output if AP_data is TRUE and kmeans_method == 'kmeans'", {

  init = Image_Segmentation$new()

  dat = init$spixel_segmentation(input_image = im, method = "slic", superpixel = 200, kmeans_method = "kmeans", AP_data = T, use_median = TRUE,
                                 minib_kmeans_batch = 10, minib_kmeans_init_fraction = 0.5, kmeans_num_init = 3, kmeans_max_iters = 100,
                                 kmeans_initializer = "kmeans++", colour_type = "RGB", compactness_factor = 20,
                                 adjust_centroids_and_return_masks = FALSE, sim_normalize = FALSE, sim_wL = 3, sim_wA = 10, sim_wB = 10,
                                 sim_color_radius = 20, verbose = FALSE)

  testthat::expect_true( dim(dat$KMeans_image_data)[3] == 3 && dim(dat$AP_image_data)[3] == 3 )
})
