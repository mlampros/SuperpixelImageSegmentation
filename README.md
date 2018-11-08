[![Travis-CI Build Status](https://travis-ci.org/mlampros/SuperpixelImageSegmentation.svg?branch=master)](https://travis-ci.org/mlampros/SuperpixelImageSegmentation)
[![codecov.io](https://codecov.io/github/mlampros/SuperpixelImageSegmentation/coverage.svg?branch=master)](https://codecov.io/github/mlampros/SuperpixelImageSegmentation?branch=master)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/mlampros/SuperpixelImageSegmentation?branch=master&svg=true)](https://ci.appveyor.com/project/mlampros/SuperpixelImageSegmentation/branch/master)


## SuperpixelImageSegmentation
<br>

The R / Rcpp code of the *SuperpixelImageSegmentation* package is based primarily on the article ["Image Segmentation using SLIC Superpixels and Affinity Propagation Clustering", Bao Zhou, International Journal of Science and Research (IJSR), 2013](https://pdfs.semanticscholar.org/6533/654973054b742e725fd433265700c07b48a2.pdf).

I wrote a [blog post]() explaining how to take advantage of the R / Rcpp code of the *SuperpixelImageSegmentation* package.

<br>

System / Software Requirements:

* [OpenImageR ](https://github.com/mlampros/OpenImageR)
* [ClusterR ](https://github.com/mlampros/ClusterR)
* a C++ 11 compiler
<br><br>

To install the package from Github you can **either** use the *install_github* function of the devtools package,
<br><br>

```R

devtools::install_github('mlampros/SuperpixelImageSegmentation')


```
<br>

**or** directly download the .zip file using the **Clone or download** button in the [repository page](https://github.com/mlampros/SuperpixelImageSegmentation), extract it locally (rename it to *SuperpixelImageSegmentation* if necessary) and run,

<br>

```R

setwd('/your_folder/SuperpixelImageSegmentation/')
Rcpp::compileAttributes(verbose = TRUE)
setwd('/your_folder/')
system("R CMD build SuperpixelImageSegmentation")
system("R CMD INSTALL SuperpixelImageSegmentation_1.0.0.tar.gz")

```


<br>

Use the following link to report bugs/issues,
<br><br>

[https://github.com/mlampros/SuperpixelImageSegmentation/issues](https://github.com/mlampros/SuperpixelImageSegmentation/issues)

