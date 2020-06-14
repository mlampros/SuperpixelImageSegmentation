#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _SuperpixelImageSegmentation_apply_rcpp(SEXP);
extern SEXP _SuperpixelImageSegmentation_image_segmentation(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _SuperpixelImageSegmentation_is_mt_finite(SEXP);
extern SEXP _SuperpixelImageSegmentation_NAs_matrix(SEXP);
extern SEXP _SuperpixelImageSegmentation_simil_A(SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_SuperpixelImageSegmentation_apply_rcpp",         (DL_FUNC) &_SuperpixelImageSegmentation_apply_rcpp,          1},
    {"_SuperpixelImageSegmentation_image_segmentation", (DL_FUNC) &_SuperpixelImageSegmentation_image_segmentation, 21},
    {"_SuperpixelImageSegmentation_is_mt_finite",       (DL_FUNC) &_SuperpixelImageSegmentation_is_mt_finite,        1},
    {"_SuperpixelImageSegmentation_NAs_matrix",         (DL_FUNC) &_SuperpixelImageSegmentation_NAs_matrix,          1},
    {"_SuperpixelImageSegmentation_simil_A",            (DL_FUNC) &_SuperpixelImageSegmentation_simil_A,             5},
    {NULL, NULL, 0}
};

void R_init_SuperpixelImageSegmentation(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
