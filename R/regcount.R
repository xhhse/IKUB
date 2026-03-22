#' @title Region counts
#' @description Calculates counts of all regions.
#' @details Inputs a NIfTI-file and returns total counts and volumes for all
#' regions.
#' @param filename One NIfTI-file
#' @export
#' @return Total counts and volumes for all region in a supplied nifty-file.

regcount <- function(filename) {
  nifti <- neurobase::readnii(filename)
  volpervoxel <- prod(nifti@pixdim[2:4])
  output <- tibble::as_tibble(table(nifti))
  output$Label_No <- as.integer(output$nifti)
  output <- output %>%
    dplyr::mutate(vol_mm3 = round(n * volpervoxel, 3), filename = filename) %>%
    dplyr::select(filename = filename, Label_No, counts = n, vol_mm3)
  return(output)
}
