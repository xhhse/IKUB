#' @title Total counts for filename
#' @description Calculates total number of counts in both images and counts for
#' overlap, counts only in image1, counts only in image2 for given filename.
#' @details Function is used to calculate counts for calculating Dice
#' coefficients. This function is primarily used by other functions.
#' @param filename1 A filename for one NIfTI-file
#' @param filename2 A filename for one NIfTI-file
#' @return Total number of counts in both images and counts for overlap, counts
#' only in image1, counts only in image2 for the given file pair.
#' @export

files_element <- function(filename1, filename2) {
  img1 <- neurobase::readnii(filename1)
  img2 <- neurobase::readnii(filename2)
  chk_dim <- dim(img1) - dim(img2)
  # check dimensions
  if (chk_dim[1] != 0 | chk_dim[2] != 0 | chk_dim[3] != 0) {
    stop("Files do not have the same diemensions.")
  } else {
    slice_img1 <- plyr::alply(img1, 3)
    slice_img2 <- plyr::alply(img2, 3)
    slice_no <- as.list(1:dim(img1)[3])
    paras <- list(slice_no, slice_img1, slice_img2)
    result <- purrr::pmap_dfr(paras, slice_label)
    result$datalabel <- paste(gsub(pattern = ".nii.gz", "", x = filename1), gsub(pattern = ".nii.gz", "", x = filename2), sep = "&")
    return(result)
  }
}
