#' @title Counts for elements
#' @description Calculates total number of counts in both images and counts for
#' overlap, counts only in slice_image1, counts only in slice_image2.
#' @details Inputs one slice number and one label number. Function calculates
#' counts for total number of pixels in both images, pixels for overlap in the
#' given slice for the given label number, pixels only exist in slice_image1 and
#' pixels only exist in slice_image2. This function is primarily used by other
#' functions.
#' @param slice_no slice_no with integer format
#' @param targetlabel Label_No with integer format
#' @param slice_img1 Slice image for filename1
#' @param slice_img2 Slice image for filename2
#' @return Total counts for all label numbers and slice numbers in both images,
#' overlap, only in slice_image1 and only in slice_image2.
#' @export

element_count<- function(slice_no, targetlabel, slice_img1, slice_img2) {
  slice_img1[which(slice_img1 != targetlabel)] <- 0
  slice_img2[which(slice_img2 != targetlabel)] <- 0
  if (sum(slice_img1 + slice_img2) > 0) {
    slice_tot <- (slice_img1 + 2 * slice_img2) / targetlabel
    slice_output <- tibble::tibble(slice_No = slice_no, Label_No = targetlabel, overlap_counts = sum(slice_tot == 3), only_file1 = sum(slice_tot == 1), only_file2 = sum(slice_tot == 2), file1_counts = sum(slice_img1 == targetlabel), file2_counts = sum(slice_img2 == targetlabel))
    return(slice_output)
  }
}
