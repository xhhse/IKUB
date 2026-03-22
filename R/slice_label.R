#' @title Extracts label numbers
#' @description Extracts label numbers using slice.
#' @details Inputs one slice number. Returns a tibble. This function is
#' primarily used by other functions.
#' @param slice_no One slice number with format integer
#' @param slice_img1 Slice image for filename1
#' @param slice_img2 Slice image for filename2
#' @return Total counts for all label numbers and slice numbers in both images,
#' overlap, only in image1 and only in image2.
#' @export

slice_label<- function(slice_no, slice_img1, slice_img2) {
  if (sum(slice_img1 + slice_img2) > 0) {
    tibble_img1 <- tibble::as_tibble(table(slice_img1)) %>% dplyr::mutate(Label_No = as.integer(slice_img1))
    tibble_img2 <- tibble::as_tibble(table(slice_img2)) %>% dplyr::mutate(Label_No = as.integer(slice_img2))
    tibble_merge <- dplyr::full_join(tibble_img1, tibble_img2, by = "Label_No") %>%
      dplyr::mutate(slice_no = slice_no) %>%
      dplyr::select(slice_no, Label_No) %>%
      dplyr::filter(Label_No > 0)
      img1_lst <- rep(list(slice_img1),dim(tibble_merge)[1])
      img2_lst <- rep(list(slice_img2),dim(tibble_merge)[1])
      sliceno <- rep(list(slice_no), dim(tibble_merge)[1])
      paras <- list(sliceno, as.list(tibble_merge$Label_No), img1_lst, img2_lst)
      output <- purrr::pmap_dfr(paras, element_count)
    return(output)
  }
}
