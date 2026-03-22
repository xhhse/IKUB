#' @title Dice coefficients and Jaccard indices
#' @description Calculates Dice coefficients and Jaccard indices for a file
#' pair.
#' @details Function is used to calculate Dice coefficients and Jaccard indices
#' for a file pair. A file pair contains two segmented NIfTI -files which are
#' generated from the same source.
#' @param filename1 One NIfTI-file
#' @param filename2 One NIfTI-file
#' @param labelnametibble A label name tibble with correct format and unique
#' columns. First column of the label name tibble should contain an integer
#' label number and second column should contain a label name string. Label
#' numbers and label names must be unique.
#' @return Dice coefficients and Jaccard indices for a supplied file pair.
#' Only regions which are in the label name tibble will be included in the
#' result.
#' @export
bsimilarity <- function(filename1, filename2, labelnametibble) {
  labelnametibble <- check_label(labelnametibble)
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
    output<- purrr::pmap_dfr(paras, slice_label) %>%
      dplyr::group_by(Label_No) %>%
      dplyr::summarise(tot_overlap = sum(overlap_counts), tot_file1 = sum(file1_counts), tot_file2 = sum(file2_counts)) %>%
      dplyr::mutate(dice.coefficient = round(2 * tot_overlap / (tot_file1 + tot_file2), 3), jaccard.index = round(tot_overlap / (tot_file1 + tot_file2 - tot_overlap), 3)) %>%
      dplyr::left_join(labelnametibble, by = "Label_No") %>%
      dplyr::select(Label_No, Label_name, dice.coefficient, jaccard.index)
    return(output)
  }
}


