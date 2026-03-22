#' @title Region z-scores
#' @description Calculates z-scores of all regions.
#' @details Inputs one or more NIfTI-files from individuals, for which z-scores
#' are to be calculated, a label name tibble with correct formats and unique
#' columns and a tibble and a tibble including mean values and standard
#' deviations from the reference group for one or more individuals. Returns
#' z-scores, with label names, for all regions.
#' @param filename One or more NIfTI-files for individuals from which z-scores
#' are to be calculated
#' @param labelnametibble A label name tibble
#' @param refstats A tibble which was previously calculated by refmeanstd
#' function
#' @return z-scores for all regions with label name for one or more NIfTI-files
#' @export

bzscore <- function(filename, labelnametibble, refstats) {
  labelnametibble <- check_label(labelnametibble)
  zscoredf <- filename %>%
    purrr::map_dfr(regcount) %>%
    dplyr::left_join(refstats, by = "Label_No") %>%
    dplyr::mutate(z_score = (vol_mm3 - mean) / std) %>%
    dplyr::right_join(labelnametibble, by = "Label_No") %>%
    dplyr::select(filename, Label_No, Label_name, z_score) %>%
    tidyr::pivot_wider(names_from = "filename", values_from = "z_score")
  return(zscoredf)
}
