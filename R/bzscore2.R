#' @title z-scores analysis data
#' @description Calculates and saves z-scores of all regions.
#' @details Input one or many NIfTI-files from individuals, for which z-scores
#' are to be calculated, a label name tibble with correct formats and unique
#' columns and a tibble including mean values and standard deviations from the
#' reference group for one or more individuals.
#' @param targets One or more NIfTI-files for individuals from which z-scores
#' are to be calculated
#' @param labelnametibble A label name tibble
#' @param refstats a tibble which was previously calculated by refmeanstd
#' function
#' @return z-scores for all regions with label name for one or more NIfTI-files
#' @export

bzscore2 <- function(targets, labelnametibble, refstats) {
  labelnametibble <- check_label(labelnametibble)
  targetvols <- targets %>% purrr::map_dfr(regcount)
  zscoredf <- dplyr::left_join(targetvols, refstats, by = "Label_No") %>%
    dplyr::mutate(z_score = (vol_mm3 - mean) / std) %>%
    dplyr::right_join(labelnametibble, by = "Label_No") %>%
    dplyr::select(filename, Label_No, Label_name, z_score) %>%
    dplyr::mutate(data.annotation = gsub(pattern = ".nii.gz", "", x = filename), variable_x = paste(Label_No, Label_name)) %>%
    dplyr::select(data.annotation, variable_x, variable_y = z_score)

}
