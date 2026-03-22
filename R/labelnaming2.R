#' @title Region volumes analysis data
#' @description Calculates volumes for all regions and save the calculation
#' in a csv file.
#' @details Input one or more NIfTI-files and a label name tibble. Returns
#' total volumes, with label names, for all regions in the supplied NIfTI-file.
#' A label name tibble must have correct format and unique columns. First column
#' of the label name tibble must contain an integer label number and second
#' column must contain a label name string. Label numbers and label names must
#' be unique. Only regions which are in the label name tibble will be included
#' in the result.
#' @param filename One or more NIfTI-files
#' @param labelnametibble A label name tibble
#' @return Total volumes with label names, taken from the supplied label name
#' tibble, for all regions in the supplied NIfTI-file(s).
#' @export

labelnaming2 <- function(filename, labelnametibble) {
  labelnametibble <- check_label(labelnametibble)
  voldf <- filename %>%
    purrr::map_dfr(regcount) %>%
    dplyr::right_join(labelnametibble, by = "Label_No") %>%
    dplyr::select(filename, Label_No, Label_name, vol_mm3) %>%
    dplyr::mutate(data.annotation = gsub(pattern = ".nii.gz", "", x = filename), variable_x = paste(Label_No, Label_name)) %>%
    dplyr::select(data.annotation, variable_x, variable_y = vol_mm3)

}
