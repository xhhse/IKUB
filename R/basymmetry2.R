#' @title Asymmetry indices analysis data
#' @description Calculates asymmetry indices for one or more supplied
#' NIfTI-files.
#' @details Inputs one or more NIfTI-files to calculate asymmetry indices for
#' one or more NIfTI-files.
#' @param filename One or more NIfTI-files
#' @param labelnametibble A label name tibble with correct format and unique
#' columns. First column of the label name tibble should contain an integer
#' label number and second column should contain a label name string. Label
#' numbers and label names must be unique. The trailing characters of label
#' names must be R or L.
#' @return Asymmetry index/indices for the left and right regions for supplied
#' NIfTI-file(s).
#' @export

basymmetry2 <- function(filename, labelnametibble) {
  labelnametibble <- check_label(labelnametibble)
  output <- filename %>%
    purrr::map_dfr(regcount) %>%
    dplyr::left_join(labelnametibble, by = "Label_No") %>%
    dplyr::mutate(side = substring(
      trimws(Label_name), nchar(Label_name),nchar(Label_name))) %>%
    dplyr::filter(side == "R" | side == "L") %>%
    dplyr::mutate(item = substring(trimws(Label_name), 1, nchar(Label_name) - 2)) %>%
    dplyr::group_by(filename, item, side) %>%
    dplyr::summarise(tot_vol_mm3 = sum(vol_mm3), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = "side", values_from = "tot_vol_mm3") %>%
    dplyr::mutate(data.annotation = gsub(pattern = ".nii.gz", "", x = filename), asymmetry_index = round(2 * abs(R - L) / (R + L), 5)) %>%
    dplyr::select(data.annotation, variable_x = item, variable_y = asymmetry_index)
}
