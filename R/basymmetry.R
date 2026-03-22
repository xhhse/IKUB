#' @title Asymmetry indices
#' @description Calculates asymmetry indices for one or more supplied
#' NIfTI-files.
#' @details Input one or more NIfTI-files and returns asymmetry indices for
#' whole brain or returns asymmetry indices for the left and right regions.
#' @param filename One or more NIfTI-files
#' @param labelnametibble A label name tibble with correct format and unique
#' columns. First column of the label name tibble should contain an integer
#' label number and second column should contain a label name string. Label
#' numbers and label names must be unique. The trailing characters of label
#' names must be R or L.
#' @param option 1: Returns asymmetry indices for whole brain. 2: Returns
#' asymmetry indices for the left and right regions.
#' @return Asymmetry index/indices for whole brain or asymmetry indices for
#' the left and right regions for supplied NIfTI-file(s).
#' @export

basymmetry <- function(filename, labelnametibble, option = 1) {
  labelnametibble <- check_label(labelnametibble)
  volume_cal <- filename %>%
    purrr::map_dfr(regcount) %>%
    dplyr::left_join(labelnametibble, by = "Label_No") %>%
    dplyr::mutate(side = substring(trimws(Label_name), nchar(Label_name),nchar(Label_name))) %>%
    dplyr::filter(side == "R" | side == "L") %>%
    dplyr::mutate(item = substring(trimws(Label_name), 1, nchar(Label_name) - 2))

  if (option == 1) {
    output <- volume_cal %>%
      dplyr::group_by(filename, side) %>%
      dplyr::summarise(tot_vol_mm3 = sum(vol_mm3), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = "side", values_from = "tot_vol_mm3") %>%
      dplyr::mutate(asymmetry_index = round(2 * abs(R - L) / (R + L), 5)) %>%
      dplyr::select(filename, asymmetry_index)
    return(output)
  } else {
    output <- volume_cal %>%
      dplyr::group_by(filename, item, side) %>%
      dplyr::summarise(tot_vol_mm3 = sum(vol_mm3), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = "side", values_from = "tot_vol_mm3") %>%
      dplyr::mutate(asymmetry_index = round(2 * abs(R - L) / (R + L), 5)) %>%
      dplyr::select(filename, item, asymmetry_index) %>%
      tidyr::pivot_wider(names_from = "filename", values_from = "asymmetry_index")
    return(output)
  }
}
