#' @title Mean och std for z-score
#' @description Calculates mean and standard deviation for all regions in a
#' reference group.
#' @details Input NIfTI-files from a reference group to calculate mean and
#' standard deviation. The reference group must contain at least two NIfTI
#' files.
#' @param reflabels At least two NIfTI-files
#' @return Mean values and standard deviations for all regions in a reference
#' group.
#' @export

refmeanstd <- function(reflabels) {
  refstats <- reflabels %>%
    purrr::map_dfr(regcount) %>%
    dplyr::group_by(Label_No) %>%
    dplyr::summarize(mean = mean(vol_mm3), std = stats::sd(vol_mm3))
  return(refstats)
}
