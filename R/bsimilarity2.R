#' @title Jaccard indices analysis data
#' @description Calculates Jaccard indices for one or more file pairs.
#' @details Function is used to calculate Jaccard indices for all regions for
#' one or more file pairs. Each file pair contains two segmented NIfTI -files
#' which are generated from the same source.
#' directory.
#' @param filename1 One or more NIfTI-files
#' @param filename2 One or more NIfTI-files
#' @param labelnametibble A label name tibble with correct format and unique
#' columns. First column of the label name tibble should contain an integer
#' label number and second column should contain a label name string. Label
#' numbers and label names must be unique.
#' @return Jaccard indices for the supplied file pairs.
#' @export

bsimilarity2 <- function(filename1, filename2, labelnametibble) {
  labelnametibble <- check_label(labelnametibble)
  counts_output <- purrr::map2_dfr(filename1, filename2, files_element)
  jaccarddf <- counts_output %>% dplyr::group_by(Label_No, datalabel) %>%
    dplyr::summarise(tot_overlap=sum(overlap_counts), tot_file1=sum(file1_counts),tot_file2=sum(file2_counts), .groups = 'drop') %>%
    dplyr::mutate(jaccard.index=round(tot_overlap/(tot_file1+tot_file2-tot_overlap),3))%>%
    dplyr::left_join(labelnametibble, by='Label_No') %>%
    dplyr::select(datalabel, Label_No, Label_name, jaccard.index) %>%
    dplyr::mutate(variable_x=paste(Label_No, Label_name)) %>%
    dplyr::select(data.annotation=datalabel, variable_x, variable_y=jaccard.index)
}
