#' @title Check labelnametibble with R, G, B columns
#' @description Checks the format of the label name tibble.
#' @details Checks if label number is an integer and if there are any duplicate
#' label numbers or label names in the tibble. Checks if the tibble includes R,
#' G, B columns.
#' @param labelnametibble A label name tibble
#' @return An error message or a label name tibble.
#' @export
check_label_RGB <- function(labelnametibble) {
  if (prod(c("Label_No", "Label_name", "R", "G", "B") %in% names(labelnametibble)) == 1) {
    if (is.integer(labelnametibble$Label_No) == FALSE) {
      stop("Please convert label number to integer.")
    } else {
      if (sum(duplicated(labelnametibble$Label_No)) + sum(duplicated(labelnametibble$Label_name)) > 0) {
        stop("There are duplicated label numbers or label names in the label name tibble.")
      } else {
        labelnametibble
      }
    }
  } else {
    stop("Label_No, label_name, R, G or B colum do not exit in the labelnametibble.")
  }
}
