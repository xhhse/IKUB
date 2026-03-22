#' @title Labelnametibble check
#' @description Checks the format of the label name tibble.
#' @details Checks if label number is an integer and if there are any duplicate
#' label numbers or label names in the tibble.
#' @param labelnametibble A label name tibble
#' @return An error message or a label name tibble.
#' @export
check_label <- function(labelnametibble) {
  if (prod(c("Label_No", "Label_name") %in% names(labelnametibble)) == 1) {
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
    stop("Label_No or label_name colum do not exit in the labelnametibble.")
  }
}
