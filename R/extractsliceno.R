#' @title Extract slice numbers
#' @description Extracts slice numbers for one or more supplied label
#' numbers.
#' @details Input one NIfTI-file and one or more target label numbers. Returns
#' slice number(s) which include the supplied target label number(s).
#' @param filename One NIfTI-file
#' @param target_label One or more label numbers
#' @param anatomical_plane One for coronal plane, Two for sagittal plane and
#' Three for transverse plane. Default is Three.
#' @return Slice numbers which include the target label number(s).
#' @export

extractsliceno <- function(filename, target_label, anatomical_plane = 3) {
  input_img <- neurobase::readnii(filename)
  target_output <- ""
  for (i in 1:length(target_label)) {
    img <- ifelse(input_img == target_label[i], 1, 0)
    output <- ""
    if (anatomical_plane == 1) {
      for (slice_no in 1:dim(img)[2]) {
        chk_con <- sum(img[, slice_no, ])
        while (chk_con > 0) {
          output <- paste(output, slice_no, sep = ",")
          chk_con <- 0
        }
      }
    } else if (anatomical_plane == 2) {
      for (slice_no in 1:dim(img)[1]) {
        chk_con <- sum(img[slice_no, , ])
        while (chk_con > 0) {
          output <- paste(output, slice_no, sep = ",")
          chk_con <- 0
        }
      }
    } else {
      for (slice_no in 1:dim(img)[3]) {
        chk_con <- sum(img[, , slice_no])
        while (chk_con > 0) {
          output <- paste(output, slice_no, sep = ",")
          chk_con <- 0
        }
      }
    }
    # output <- strsplit(substring(output, 2, nchar(output)), split=',')
    output <- substring(output, 2, nchar(output))
    target_output <- paste(target_output, output, sep = ",")
  }
  target_output <- strsplit(substring(target_output, 2, nchar(target_output)), split = ",")
  return(sort(as.integer(unique(as.vector(target_output[[1]])))))
}
