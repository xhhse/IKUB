#' @title Segmentation comparison in a 3D view
#' @description Displays label segmentation differences in a 3D view.
#' @details Displays label segmentation differences in an MR image for supplied
#' label number. Region only present in filename_seg1 is displayed in green color.
#' Region only present in filename_seg2 is displayed in blue color. Region present in
#' both files is displayed in red color. 3D view can be rotated by dragging the
#' mouse.
#' @param filename_MR A NIfTI-file of an MR image
#' @param filename_seg1 First NIfTI-file of segmentation
#' @param filename_seg2 Second NIfTI-file of segmentation
#' @param targetlabel One label number for which region is to be displayed
#' @return 3D view with segmentation differences.
#' @export

segcomparison3D <- function(filename_MR, filename_seg1, filename_seg2, targetlabel) {
  img_seg1 <- neurobase::readnii(filename_seg1)
  img_seg2 <- neurobase::readnii(filename_seg2)
  img_MR <- neurobase::readnii(filename_MR)

  # Create and display contour for skull
  brain <- misc3d::contour3d(img_MR,
    x = 1:dim(img_MR)[1], y = 1:dim(img_MR)[2],
    z = 1:dim(img_MR)[3], level = 10, alpha = 0.1, draw = FALSE, engine = "standard"
  )
  misc3d::drawScene.rgl(brain)
  # Create segmentations for two input files
  img_seg1 <- ifelse(img_seg1 == targetlabel, 1, 0)
  img_seg2 <- ifelse(img_seg2 == targetlabel, 1, 0)
  img_diff <- img_seg1 + 2 * img_seg2
  img_green <- ifelse(img_diff == 1, 1, 0)
  img_blue <- ifelse(img_diff == 2, 2, 0)
  img_red <- ifelse(img_diff == 3, 3, 0)
  misc3d::contour3d(img_green, level = 1, alpha = 0.9, add = TRUE, color = "green")
  misc3d::contour3d(img_blue, level = 2, alpha = 0.9, add = TRUE, color = "blue")
  misc3d::contour3d(img_red, level = 3, alpha = 0.9, add = TRUE, color = "red")
  # Add text and legend
  rgl::text3d(x = dim(img_MR)[1] / 2, y = dim(img_MR)[2] * 0.98, z = dim(img_MR)[3] / 2, text = "A")
  rgl::text3d(x = dim(img_MR)[1] * 0.98, y = dim(img_MR)[2] / 2, z = dim(img_MR)[3] / 2, text = "R")
  rgl::legend3d("bottomright", legend = paste(c("Only in file1", "Only in file2", "In both files")), pch = 16, col = c("green", "blue", "red"), cex = 0.8, inset = 0.02)
  rgl::title3d(paste("Targetlabel: ", targetlabel, sep = ""))
}
