#' @title Displays label surfaces in a 3D view
#' @description Displays one or more label surfaces in an MR image in a 3D view.
#' @details Displays one or more label surfaces in an MR image for given label
#' numbers. 3D view can be rotated by dragging the mouse.
#' @param filename_MR A NIfTI-file of an MR image
#' @param filename_seg A NIfTI-file of segmentation
#' @param labelnametibble A label name tibble with correct format and unique
#' columns. First column of the label name tibble should contain an integer
#' label number and second column should contain a label name string. Label
#' numbers and label names must be unique. Three columns, R, G, B, should be
#' included in the labelnametibble.
#' @param targetlabel One or more label numbers for which label surfaces are to
#' be shown.
#' @return 3D view with label surface(s). The different segments will have the
#' color from the R, G, B columns in the labelnametibble. A legend for label
#' surfaces with label names. Only surfaces which are in the label name tibble
#' and input label numbers will be displayed in the result.
#' @export

segsurface3D <- function(filename_MR, filename_seg, labelnametibble, targetlabel) {
  labelnametibble <- check_label_RGB(labelnametibble)
  img_MR <- neurobase::readnii(filename_MR)
  img_seg <- neurobase::readnii(filename_seg)
  # Create labeldf for targetlabel
  rgb2hex <- function(r, g, b) grDevices::rgb(r, g, b, maxColorValue = 255)
  labeldf <- tibble::tibble(Label_No = as.integer(targetlabel)) %>%
    dplyr::left_join(labelnametibble, by = "Label_No")
  labeldf <- labeldf %>% dplyr::mutate(color = rgb2hex(labeldf$R, labeldf$G, labeldf$B))

  # Create and display contour for skull
  brain <- misc3d::contour3d(img_MR,
    x = 1:dim(img_MR)[1], y = 1:dim(img_MR)[2],
    z = 1:dim(img_MR)[3], level = 10, alpha = 0.1, draw = FALSE, engine = "standard"
  )

  misc3d::drawScene.rgl(brain)

  # Display segmentations
  for (i in (1:dim(labeldf)[1])) {
    img <- img_seg
    img[which(img != labeldf$Label_No[i])] <- 0
    misc3d::contour3d(img, level = labeldf$Label_No[i], alpha = 0.9, add = TRUE, color = labeldf$color[i])
  }

  # Add text and legend
  rgl::text3d(x = dim(img_MR)[1] / 2, y = dim(img_MR)[2] * 0.98, z = dim(img_MR)[3] / 2, text = "A")
  rgl::text3d(x = dim(img_MR)[1] * 0.98, y = dim(img_MR)[2] / 2, z = dim(img_MR)[3] / 2, text = "R")
  rgl::legend3d("topright", legend = labeldf$Label_name, pch = 16, col = labeldf$color, cex = 0.8, inset = 0.02)
}
