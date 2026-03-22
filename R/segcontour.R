#' @title Displays label contours in an MR image
#' @description Displays one or more label contours for one or more slices in
#' an MR image.
#' @details Displays one or more label contours for one or more slices in an MR
#' image for given label numbers. Contours lie on the edge of regions.
#' @param filename_MR A NIfTI-file of an MR image
#' @param filename_seg A NIfTI-file of segmentation
#' @param labelnametibble A label name tibble with correct format and unique
#' columns. First column of the label name tibble must contain an integer label
#' number and second column must contain a label name string. Label numbers
#' and label names must be unique.
#' @param slice_no One or more slice numbers for which label contours are to be
#' displayed.
#' @param target_label One or more label numbers for which label contours are
#' to be displayed.
#' @param anatomical_plane 1 for coronal plane, 2 for sagittal plane and 3 for
#' transverse plane. Default is 3.
#' @param rowno Number of rows for showing images. Default is 1.
#' @param colno Number of columns for showing images. Default is 1.
#' @return One or more MR images with label contours for slice(s). A legend for
#' label contours with label names. Only contours which are in the label name
#' tibble and input label numbers will be displayed in the result.
#' @export

segcontour <- function(filename_MR, filename_seg, labelnametibble, slice_no, target_label, anatomical_plane = 3, rowno = 1, colno = 1) {
  labelnametibble <- check_label(labelnametibble)
  if (!is.null(grDevices::dev.list())) grDevices::dev.off()
  # Set graphical parameters
  graphics::par(mfcol = c(rowno, colno))
  target_label <- target_label %>%
    tibble::as_tibble() %>%
    stats::setNames("Label_No")
  t1 <- neurobase::readnii(filename_seg)
  t1_MR <- neurobase::readnii(filename_MR)
  # Define target and target_MR for cornal, sagittal and transverse plane
  if (anatomical_plane == 1) { # cornal plane
    target <- t1[, slice_no, ]
    target_MR <- t1_MR[, slice_no, ]
  } else if (anatomical_plane == 2) { # sagittal plane
    target <- t1[slice_no, , ]
    target_MR <- t1_MR[slice_no, , ]
  } else if (anatomical_plane == 3) { # transverse plane
    target <- t1[, , slice_no]
    target_MR <- t1_MR[, , slice_no]
  } else {
    stop("Input for antomaical plane must be 1 for cornal plane, 2 for sagittal plane and 3 for transverse plane.")
  }
  # Create a tibble including label no, label name, RGB values for all slices
  RGBtibble <- tibble::as_tibble(table(target)) %>%
    dplyr::mutate(Label_No = as.integer(target)) %>%
    dplyr::inner_join(labelnametibble, by = "Label_No") %>%
    dplyr::inner_join(target_label, by = "Label_No")
  RGBtibble <- RGBtibble %>%
    dplyr::mutate(R_value = rep(c(1, 0, 1, 0, 1), len = nrow(RGBtibble)), G_value = rep(c(0, 1, 1, 1, 0), len = nrow(RGBtibble)), B_value = rep(c(0, 0, 0, 1, 1), len = nrow(RGBtibble))) %>%
    dplyr::select(Label_No, Label_name, R_value, G_value, B_value)
  if (nrow(RGBtibble) == 0) {
    stop("Target label(s) is/are not included in slice(s).")
  } else {
    # Display label contour legend
    graphics::plot(NULL, xaxt = "n", yaxt = "n", bty = "n", ylab = "", xlab = "", xlim = 0:1, ylim = 0:1)
    graphics::legend("topleft", legend = as.vector(RGBtibble$Label_name), pch = 15, pt.cex = 1.3, cex = 0.7, bty = "n", xjust = 0, yjust = 0, col = rep(c("red", "green", "yellow", "cyan", "magenta"), len = nrow(RGBtibble)))
    graphics::mtext("Label Contour", at = 0.1, cex = 0.8)

    for (i in 1:length(slice_no)) {
      # if #slice=0, slice_img/slice_MR = target/target_MR matrix
      if (length(slice_no) == 1) {
        slice_img <- target
        slice_MR <- EBImage::normalize(target_MR)
      } else {
        # if #slice is more than 1, should handle every slice
        if (anatomical_plane == 1) {
          slice_img <- target[, i, ]
          slice_MR <- EBImage::normalize(target_MR[, i, ])
        } else if (anatomical_plane == 2) {
          slice_img <- target[i, , ]
          slice_MR <- EBImage::normalize(target_MR[i, , ])
        } else if (anatomical_plane == 3) {
          slice_img <- target[, , i]
          slice_MR <- EBImage::normalize(target_MR[, , i])
        } else {
          stop("Input for antomaical plane must be 1 for sagittal plane, 2 for coronal plane and 3 for transverse plane.")
        }
      }
      # Covert MR image to RGB image
      slice_MR <- EBImage::toRGB(slice_MR)
      # Create a tibble including label no, label name, RGB values for one slice
      slice_RGBtibble <- tibble::as_tibble(table(slice_img)) %>%
        dplyr::mutate(Label_No = as.integer(slice_img)) %>%
        dplyr::inner_join(RGBtibble, by = "Label_No") %>%
        dplyr::select(Label_No, Label_name, R_value, G_value, B_value)
      # Create one empty image and convert it to RGB image
      com_img <- matrix(0, nrow = dim(slice_img)[1], ncol = dim(slice_img)[2])
      com_img <- EBImage::toRGB(com_img)
      # Handle contour for every label number
      for (j in 1:nrow(slice_RGBtibble)) {
        img <- slice_img
        # zero pixel-value which is not equal to target label
        img[which(img != as.numeric(slice_RGBtibble[j, 1]))] <- 0
        # Edge detection.Edge Edge is outside the label.
        fhi <- matrix(1, nrow = 3, ncol = 3)
        fhi[2, 2] <- -8
        img_fhi <- EBImage::filter2(img, fhi)
        # Move in eage.Egeg is on the label.
        img_fhi <- round(img_fhi, 0)
        img_fhi[which(img_fhi >= 0)] <- 0
        img_fhi <- abs(img_fhi)
        # Give label contour RGB values
        contour_img <- EBImage::rgbImage(red = img_fhi * as.numeric(slice_RGBtibble[j, 3]), green = img_fhi * as.numeric(slice_RGBtibble[j, 4]), blue = img_fhi * as.numeric(slice_RGBtibble[j, 5]))
        # Overlay label contour images
        com_img <- EBImage::combine(com_img + contour_img)
      }
      # combine overlaid label contour image and MR image
      output <- EBImage::combine(slice_MR + com_img)
      # Rotate image
      img_rotate <- EBImage::rotate(output, 180, bg.col = "white")
      EBImage::display(img_rotate, method = "raster")
      graphics::mtext(paste("slice:", slice_no[i], sep = ""), col = "red", side = 4, line = -2)
    }
  }
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))
}
