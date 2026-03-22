#' @title Segmentation comparison
#' @description Displays label segmentation differences in an MR image.
#' @details Displays label segmentation differences in an MR image for a
#' supplied label number. Region only present in filename_seg 1 is displayed in
#' green color. Region only present in filename_seg2 is displayed in blue color.
#' Region present in both files is displayed in red color.
#' @param filename_MR A NIfTI-file of an MR image.
#' @param filename_seg1 First NIfTI-file of segmentation
#' @param filename_seg2 Second NIfTI-file of segmentation
#' @param Label_No One label number for which region is to be displayed
#' @param anatomical_plane 1 for coronal plane, 2 for sagittal plane and 3 for
#' transverse plane. Default is 3.
#' @param rowno Number of rows for showing images. Default is 1.
#' @param colno Number of columns for showing images. Default is 1.
#' @return Label segmentation differences in supplied MR image for supplied
#' label number.
#' @export

segcomparison <- function(filename_MR, filename_seg1, filename_seg2, Label_No, anatomical_plane = 3, rowno = 1, colno = 1) {
  if (!is.null(grDevices::dev.list())) grDevices::dev.off()
  graphics::par(mfcol = c(rowno, colno))
  target1 <- neurobase::readnii(filename_seg1)
  target2 <- neurobase::readnii(filename_seg2)
  target_MR <- neurobase::readnii(filename_MR)
  chk_dim <- dim(target1) - dim(target2)
  # check dimensions
  if (chk_dim[1] != 0 | chk_dim[2] != 0 | chk_dim[3] != 0) {
    stop("Files do not have the same diemensions.")
  } else {
    target1 <- ifelse(target1 == Label_No, 1, 0)
    target2 <- ifelse(target2 == Label_No, 1, 0)
    target_diff <- target1 + 2 * target2
    img_1 <- ifelse(target_diff == 1, 1, 0)
    img_2 <- ifelse(target_diff == 2, 2, 0)
    img_3 <- ifelse(target_diff == 3, 3, 0)
    if (anatomical_plane == 1) {
      for (slice_no in 1:dim(target1)[2]) {
        chk_con <- sum(target_diff[, slice_no, ])
        while (chk_con > 0) {
          slice_img1 <- EBImage::rgbImage(red = img_1[, slice_no, ] * 0, green = img_1[, slice_no, ] * 0.3, blue = img_1[, slice_no, ] * 0)
          slice_img2 <- EBImage::rgbImage(red = img_2[, slice_no, ] * 0, green = img_2[, slice_no, ] * 0, blue = img_2[, slice_no, ] * 0.5)
          slice_img3 <- EBImage::rgbImage(red = img_3[, slice_no, ] * 0.1, green = img_3[, slice_no, ] * 0, blue = img_3[, slice_no, ] * 0)
          slice_MR <- EBImage::normalize(EBImage::toRGB(target_MR[, slice_no, ]))
          output <- EBImage::combine(slice_img1 + slice_img2 + slice_img3 + slice_MR)
          img_rotate <- EBImage::rotate(output, 180, bg.col = "white")
          EBImage::display(img_rotate, method = "raster")
          graphics::title(paste("slice:", slice_no, sep = ""), col.main = "white", col.sub = "red")
          chk_con <- 0
        }
      }
    } else if (anatomical_plane == 2) {
      for (slice_no in 1:dim(target1)[1]) {
        chk_con <- sum(target_diff[slice_no, , ])
        while (chk_con > 0) {
          slice_img1 <- EBImage::rgbImage(red = img_1[slice_no, , ] * 0, green = img_1[slice_no, , ] * 0.3, blue = img_1[slice_no, , ] * 0)
          slice_img2 <- EBImage::rgbImage(red = img_2[slice_no, , ] * 0, green = img_2[slice_no, , ] * 0, blue = img_2[slice_no, , ] * 0.5)
          slice_img3 <- EBImage::rgbImage(red = img_3[slice_no, , ] * 0.1, green = img_3[slice_no, , ] * 0, blue = img_3[slice_no, , ] * 0)
          slice_MR <- EBImage::normalize(EBImage::toRGB(target_MR[slice_no, , ]))
          output <- EBImage::combine(slice_img1 + slice_img2 + slice_img3 + slice_MR)
          img_rotate <- EBImage::rotate(output, 180, bg.col = "white")
          EBImage::display(img_rotate, method = "raster")
          graphics::title(paste("slice:", slice_no, sep = ""), col.main = "white", col.sub = "red")
          chk_con <- 0
        }
      }
    } else {
      for (slice_no in 1:dim(target1)[3]) {
        chk_con <- sum(target_diff[, , slice_no])
        while (chk_con > 0) {
          slice_img1 <- EBImage::rgbImage(red = img_1[, , slice_no] * 0, green = img_1[, , slice_no] * 0.3, blue = img_1[, , slice_no] * 0)
          slice_img2 <- EBImage::rgbImage(red = img_2[, , slice_no] * 0, green = img_2[, , slice_no] * 0, blue = img_2[, , slice_no] * 0.5)
          slice_img3 <- EBImage::rgbImage(red = img_3[, , slice_no] * 0.1, green = img_3[, , slice_no] * 0, blue = img_3[, , slice_no] * 0)
          slice_MR <- EBImage::normalize(EBImage::toRGB(target_MR[, , slice_no]))
          output <- EBImage::combine(slice_img1 + slice_img2 + slice_img3 + slice_MR)
          img_rotate <- EBImage::rotate(output, 180, bg.col = "white")
          EBImage::display(img_rotate, method = "raster")
          graphics::title(paste("slice:", slice_no, sep = ""), col.main = "white", col.sub = "red")
          chk_con <- 0
        }
      }
    }
  }
  print("Region only presents in file 1 is displayed in green color. Region only presents in file 2 is displayed in blue color. Region presents in both files is displayed in red color.")
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))
}
