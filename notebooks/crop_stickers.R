sticker_files <- list.files("img/stickers", full.names = TRUE, pattern = "\\.png$")

# library(EBImageExtra)

# nuc = readImage("img/stickers/affy.png")
# display(nuc)

dir.create("img/stickers_cropped", showWarnings = FALSE, recursive = TRUE)

for (infile in sticker_files) {
  output_file <- file.path("img/stickers_cropped", basename(infile))
  if (file.exists(output_file)) {
    message("Skipping already processed file:", infile)
    next
  }
  message("Processing ", infile)
  nuc = readImage(infile)
  
  if (dim(nuc)[3] == 3) {
    nuc <- Image(abind(nuc, array(1, dim = c(dim(nuc)[1], dim(nuc)[2], 1)), along = 3), colormode = "Color")
  }

  sum_rgb <- rowSums(nuc[, , 1:3], dims = 2)
  bg_idx <- which(sum_rgb == 3, arr.ind = TRUE)
  
  # use row and column indices in bg_idx to set those pixels to transparent
  for (i in seq_len(nrow(bg_idx))) {
    row <- bg_idx[i, "row"]
    col <- bg_idx[i, "col"]
    nuc[row, col, 4] <- 0  # set alpha channel to 0 for background pixels
  }
  
  # nuc[bg_idx[,"row"],bg_idx[,"col"],4] <- 0  # set alpha channel to 0 for background pixels
  # display(nuc)
  
  writeImage(nuc, output_file, type = "png")
}
