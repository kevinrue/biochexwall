library(EBImage)
library(geometry)

sticker_files <- list.files("img/stickers", full.names = TRUE, pattern = "\\.png$")

# library(EBImageExtra)

# nuc = readImage("img/stickers/affy.png")
# display(nuc)

dir.create("img/stickers_cropped", showWarnings = FALSE, recursive = TRUE)

for (infile in sticker_files) {
  output_file <- file.path("img/stickers_cropped", basename(infile))
  if (file.exists(output_file)) {
    # message("Skipping already processed file:", infile)
    next
  }
  message("Processing ", infile)
  nuc = readImage(infile)
  # display(nuc)
  if (infile == "img/stickers/NanoMethViz.png") {
  	writeImage(nuc, output_file, type = "png")
  }
  if (dim(nuc)[3] == 3) {
    nuc <- Image(abind(nuc, array(1, dim = c(dim(nuc)[1], dim(nuc)[2], 1)), along = 3), colormode = "Color")
  }

  sum_rgb <- rowSums(nuc[, , 1:3], dims = 2)
  bg_colour <- ifelse(sum_rgb[1,1] == 0, "black", "white")
  if (bg_colour == "black") {
    bg_idx <- which(sum_rgb == 0, arr.ind = TRUE)
  } else {
    bg_idx <- which(sum_rgb > 2.98, arr.ind = TRUE) # bioCAsia 2025 I'm looking at you
  }

  # use row and column indices in bg_idx to set those pixels to transparent
  for (i in seq_len(nrow(bg_idx))) {
    row <- bg_idx[i, "row"]
    col <- bg_idx[i, "col"]
    nuc[row, col, 4] <- 0  # set alpha channel to 0 for background pixels
  }
  # display(nuc)

  # identify coordinates of convex hull of solid pixels (where alpha channel is 1)
  coords_solid <- which(imageData(nuc)[,,4] == 1, arr.ind = TRUE)
  # ggplot(as.data.frame(coords_solid), aes(row, -col)) +
  #   geom_point() +
  #   labs(x = "Columns", y = "Rows", title = "Scatter Plot of Solid Pixels") +
  #   coord_fixed()
  # chull_keep <- chull(coords_solid)
  # coords_chull <- coords_solid[chull_keep, ]
  coords_chull <- convhulln(coords_solid)
  coords_transparent <- which(imageData(nuc)[,,4] == 0, arr.ind = TRUE)
  storage.mode(coords_transparent) <- "double"
  coors_to_reset <- coords_transparent[geometry::inhulln(coords_chull, coords_transparent),]
  coors_to_reset <- matrix(coors_to_reset, ncol = 2, dimnames = list(c(), c("row", "col")))
  # ggplot(as.data.frame(coors_to_reset), aes(row, -col)) +
  #   geom_point() +
  #   labs(x = "Columns", y = "Rows", title = "Scatter Plot of Solid Pixels") +
  #   coord_fixed()

  for (i in seq_len(nrow(coors_to_reset))) {
    row <- coors_to_reset[i, "row"]
    col <- coors_to_reset[i, "col"]
    nuc[row, col, 4] <- 1  # restore alpha channel to 1 for pixels within convex hull
  }

  # display(nuc)

  # plot(coords_solid, type = "l", col = "red", lwd = 2, main = "Convex Hull of Solid Pixels")

  # image_data_plot <- t(image_data)[seq(nrow(image_data), 1, -1), ]
  # heatmap(image_data_plot,
  #         Rowv = NA,
  #         Colv = NA,
  #         col = gray.colors(256),
  #         scale = "none",
  #         xlab = "Columns",
  #         ylab = "Rows",
  #         main = "Heatmap of Image Data")

  writeImage(nuc, output_file, type = "png")
}


