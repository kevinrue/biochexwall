image_file <- "bioconductor_logo_note.png"

# use EBImage to read the image
library(EBImage)
img <- readImage(image_file)
# Display the image
display(img)

# turn the image grayscale
img <- channel(img, "gray")
display(img)

# Remove the background
threshold = otsu(img)

# Threshold the image
img_th <- img > threshold
display(img_th)

image_data <- imageData(img_th)

storage.mode(image_data) <- "double"

# change coordinates to x axis increasing left to right and y axis increasing bottom to top
image_data_plot <- t(image_data)[seq(nrow(image_data), 1, -1), ]

heatmap(image_data_plot, 
      Rowv = NA, 
      Colv = NA, 
      col = gray.colors(256), 
      scale = "none", 
      xlab = "Columns", 
      ylab = "Rows", 
      main = "Heatmap of Image Data")

# make a scatter plot using ggplot
library(ggplot2)
image_xy_coords <- which(image_data_plot > 0, arr.ind = TRUE)

ggplot(image_xy_coords, aes(col, row)) +
  geom_point() +
  labs(x = "Columns", y = "Rows", title = "Scatter Plot of Image Data") +
  theme_void() +
  coord_fixed()

# downsample xy coordinates every N rows and columns
downsample_factor <- 20
keep_rows <- seq(1, max(image_xy_coords[, "row"]), by = downsample_factor)
keep_cols <- seq(1, max(image_xy_coords[, "col"]), by = downsample_factor)
image_xy_coords_downsampled <- image_xy_coords[image_xy_coords[, "row"] %in% keep_rows & 
                                                  image_xy_coords[, "col"] %in% keep_cols, ]

ggplot(image_xy_coords_downsampled, aes(col, row)) +
  geom_point() +
  labs(x = "Columns", y = "Rows", title = "Scatter Plot of Image Data") +
  theme_void() +
  coord_fixed()

# shift every other row by half the horizontal distance between points
rows_to_shift <- keep_rows[seq(2, length(keep_rows), by = 2)]
rows_to_shift <- image_xy_coords_downsampled[, "row"] %in% rows_to_shift
image_xy_coords_downsampled[rows_to_shift, "col"] <- image_xy_coords_downsampled[rows_to_shift,"col"] + downsample_factor / 2

ggplot(image_xy_coords_downsampled, aes(col, row)) +
  geom_point(size = 0.1) +
  labs(x = "Columns", y = "Rows", title = "Scatter Plot of Image Data") +
  theme_void() +
  coord_fixed()

# q: what ggplot function allows one to plot an image at given coordinates?
# a: geom_image from the ggimage package

library(ggimage)
gg <- ggplot(image_xy_coords_downsampled, aes(col, row)) +
  geom_image(image = "img/Bioconductor-rh.png", size = 0.015) +
  labs(x = "Columns", y = "Rows", title = "Scatter Plot of Image Data") +
  theme_void() +
  coord_fixed()

ggsave("img/outputs/biochexwall.png", gg, width = 20, height = 20, dpi = 300)

sticker_files <- list.files("img/stickers_cropped", full.names = TRUE, pattern = "\\.png$")
# sticker_files <- sticker_files[!grepl("CSAMA2019", sticker_files)]

final_df <- data.frame(
  image_xy_coords_downsampled,
  image = c(sticker_files, sticker_files[1:(nrow(image_xy_coords_downsampled) - length(sticker_files))])
)

gg <- ggplot(final_df, aes(col, row)) +
  geom_image(aes(image = image), size = 0.015) +
  coord_fixed() +
  labs(x = NULL, y = NULL) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA)
  )
ggsave("img/outputs/biochexwall-mixed-600.png", gg, width = 20, height = 20, dpi = 600)

