image_file <- "img/biocnote-rh.png"

# use EBImage to read the image
library(EBImage)
img <- readImage(image_file)
# Display the image
# display(img)

# turn the image grayscale
img <- channel(img, "gray")
# display(img)

# Remove the background
threshold = otsu(img)

# Threshold the image
img_th <- img > threshold
# display(img_th)

image_data <- imageData(img_th)

storage.mode(image_data) <- "double"

# change coordinates to x axis increasing left to right and y axis increasing bottom to top
image_data_plot <- t(image_data)[seq(nrow(image_data), 1, -1), ]

# heatmap(image_data_plot, 
#       Rowv = NA, 
#       Colv = NA, 
#       col = gray.colors(256), 
#       scale = "none", 
#       xlab = "Columns", 
#       ylab = "Rows", 
#       main = "Heatmap of Image Data")

# make a scatter plot using ggplot
library(ggplot2)
image_xy_coords <- which(image_data_plot > 0, arr.ind = TRUE)

# ggplot(image_xy_coords, aes(col, row)) +
#   geom_point() +
#   labs(x = "Columns", y = "Rows", title = "Scatter Plot of Image Data") +
#   theme_void() +
#   coord_fixed()

# downsample xy coordinates every N rows and columns
downsample_factor <- 1.3
downsample_x <- round(20*downsample_factor)
downsample_y <- round(17*downsample_factor)
keep_rows <- seq(max(image_xy_coords[, "row"]) - 5, 1, by = -downsample_y)
keep_cols <- seq(5, max(image_xy_coords[, "col"]), by = downsample_x)
image_xy_coords_downsampled <- image_xy_coords[image_xy_coords[, "row"] %in% keep_rows & 
                                                  image_xy_coords[, "col"] %in% keep_cols, ]

ggplot(image_xy_coords_downsampled, aes(col, row)) +
  geom_point() +
  labs(x = "Columns", y = "Rows", title = "Scatter Plot of Image Data") +
  theme_void() +
  coord_fixed()

dim(image_xy_coords_downsampled)

# shift every other row by half the horizontal distance between points
rows_to_shift <- keep_rows[seq(2, length(keep_rows), by = 2)]
rows_to_shift <- image_xy_coords_downsampled[, "row"] %in% rows_to_shift
image_xy_coords_downsampled[rows_to_shift, "col"] <- image_xy_coords_downsampled[rows_to_shift,"col"] + downsample_x / 2

ggplot(image_xy_coords_downsampled, aes(col, row)) +
  geom_point(size = 0.1) +
  coord_fixed() +
  labs(x = NULL, y = NULL) +
  theme_void()

# q: what ggplot function allows one to plot an image at given coordinates?
# a: geom_image from the ggimage package

library(ggimage)
gg <- ggplot(image_xy_coords_downsampled, aes(col, row)) +
  geom_image(image = "img/Bioconductor-rh.png", size = 0.02) +
  labs(x = "Columns", y = "Rows", title = "Scatter Plot of Image Data") +
  theme_void() +
  coord_fixed()

ggsave("img/outputs/biochexwall.png", gg, width = 20, height = 20, dpi = 600)

sticker_files <- list.files("img/stickers_cropped", full.names = TRUE, pattern = "\\.png$")
length(sticker_files)
# sticker_files <- sticker_files[!grepl("CSAMA2019", sticker_files)]

sticker_files_pool <- rep(sticker_files, 1 + nrow(image_xy_coords_downsampled) %/% length(sticker_files))
sticker_files_pool <- sticker_files_pool[1:nrow(image_xy_coords_downsampled)]

final_df <- data.frame(
  image_xy_coords_downsampled,
  image = sticker_files_pool[1:nrow(image_xy_coords_downsampled)]
)

gg <- ggplot(final_df, aes(col, row)) +
  geom_image(aes(image = image), size = 0.02) +
  coord_fixed() +
  labs(x = NULL, y = NULL) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA)
  )
ggsave("img/outputs/biochexwall-mixed-600dpi.png", gg, width = 20, height = 20, dpi = 600)

# for each sticker in the pool, identify the predominant color
get_mean_colour <- function(file) {
  message(file)
  nuc = readImage(file)
  # display(nuc)
  nuc_data = imageData(nuc)[, , 1:3]
  # nuc_data is a three dimensional array
  # convert it into a data.frame of five columns: x, y, r, g, b
  nuc_df <- expand.grid(row = 1:nrow(nuc_data), col = 1:ncol(nuc_data))
  nuc_df <- nuc_df[seq(1, nrow(nuc_df), by = 10), ]
  nuc_df[, "R"] <- mapply(function(row, col) nuc_data[row, col, 1], row = nuc_df$row, col = nuc_df$col)
  nuc_df[, "G"] <- mapply(function(row, col) nuc_data[row, col, 2], row = nuc_df$row, col = nuc_df$col)
  nuc_df[, "B"] <- mapply(function(row, col) nuc_data[row, col, 3], row = nuc_df$row, col = nuc_df$col)
  # nuc_df <- subset(nuc_df, R + G + B < 3)  # remove white pixels
  mean_colour <- colMeans(nuc_df[, c("R", "G", "B")])
  rgb <- rgb(mean_colour[1], mean_colour[2], mean_colour[3])
  message(rgb)
  rgb
}

dir.create("cache", showWarnings = FALSE)
if (file.exists("cache/mean_colours.rds")) {
  mean_colours <- readRDS("cache/mean_colours.rds")
} else {
  mean_colours <- sapply(sticker_files, get_mean_colour, USE.NAMES = TRUE)
  saveRDS(mean_colours, "cache/mean_colours.rds")
}

library(TSP)
rgb <- col2rgb(mean_colours)
tsp <- as.TSP(dist(t(rgb)))
sol <- solve_TSP(tsp, control = list(repetitions = 1e3))
ordered_cols <- mean_colours[sol]

# plot(x = 1:length(ordered_cols), y = rep(1, length(ordered_cols)), col = ordered_cols)

ordered_stickers <- names(ordered_cols)
ordered_stickers_pool <- rep(ordered_stickers, 1 + nrow(image_xy_coords_downsampled) %/% length(ordered_stickers))
ordered_stickers_pool <- ordered_stickers_pool[1:nrow(image_xy_coords_downsampled)]

final_df <- data.frame(
  image_xy_coords_downsampled
)
# sort left to right, bottom to top
# final_df <- final_df[order(final_df$row, final_df$col),]
# sort by distance from bottom left corner
final_df <- final_df[order(final_df$row^2 + final_df$col^2), ]
final_df$image = ordered_stickers_pool[1:nrow(image_xy_coords_downsampled)]

gg <- ggplot(final_df, aes(col, row)) +
  geom_image(aes(image = image), size = 0.02) +
  coord_fixed() +
  labs(x = NULL, y = NULL) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA)
  )
ggsave("img/outputs/biochexwall-sorted-600dpi.png", gg, width = 20, height = 20, dpi = 600)
