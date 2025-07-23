library(dplyr)
library(ggimage)

reverse <- FALSE

image_file <- "img/biocnote-rh.png"
sticker_files <- list.files("img/stickers_cropped", full.names = TRUE, pattern = "\\.png$")

dir.create("img/outputs", showWarnings = FALSE)

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

ggplot(image_xy_coords, aes(col, row)) +
  geom_point() +
  labs(x = "Columns", y = "Rows", title = "Scatter Plot of Image Data") +
  theme_void() +
  coord_fixed()

# downsample xy coordinates every N rows and columns

## different x and y to account for hexagonal geometry
# downsample_factor <- 1.27
# downsample_x <- round(20*downsample_factor)
# downsample_y <- round(17*downsample_factor)
# keep_rows <- seq(max(image_xy_coords[, "row"]) - 5, 1, by = -downsample_y)
# keep_cols <- seq(5, max(image_xy_coords[, "col"]), by = downsample_x)

## same x and y with mathematical fix later
downsample <- 23
keep_rows <- seq(max(image_xy_coords[, "row"]) - 5, 1, by = -downsample)
keep_cols <- seq(5, max(image_xy_coords[, "col"]), by = downsample)

image_xy_coords_downsampled <- as.data.frame(image_xy_coords[image_xy_coords[, "row"] %in% keep_rows & image_xy_coords[, "col"] %in% keep_cols, ])

ggplot(image_xy_coords_downsampled, aes(col, row)) +
	geom_point() +
	labs(x = "Columns", y = "Rows", title = "Scatter Plot of Image Data") +
	theme_bw() +
	coord_fixed()

# shift every other row by half the horizontal distance between points
rows_to_shift <- keep_rows[seq(2, length(keep_rows), by = 2)]
rows_to_shift <- image_xy_coords_downsampled[, "row"] %in% rows_to_shift
# image_xy_coords_downsampled[rows_to_shift, "col"] <- image_xy_coords_downsampled[rows_to_shift,"col"] + downsample_x / 2
image_xy_coords_downsampled[rows_to_shift, "col"] <- image_xy_coords_downsampled[rows_to_shift,"col"] + downsample / 2

ggplot(image_xy_coords_downsampled, aes(col, row)) +
	geom_point() +
	coord_fixed() +
	labs(x = NULL, y = NULL) +
	theme_bw()

# shift every row from the second onwards, downwards to stack them tighter
unique_row_values <- sort(unique(image_xy_coords_downsampled$row))
downward_shift <- data.frame(
	row_value = unique_row_values,
	shift = seq_along(unique_row_values) - 1
)

for (i in seq(1, nrow(downward_shift))) {
	target_row <- downward_shift$row_value[i]
	current_values <- image_xy_coords_downsampled[image_xy_coords_downsampled[, "row"] == target_row, "row"]
	image_xy_coords_downsampled[image_xy_coords_downsampled[, "row"] == target_row, "row"] <- current_values - downward_shift$shift[i] * (downsample / 8)
}

ggplot(image_xy_coords_downsampled, aes(col, row)) +
	geom_point() +
	coord_fixed() +
	labs(x = NULL, y = NULL) +
	theme_bw()

diff <- nrow(image_xy_coords_downsampled) - length(sticker_files)
if (diff > 0) {
	message(sprintf("More points than stickers. Need to duplicate %s stickers or downsample more...", abs(diff)))
} else if (diff < 0) {
	message(sprintf("More stickers than points. Need to exclude %s stickers or downsample less...", abs(diff)))
} else {
	message("Bang on! Number of points matches number of stickers!")
}

# order coordinates by x then y, then
image_xy_coords_ordered <- image_xy_coords_downsampled %>%
	arrange(row, col) %>%
	arrange(row+col) %>%
	mutate(index = row_number())


ggplot(image_xy_coords_ordered, aes(col, row)) +
	geom_point(aes(colour = index), size = 2, show.legend = FALSE) +
	coord_fixed() +
	labs(x = NULL, y = NULL) +
	theme_bw()

# order coordinates by sum of xy coordinates
# this should result in coordinates ordered as follows
#
# 4
# 2 5
# 1 3 6

sticker_ordered <- read.table("cache/sticker_colours_ordered.txt", header = TRUE)
head(sticker_ordered)

if (reverse) {
  sticker_ordered <- sticker_ordered[rev(seq_len(nrow(sticker_ordered))),]
}

sticker_files_pool <- rep(sprintf("img/stickers_cropped/%s.png", sticker_ordered$sticker), 1 + nrow(image_xy_coords_downsampled) %/% length(sticker_files))
sticker_files_pool <- sticker_files_pool[1:nrow(image_xy_coords_ordered)]

final_df <- data.frame(
	image_xy_coords_ordered,
	image = sticker_files_pool[1:nrow(image_xy_coords_downsampled)]
)

gg <- ggplot(final_df, aes(col, row)) +
	geom_image(aes(image = image), size = 0.0215) +
	coord_fixed() +
	labs(x = NULL, y = NULL) +
	theme_void() +
	theme(
		plot.background = element_rect(fill = "white", color = NA)
	)

n_stickers <- nrow(sticker_ordered)
n_hex <- nrow(final_df)

ggsave(sprintf("img/outputs/biochexwall-%i-stickers-%i-hex.png", n_stickers, n_hex), gg, width = 20, height = 20, dpi = 600)

rm(list = ls())
