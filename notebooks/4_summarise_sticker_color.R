library(ggimage)
library(dplyr)

# One of 'average'
summary_mode <- "average"

n_pixels <- scan("cache/sample_pixels.txt", what = double())

sticker_files <- list.files("img/stickers_cropped", full.names = TRUE, pattern = "\\.png$")

# image_file <- sticker_files[1]

if (file.exists(sprintf("cache/sticker_colour.%s.txt", summary_mode))) {
	sticker_stats <- read.table(sprintf("cache/sticker_colour.%s.txt", summary_mode), sep = "\t", header = TRUE)
}

make_data_frame <- function(image_file) {
	message("Processing ", image_file)
	img = readImage(image_file)
	img_data <- imageData(img)
	img_df <- expand.grid(row = 1:nrow(img), col = 1:ncol(img))
	# crop outside hex
	coords_transparent <- as.data.frame(which(img_data[ , , 4] == 0, arr.ind = TRUE))
	img_df <- anti_join(img_df, coords_transparent, by = join_by(row, col))
	# img_df <- setdiff(img_df, coords_transparent)
	if (nrow(img_df) > n_pixels) {
		img_df <- img_df[round(seq(1, nrow(img_df), length.out = n_pixels)), ]
	}
	img_df[, "R"] <- mapply(function(row, col) img_data[row, col, 1], row = img_df$row, col = img_df$col)
	img_df[, "G"] <- mapply(function(row, col) img_data[row, col, 2], row = img_df$row, col = img_df$col)
	img_df[, "B"] <- mapply(function(row, col) img_data[row, col, 3], row = img_df$row, col = img_df$col)
	mean_colour <- colMeans(img_df[, c("R", "G", "B")])
	rgb <- rgb(mean_colour[1], mean_colour[2], mean_colour[3])
	data.frame(sticker = str_replace(basename(image_file), ".png", ""), colour = rgb)
}

sticker_files <- setdiff(sticker_files, sprintf("img/stickers_cropped/%s.png", sticker_stats$sticker))
sticker_stats <- rbind(sticker_stats, do.call("rbind", lapply(sticker_files, make_data_frame)))

sticker_stats <- do.call("rbind", lapply(sticker_files, make_data_frame))

write.table(sticker_stats, sprintf("cache/sticker_colour.%s.txt", summary_mode), sep = "\t", quote = TRUE, row.names = FALSE)
