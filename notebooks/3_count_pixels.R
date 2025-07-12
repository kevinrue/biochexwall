library(EBImage)
library(stringr)
library(ggplot2)
library(dplyr)

sticker_files <- list.files("img/stickers_cropped", full.names = TRUE)
dir.create("cache", showWarnings = FALSE)

if (file.exists("cache/count_pixels.txt")) {
	sticker_stats <- read.table("cache/count_pixels.txt", sep = "\t", header = TRUE)
}

make_data_frame <- function(image_file) {
	message("Processing ", image_file)
	img <- readImage(image_file)
	# crop within hex
	nuc_df <- expand.grid(row = 1:nrow(img), col = 1:ncol(img))
	# crop outside hex
	coords_transparent <- as.data.frame(which(img[,,4] == 0, arr.ind = TRUE))
	# nuc_df_cropped <- setdiff(nuc_df, coords_transparent)
	nuc_df_cropped <- anti_join(nuc_df, coords_transparent, by = join_by(row, col))
	# ggplot(nuc_df_cropped, aes(row, col)) +
	# 	geom_point()
	n_pixels <- nrow(nuc_df_cropped)
	data.frame(sticker = str_replace(basename(image_file), ".png", ""), pixels = n_pixels)
}

# make_vector(sticker_files[1])
sticker_files <- setdiff(sticker_files, sprintf("img/stickers_cropped/%s.png", sticker_stats$sticker))
sticker_stats <- rbind(sticker_stats, do.call("rbind", lapply(sticker_files, make_data_frame)))

sticker_stats <- sticker_stats %>%
	arrange(desc(pixels)) %>%
	mutate(sticker = factor(sticker, sticker))

write.table(sticker_stats, "cache/count_pixels.txt", sep = "\t", quote = FALSE, row.names = FALSE)

q_pixels <- quantile(sticker_stats$pixels, 0.75)

ggplot(sticker_stats, aes(sticker, pixels)) +
	geom_point() +
	geom_hline(yintercept = q_pixels, colour = "red") +
	annotate("text", x = 5, y = q_pixels, label = format(q_pixels, big.mark = ","), hjust = 0, size = 5) +
	scale_y_log10() +
	theme_light() +
	theme(
		panel.grid = element_blank(),
		axis.text.x = element_text(angle = 90, hjust = 1)
	)

write(q_pixels, file = "cache/sample_pixels.txt")
