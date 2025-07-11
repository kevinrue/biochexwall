library(ggplot2)
library(ggimage)

# draw an hexagon in ggplot
sticker_files <- list.files("img/stickers_cropped", full.names = TRUE, pattern = "\\.png$")
n_stickers <- length(sticker_files)

get_optimal_number_of_layers <- function(n_stickers, n_hex = 1, layers = 1L) {
	if (n_stickers > n_hex) {
		return(get_optimal_number_of_layers(n_stickers, n_hex + 6 * layers, layers + 1))
	} else {
		return(layers - 1)
	}
}

n_layers <- get_optimal_number_of_layers(n_stickers)

get_n_hex_first_row <- function(n_layers) {
	n_layers
}

n_hex_first_row <- get_n_hex_first_row(n_layers)

get_n_hex_mid_row <- function(n_layers) {
	return(1 + 2 * n_layers)
}
n_hex_mid_row <- get_n_hex_mid_row(n_hex_first_row)

get_n_hex_total<- function(n_stickers, n_hex = 1, layers = 1L) {
	if (n_stickers > n_hex) {
		return(get_n_hex_total(n_stickers, n_hex + 6 * layers, layers + 1))
	} else {
		return(n_hex)
	}
}

n_hex_total <- get_n_hex_total(n_stickers)

hex_size <- 1

hex_width <- sqrt(3) * hex_size
hex_height <- 2 * hex_size

x_spacing <- hex_width
y_spacing <- 3/4 * hex_height

coords <- data.frame(
	y = numeric(0),
	x = numeric(0)
)
n_stickers_left_to_add <- n_hex_total
n_stickers_next_row <- n_hex_first_row
y_next_row <- 0
first_x_next_row <- 0
top_half <- TRUE
while(n_stickers_left_to_add > 0) {
	coords_next_layer <- data.frame(
		y = y_next_row,
		x = first_x_next_row + seq(0, n_stickers_next_row - 1) * x_spacing
	)
	coords <- rbind(coords, coords_next_layer)
	n_stickers_left_to_add <- n_stickers_left_to_add - n_stickers_next_row
	n_stickers_next_row <- ifelse(top_half, n_stickers_next_row + 1, n_stickers_next_row - 1)
	first_x_next_row <- ifelse(top_half, first_x_next_row - x_spacing / 2, first_x_next_row + x_spacing / 2)
	y_next_row <- y_next_row - y_spacing
	if (n_stickers_next_row == n_hex_mid_row) {
		top_half <- FALSE
	}
}

coords$image <- "img/stickers_cropped/Bioconductor_original.png"

# gg <- ggplot(coords, aes(x, y)) +
# 	geom_point() +
# 	geom_image(aes(image = image), size = 0.055, data = coords[sample(1:nrow(coords), size = 30), ]) +
# 	theme_bw() +
# 	coord_fixed()
# ggsave("img/outputs/hexwall.bioc.sample.png", gg, width = 20, height = 20, dpi = 600)

# gg <- ggplot(coords, aes(x, y)) +
# 	geom_image(image = "img/stickers_cropped/Bioconductor_original.png", size = 0.055) +
# 	theme_void() +
# 	coord_fixed()
# ggsave("img/outputs/hexwall.bioc.png", gg, width = 20, height = 20, dpi = 600)

sticker_ordered <- read.table("cache/sticker_colours_ordered.txt", header = TRUE)
# head(sticker_ordered)

sticker_files_pool <- rep(sprintf("img/stickers_cropped/%s.png", sticker_ordered$sticker), 1 + nrow(coords) %/% length(sticker_files))
sticker_files_pool <- sticker_files_pool[seq(length(sticker_files_pool), 1, -1)]
sticker_files_pool <- sticker_files_pool[1:nrow(coords)]

final_df <- data.frame(
	coords
)
final_df$image <- sticker_files_pool

gg <- ggplot(final_df, aes(x, y)) +
	geom_image(aes(image = image), size = 0.055) +
	coord_fixed() +
	labs(x = NULL, y = NULL) +
	theme_void() +
	theme(
		plot.background = element_rect(fill = "white", color = NA)
	)
ggsave("img/outputs/hexwall-sorted-600dpi.png", gg, width = 20, height = 20, dpi = 600)
