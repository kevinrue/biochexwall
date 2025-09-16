library(ggplot2)
library(ggimage)

hex_size <- 1

hex_width <- sqrt(3) * hex_size
hex_height <- 2 * hex_size

x_spacing <- hex_width
y_spacing <- 3/4 * hex_height

final_df <- data.frame(
	x = numeric(0),
	y = numeric(0),
	image = character(0)
)

test_sticker <- "img/stickers_cropped/Bioconductor_original.png"

## row 1 ----
y_next_row <- 0
first_x_next_row <- 0
n_stickers_next_row <- 1
final_df <- rbind(
	final_df,
	data.frame(
		x = y_next_row,
		y = first_x_next_row,
		image = test_sticker
	)
)

## row 2 ----
y_next_row <- y_next_row - y_spacing
add_sticker <- TRUE
if (add_sticker) {
	n_stickers_next_row <- n_stickers_next_row + 1
	first_x_next_row <- first_x_next_row - x_spacing / 2
} else {
	n_stickers_next_row <- n_stickers_next_row - 1
	first_x_next_row <- first_x_next_row + x_spacing / 2
}
coords_next_row <- data.frame(
	y = y_next_row,
	x = first_x_next_row + seq(0, n_stickers_next_row - 1) * x_spacing,
	image = test_sticker
)
final_df <- rbind(
	final_df,
	coords_next_row
)

## row 3 ----
y_next_row <- y_next_row - y_spacing
add_sticker <- TRUE
if (add_sticker) {
	n_stickers_next_row <- n_stickers_next_row + 1
	first_x_next_row <- first_x_next_row - x_spacing / 2
} else {
	n_stickers_next_row <- n_stickers_next_row - 1
	first_x_next_row <- first_x_next_row + x_spacing / 2
}
coords_next_row <- data.frame(
	y = y_next_row,
	x = first_x_next_row + seq(0, n_stickers_next_row - 1) * x_spacing,
	image = test_sticker
)
final_df <- rbind(
	final_df,
	coords_next_row
)

## row 4 ----
y_next_row <- y_next_row - y_spacing
add_sticker <- TRUE
if (add_sticker) {
	n_stickers_next_row <- n_stickers_next_row + 1
	first_x_next_row <- first_x_next_row - x_spacing / 2
} else {
	n_stickers_next_row <- n_stickers_next_row - 1
	first_x_next_row <- first_x_next_row + x_spacing / 2
}
coords_next_row <- data.frame(
	y = y_next_row,
	x = first_x_next_row + seq(0, n_stickers_next_row - 1) * x_spacing,
	image = test_sticker
)
final_df <- rbind(
	final_df,
	coords_next_row
)

## row 5 ----
y_next_row <- y_next_row - y_spacing
add_sticker <- FALSE
if (add_sticker) {
	n_stickers_next_row <- n_stickers_next_row + 1
	first_x_next_row <- first_x_next_row - x_spacing / 2
} else {
	n_stickers_next_row <- n_stickers_next_row - 1
	first_x_next_row <- first_x_next_row + x_spacing / 2
}
coords_next_row <- data.frame(
	y = y_next_row,
	x = first_x_next_row + seq(0, n_stickers_next_row - 1) * x_spacing,
	image = test_sticker
)
final_df <- rbind(
	final_df,
	coords_next_row
)

## row 6 ----
y_next_row <- y_next_row - y_spacing
add_sticker <- TRUE
if (add_sticker) {
	n_stickers_next_row <- n_stickers_next_row + 1
	first_x_next_row <- first_x_next_row - x_spacing / 2
} else {
	n_stickers_next_row <- n_stickers_next_row - 1
	first_x_next_row <- first_x_next_row + x_spacing / 2
}
coords_next_row <- data.frame(
	y = y_next_row,
	x = first_x_next_row + seq(0, n_stickers_next_row - 1) * x_spacing,
	image = test_sticker
)
final_df <- rbind(
	final_df,
	coords_next_row
)

## row 7 ----
y_next_row <- y_next_row - y_spacing
add_sticker <- FALSE
if (add_sticker) {
	n_stickers_next_row <- n_stickers_next_row + 1
	first_x_next_row <- first_x_next_row - x_spacing / 2
} else {
	n_stickers_next_row <- n_stickers_next_row - 1
	first_x_next_row <- first_x_next_row + x_spacing / 2
}
coords_next_row <- data.frame(
	y = y_next_row,
	x = first_x_next_row + seq(0, n_stickers_next_row - 1) * x_spacing,
	image = test_sticker
)
final_df <- rbind(
	final_df,
	coords_next_row
)

## row 8 ----
y_next_row <- y_next_row - y_spacing
add_sticker <- TRUE
if (add_sticker) {
	n_stickers_next_row <- n_stickers_next_row + 1
	first_x_next_row <- first_x_next_row - x_spacing / 2
} else {
	n_stickers_next_row <- n_stickers_next_row - 1
	first_x_next_row <- first_x_next_row + x_spacing / 2
}
coords_next_row <- data.frame(
	y = y_next_row,
	x = first_x_next_row + seq(0, n_stickers_next_row - 1) * x_spacing,
	image = test_sticker
)
final_df <- rbind(
	final_df,
	coords_next_row
)

## row 9 ----
y_next_row <- y_next_row - y_spacing
add_sticker <- FALSE
if (add_sticker) {
	n_stickers_next_row <- n_stickers_next_row + 1
	first_x_next_row <- first_x_next_row - x_spacing / 2
} else {
	n_stickers_next_row <- n_stickers_next_row - 1
	first_x_next_row <- first_x_next_row + x_spacing / 2
}
coords_next_row <- data.frame(
	y = y_next_row,
	x = first_x_next_row + seq(0, n_stickers_next_row - 1) * x_spacing,
	image = test_sticker
)
final_df <- rbind(
	final_df,
	coords_next_row
)

## row 10 ----
y_next_row <- y_next_row - y_spacing
add_sticker <- FALSE
if (add_sticker) {
	n_stickers_next_row <- n_stickers_next_row + 1
	first_x_next_row <- first_x_next_row - x_spacing / 2
} else {
	n_stickers_next_row <- n_stickers_next_row - 1
	first_x_next_row <- first_x_next_row + x_spacing / 2
}
coords_next_row <- data.frame(
	y = y_next_row,
	x = first_x_next_row + seq(0, n_stickers_next_row - 1) * x_spacing,
	image = test_sticker
)
final_df <- rbind(
	final_df,
	coords_next_row
)

## row 11 ----
y_next_row <- y_next_row - y_spacing
add_sticker <- FALSE
if (add_sticker) {
	n_stickers_next_row <- n_stickers_next_row + 1
	first_x_next_row <- first_x_next_row - x_spacing / 2
} else {
	n_stickers_next_row <- n_stickers_next_row - 1
	first_x_next_row <- first_x_next_row + x_spacing / 2
}
coords_next_row <- data.frame(
	y = y_next_row,
	x = first_x_next_row + seq(0, n_stickers_next_row - 1) * x_spacing,
	image = test_sticker
)
final_df <- rbind(
	final_df,
	coords_next_row
)

ggplot(final_df, aes(x, y)) +
	geom_image(aes(image = image), size = 0.08) +
	coord_fixed(
		xlim = c(min(final_df$x - hex_width), max(final_df$x + hex_width)),
		ylim = c(min(final_df$y - hex_height), max(final_df$y + hex_height)),
		clip = "off"
	) +
	labs(x = NULL, y = NULL) +
	theme_void() +
	theme(
		plot.background = element_rect(fill = "white", color = NA)
	)

gg <- .Last.value

ggsave(sprintf("img/outputs/hexwall-pointy.png"), gg, width = 20, height = 20, dpi = 600)
