library(TSP)
library(colorspace)
library(farver)

# hunterlab > rgb > cmy > cmyk > lab > hsv > hsb
colourspace <- "rgb"

sticker_colours_df <- read.table("cache/sticker_colour.average.txt", header = TRUE, sep = "\t")

rgb <- col2rgb(sticker_colours_df$colour)

if (colourspace == "rgb") {
	tsp <- as.TSP(dist(t(rgb)))
} else {
	message("Converting to colourspace: ", colourspace)
	rgb_matrix <- t(rgb)
	colnames(rgb_matrix) <- c("R", "G", "B")
	hsv <- convert_colour(rgb_matrix, from = "rgb", to = colourspace)
	tsp <- as.TSP(dist(hsv))
}
sol <- solve_TSP(tsp, control = list(repetitions = 1e3))

ordered_df <- sticker_colours_df[sol, ]

# lab <- convertColor(t(rgb), 'sRGB', 'Lab')
# ordered_df <- sticker_colours_df[order(lab[, 'L']), ]
# ordered_cols2 <- my_colours[order(lab[, 'L'])]

ggplot() +
	geom_tile(aes(x = 1, y = seq_along(ordered_df$colour)), fill = ordered_df$colour)

write.table(ordered_df, file = "cache/sticker_colours_ordered.txt", sep = "\t", quote = TRUE, row.names = FALSE)
