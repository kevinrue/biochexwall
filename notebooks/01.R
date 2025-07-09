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

