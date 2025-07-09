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

heatmap(t(image_data)[seq(nrow(image_data), 1, -1), ], 
      Rowv = NA, 
      Colv = NA, 
      col = gray.colors(256), 
      scale = "none", 
      xlab = "Columns", 
      ylab = "Rows", 
      main = "Heatmap of Image Data")
