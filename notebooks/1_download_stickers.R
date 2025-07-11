biocstickers_readme <- readLines("https://raw.githubusercontent.com/Bioconductor/BiocStickers/master/README.md")

library(stringr)

urls <- grep("img src=.*.png", biocstickers_readme, value = TRUE) |>
  (function(.) gsub(".*img src=\"", "", .))() |>
  (function(.) gsub("\".*", "", .))() |>
  (function(.) paste0("https://raw.githubusercontent.com/Bioconductor/BiocStickers/master/", .))()

# download the images
dir.create("img/stickers", showWarnings = FALSE, recursive = TRUE)
library(httr)
for (url in urls) {
  filename <- basename(url)
  destfile <- file.path("img/stickers", filename)
  if (!file.exists(destfile)) {
    tryCatch({
      message(paste("Downloading", url))
      GET(url, write_disk(destfile, overwrite = TRUE))
    }, error = function(e) {
      message(paste("Failed to download", url, ":", e$message))
    })
  } else {
  	message(paste("Skipping", filename))
  }
}
