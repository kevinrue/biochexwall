biocstickers_readme <- readLines("https://raw.githubusercontent.com/Bioconductor/BiocStickers/master/README.md")
library(stringr)

urls <- grep("img src=.*.png", biocstickers_readme, value = TRUE) |>
  (function(.) gsub(".*img src=\"", "", .))() |>
  (function(.) gsub("\".*", "", .))() |>
  (function(.) paste0("https://raw.githubusercontent.com/Bioconductor/BiocStickers/master/", .))()

urls <- grep("/master/events/", urls, invert = TRUE, value = TRUE)
urls <- grep("/master/Bioconductor/", urls, invert = TRUE, value = TRUE)
urls <- grep("/master/boards/", urls, invert = TRUE, value = TRUE)
names(urls) <- basename(dirname(urls))

url_exists <- RCurl::url.exists(file.path("https://bioconductor.org/packages/", names(urls)))
names(url_exists) <- names(urls)

table(url_exists)
