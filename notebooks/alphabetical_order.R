biocstickers_readme <- readLines("https://raw.githubusercontent.com/Bioconductor/BiocStickers/master/README.md")
library(stringr)

urls <- grep("img src=.*.png", biocstickers_readme, value = TRUE) |>
	(function(.) gsub(".*img src=\"", "", .))() |>
	(function(.) gsub("\".*", "", .))() |>
	(function(.) paste0("https://raw.githubusercontent.com/Bioconductor/BiocStickers/master/", .))()

cat(sort(basename(urls)), sep = "\n")
