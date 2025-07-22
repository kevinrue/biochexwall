sticker_files <- list.files("img/stickers/", full.names = TRUE)
file_sizes <- file.size(sticker_files)
names(file_sizes) <- sticker_files
file_sizes <- sort(file_sizes, decreasing = TRUE)

file_size_formatted <- utils:::format.object_size(file_sizes, standard = "SI", units = "MB", digits = 3)
names(file_size_formatted) <- names(file_sizes)

head(file_size_formatted, n = 13)

plot(seq_along(file_sizes), file_sizes)
abline(h =500E3)
abline(h =663E3)

table(file_sizes > 600E3)
