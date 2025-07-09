gg <- ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
  geom_point() +
  labs(x = NULL, y = NULL) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA)
  )

gg

ggsave("img/iris_scatter_plot.png", gg, width = 10, height = 10, dpi = 300)
