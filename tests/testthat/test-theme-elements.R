test_that("logo element works", {
  library(ggplot2)

  # compute path of an R logo file shipped with ggpath
  local_image_path <- system.file("r_logo.svg", package = "ggpath")

  # create dataframe with x-y-coordinates and the above local path
  plot_data <- data.frame(x = c(-1, 1), y = 1, path = local_image_path)

  # Replace title, subtitle, the caption, axis labels as well as y-axis text
  # with the local image
  p1 <- ggplot(plot_data, aes(x = x, y = local_image_path)) +
    theme_minimal() +
    labs(
      title = local_image_path,
      subtitle = local_image_path,
      x = local_image_path,
      y = local_image_path,
      caption = NULL
    ) +
    theme(
      plot.caption = element_path(hjust = 1, size = 0.6),
      axis.text.y = element_path(size = 1, color = "b/w"),
      axis.title.x = element_path(),
      axis.title.y = element_path(vjust = 0.9),
      plot.title = element_path(hjust = 0, size = 2, alpha = 0.5),
      plot.subtitle = element_path(hjust = 0.9, angle = 45),
    )

  vdiffr::expect_doppelganger("p1", p1)
})
