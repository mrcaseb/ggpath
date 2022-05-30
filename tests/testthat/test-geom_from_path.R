test_that("geom from path works", {
  library(ggplot2)

  # compute path of an R logo file shipped with ggpath
  local_image_path <- system.file("r_logo.png", package = "ggpath")

  # create dataframe with x-y-coordinates and the above local path
  plot_data <- data.frame(x = c(-1, 1), y = 1, path = local_image_path)

  # plot images directly from local path and apply transparency
  p1 <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_from_path(aes(path = path), width = 0.2, alpha = 0.5) +
    coord_cartesian(xlim = c(-2, 2)) +
    theme_minimal()

  vdiffr::expect_doppelganger("p1", p1)
})

