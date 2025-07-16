test_that("theme element works", {
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
      axis.text.y.left = element_path(size = 1),
      axis.title.x = element_path(),
      axis.title.y = element_path(vjust = 0.9),
      plot.title = element_path(hjust = 0, size = 2, alpha = 0.5),
      plot.subtitle = element_path(hjust = 0.9, angle = 45)
    )

  # It seems like vdiffr isn't handling cran = FALSE properly so I call
  # skip_on_cran() explicitly
  skip_on_cran()
  vdiffr::expect_doppelganger("p1", p1)
})

test_that("theme element works with 'b/w'", {
  library(ggplot2)

  # compute path of an R logo file shipped with ggpath
  local_image_path <- system.file("r_logo.svg", package = "ggpath")

  # create dataframe with x-y-coordinates and the above local path
  plot_data <- data.frame(x = c(-1, 1), y = 1, path = local_image_path)

  # Replace title, subtitle, the caption, axis labels as well as y-axis text
  # with the local image
  p2 <- ggplot(plot_data, aes(x = x, y = local_image_path)) +
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
      axis.text.y.left = element_path(size = 1, color = "b/w"),
      axis.title.x = element_path(),
      # apply color again but now with colour for 100% test coverage
      axis.title.y = element_path(vjust = 0.9, colour = "b/w"),
      plot.title = element_path(hjust = 0, size = 2, alpha = 0.5),
      plot.subtitle = element_path(hjust = 0.9, angle = 45)
    )

  # It seems like vdiffr isn't handling cran = FALSE properly so I call
  # skip_on_cran() explicitly
  skip_on_cran()

  # we test mac only here and add the color = "b/w" argument
  # because the reference file is created on a mac and comparison breaks
  # on other operating systems because of slightly different grey tones.
  # for code coverage we build the plot without printing it
  out <- ggplot2::build_ggplot(p2) |>
    ggplot2::gtable_ggplot()
  skip_on_os(c("windows", "linux", "solaris"))

  vdiffr::expect_doppelganger("p2", p2)
})

test_that("background element works", {
  library(ggplot2)

  # compute path of a background image file shipped with ggpath
  local_background_image <- system.file("example_bg.jpg", package = "ggpath")

  # create dataframe with x-y-coordinates and the above local path
  plot_data <- data.frame(x = c(-1, 1), y = 1)

  # Replace title, subtitle, the caption, axis labels as well as y-axis text
  # with the local image
  p3 <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_point() +
    coord_cartesian(xlim = c(-2, 2)) +
    theme_dark() +
    theme(
      plot.background = element_raster(image_path = local_background_image),
      panel.background = element_rect(fill = "transparent")
    )

  # Do above but intentionally break the path
  p4 <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_point() +
    coord_cartesian(xlim = c(-2, 2)) +
    theme_dark() +
    theme(
      plot.background = element_raster(image_path = paste0(local_background_image, "_broken_path")),
      panel.background = element_rect(fill = "transparent")
    )

  expect_warning(print(p4), "ggpath failed to read an image from")

  # It seems like vdiffr isn't handling cran = FALSE properly so I call
  # skip_on_cran() explicitly
  skip_on_cran()
  # we skip mac here because the reference file is created on windows and
  # comparison breaks on Mac.
  # Please don't ask me why I create this reference file on windows compared
  # to the mac reference above lol
  skip_on_os("mac")
  vdiffr::expect_doppelganger("p3", p3)
})
