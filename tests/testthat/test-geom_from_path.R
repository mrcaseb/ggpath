test_that("geom from path works", {
  # prevents ggplot from creating a Rplots.pdf file in test directory
  if(!interactive()) pdf(NULL)

  library(ggplot2)

  # compute path of an R logo file shipped with ggpath
  local_image_path <- system.file("r_logo.png", package = "ggpath")

  # create dataframe with x-y-coordinates and the above local path
  plot_data <- data.frame(x = c(-1, 1), y = 1, path = local_image_path)

  # plot images directly from local path and apply transparency
  p1 <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_from_path(aes(path = as.factor(path)), width = 0.2, alpha = 0.5) +
    coord_cartesian(xlim = c(-2, 2)) +
    theme_minimal()

  # alpha > 1 error
  p2 <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_from_path(aes(path = path), width = 0.2, alpha = 2) +
    coord_cartesian(xlim = c(-2, 2)) +
    theme_minimal()

  # alpha < 0 error
  p3 <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_from_path(aes(path = path), width = 0.2, alpha = -1) +
    coord_cartesian(xlim = c(-2, 2)) +
    theme_minimal()

  # bad path error
  p4 <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_from_path(aes(path = paste0(path, "g")), width = 0.2, alpha = -1) +
    coord_cartesian(xlim = c(-2, 2)) +
    theme_minimal()

  expect_error(print(p2), regexp = 'all values of `alpha` have to be in range')
  expect_error(print(p3), regexp = 'all values of `alpha` have to be in range')
  expect_error(print(p4))

  # It seems like vdiffr isn't handling cran = FALSE properly so I call
  # skip_on_cran() explicitly
  skip_on_cran()
  vdiffr::expect_doppelganger("p1", p1)
})

