test_that("geom from path works", {
  # skip this on cran because we load images from github which could fail
  skip_on_cran()

  library(ggplot2)

  # create x-y-coordinates of a pentagon and add nflverse logo urls
  df <- data.frame(
    a = sin(2 * pi * (0) / 5),
    b = cos(2 * pi * (0) / 5),
    url = c(
      "https://github.com/nflverse/nflseedR/raw/master/man/figures/logo.png"
    )
  )

  # plot images directly from url and apply transparency
  p1 <- ggplot(df, aes(x = a, y = b)) +
    geom_from_path(aes(path = url), width = 0.1, alpha = 0.5) +
    coord_cartesian(xlim = c(-2, 2), ylim = c(-1.3, 1.5)) +
    theme_void()

  vdiffr::expect_doppelganger("p1", p1)
})

