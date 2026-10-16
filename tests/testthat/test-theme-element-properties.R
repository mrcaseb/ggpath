test_that("element_path preserves image units when drawing", {
  path <- system.file("r_logo.png", package = "ggpath")
  for (size in list(
    0.5,
    grid::unit(2, "lines"),
    grid::unit(1, "cm") + grid::unit(1, "lines")
  )) {
    element <- element_path(size = size)
    expect_s7_class(element, ggplot2::element_text)
    grob <- ggplot2::element_grob(element, label = path)
    expected <- if (grid::is.unit(size)) size else grid::unit(size, "cm")
    expect_identical(grid::grobWidth(grob), expected)
    expect_identical(grid::grobHeight(grob), expected)
  }
})

test_that("element_raster is a rectangle element and draws local images", {
  element <- element_raster(
    image_path = system.file("r_logo.png", package = "ggpath")
  )
  expect_s7_class(element, ggplot2::element_rect)
  expect_s3_class(ggplot2::element_grob(element), "rastergrob")
})
