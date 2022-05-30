
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggpath

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/ggpath)](https://CRAN.R-project.org/package=ggpath)
[![Codecov test
coverage](https://codecov.io/gh/mrcaseb/ggpath/branch/main/graph/badge.svg)](https://app.codecov.io/gh/mrcaseb/ggpath?branch=main)
[![R-CMD-check](https://github.com/mrcaseb/ggpath/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mrcaseb/ggpath/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

ggpath is a ‘ggplot2’ extension that enables robust image grobs in
panels and theme elements. This means it helps plotting images (from
local paths, from urls or from raw image data) in nearly every part of a
ggplot.

## Installation

Until ggpath is available on CRAN, the only way to install it is from
[GitHub](https://github.com/mrcaseb/ggpath), for example with:

``` r
if (!require("pak")) install.packages("pak")
pak::pak("mrcaseb/ggpath")
```

## Example

The two main features to provide images in a ggplot are a geom
(geom_from_path) and a theme element (element_path). Both replace image
urls, local image paths, or raw image data with the actual image. And to
improve performance, the images are cached locally.

The below examples use local image files that are shipped with the
package. Let’s locate the images first.

``` r
local_image_path <- system.file("r_logo.png", package = "ggpath")
```

Now, we can make a simple plot, where we use the image like a point by
replacing the local path with the actual image.

``` r
library(ggplot2)
library(ggpath)
plot_data <- data.frame(x = c(-1, 1), y = 1, path = local_image_path)
ggplot(plot_data, aes(x = x, y = y)) +
  geom_from_path(aes(path = path), width = 0.2) +
  coord_cartesian(xlim = c(-2, 2)) +
  theme_minimal()
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

We can build on top of that by adding new axis labels, axis titles, plot
title and subtitle, or a caption and using the ggpath theme element.
Note the usage of transparency with the `alpha` argument, the
justification with the `hjust`/`vjust` arguments, or the rotation with
the `angle` argument.

``` r
ggplot(plot_data, aes(x = x, y = local_image_path)) +
  geom_from_path(aes(path = path), width = 0.2, alpha = 0.2) +
  coord_cartesian(xlim = c(-2, 2)) +
  theme_minimal() +
  labs(
    title = local_image_path,
    subtitle = local_image_path,
    x = local_image_path,
    y = local_image_path,
    caption = local_image_path
  ) +
  theme(
    plot.caption = element_path(hjust = 1, size = 0.6),
    axis.text.y = element_path(size = 1),
    axis.title.x = element_path(),
    axis.title.y = element_path(vjust = 0.9),
    plot.title = element_path(hjust = 0, size = 2, alpha = 0.5),
    plot.subtitle = element_path(hjust = 0.9, angle = 45),
  )
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />
