
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggpath

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/ggpath)](https://CRAN.R-project.org/package=ggpath)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/grand-total/ggpath)](https://CRAN.R-project.org/package=ggpath)
[![Dev
status](https://img.shields.io/github/r-package/v/mrcaseb/ggpath/main?label=dev%20version&style=flat-square&logo=github)](https://mrcaseb.github.io/ggpath/)
[![R-CMD-check](https://img.shields.io/github/actions/workflow/status/mrcaseb/ggpath/R-CMD-check.yaml?label=R%20check&style=flat-square&logo=github)](https://github.com/mrcaseb/ggpath/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Codecov test
coverage](https://codecov.io/gh/mrcaseb/ggpath/graph/badge.svg)](https://app.codecov.io/gh/mrcaseb/ggpath)
<!-- badges: end -->

ggpath is a ‘ggplot2’ extension that enables robust image grobs in
panels and theme elements. This means it helps plotting images (from
local paths, from urls or from raw image data) in nearly every part of a
ggplot.

## Installation

The easiest way to get ggpath is to install it from
[CRAN](https://cran.r-project.org/package=ggpath) with:

``` r
install.packages("ggpath")
```

To get a bug fix or to use a feature from the development version, you
can install the development version of ggpath from
[GitHub](https://github.com/mrcaseb/ggpath), for example with:

``` r
if (!require("pak")) install.packages("pak")
pak::pkg_install("mrcaseb/ggpath")
```

## Examples

The two main features to provide images in a ggplot are a geom
(`geom_from_path()`) and theme elements (`element_path()` &
`element_raster()`). All of them replace image urls, local image paths,
or raw image data with the actual image. And to improve performance, the
images are cached locally.

The below examples use local image files that are shipped with the
package. Let’s locate the images first.

``` r
r_logo <- "https://cran.r-project.org/Rlogo.svg"
local_background_image <- system.file("example_bg.jpg", package = "ggpath")
```

### Image Geom

Now, we can make a simple plot, where we use the logo image like a point
by replacing the local path with the actual image.

``` r
library(ggplot2)
library(ggpath)
plot_data <- data.frame(x = c(-1, 1), y = 1, path = r_logo)
ggplot(plot_data, aes(x = x, y = y)) +
  geom_from_path(aes(path = path), width = 0.2) +
  coord_cartesian(xlim = c(-2, 2)) +
  theme_minimal()
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

### Images in Theme Elements (element_path)

We can build on top of that by adding new axis labels, axis titles, plot
title and subtitle, or a caption and using a ggpath theme element. Note
the usage of transparency with the `alpha` argument, the justification
with the `hjust`/`vjust` arguments, or the rotation with the `angle`
argument.

``` r
ggplot(plot_data, aes(x = x, y = r_logo)) +
  geom_from_path(aes(path = path), width = 0.2, alpha = 0.2) +
  coord_cartesian(xlim = c(-2, 2)) +
  theme_minimal() +
  labs(
    title = r_logo,
    subtitle = r_logo,
    x = r_logo,
    y = r_logo,
    caption = r_logo
  ) +
  theme(
    plot.caption = element_path(hjust = 1, size = grid::unit(4, "lines")),
    axis.text.y.left = element_path(size = 1, alpha = 0.4),
    axis.title.x = element_path(),
    axis.title.y = element_path(vjust = 0.9),
    plot.title = element_path(hjust = 0, size = 2, alpha = 0.5),
    plot.subtitle = element_path(hjust = 0.9, angle = 45)
  )
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

A popular way to personalize a plot is to include a logo in the title
area. As shown above, we can replace the title or subtitle with an image
but not combine it with text. So if we want a title, a subtitle and
still a logo in the title area, we can use the ggplot2 tag, which is
actually used to implement figure numbering.

``` r
ggplot(plot_data, aes(x = x, y = 1)) +
  geom_from_path(aes(path = path), width = 0.2, alpha = 0.2) +
  coord_cartesian(xlim = c(-2, 2)) +
  theme_minimal() +
  labs(
    title = "This is a very catchy title",
    subtitle = "And an informative subtitle",
    x = "x axis label",
    y = "y axis label",
    caption = "useful caption",
    tag = r_logo
  ) +
  theme(
    plot.tag = element_path(size = 2, vjust = 1, alpha = 0.7),
    plot.tag.position = c(0.3,1),
  )
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

Please note how to place the image in the whole plot area via
`plot.tag.position`. So in combination with alpha you can place a logo
also behind title and subtitle.

### Images in Theme Elements (element_raster)

The second theme element, `element_raster()`, allows rendering of images
in the plot background. It is a replacement for
`ggplot2::element_rect()`. In the following example, we plot the two
logos again and now set a sample background. The sample background is a
photo by Dan Cristian Pădureț on Unsplash.

``` r
ggplot(plot_data, aes(x = x, y = y)) +
  geom_from_path(aes(path = path), width = 0.2) +
  coord_cartesian(xlim = c(-2, 2)) +
  theme_dark() +
  theme(
    plot.background = element_raster(image_path = local_background_image),
    panel.background = element_rect(fill = "transparent")
  )
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

Some notes on the plot and the general behaviour

- We remove the panel background to be able to see the plot background
  by setting it’s `fill` parameter to “transparent”.
- `element_raster()` defaults to plot the image to 100% of the plot
  width and height (`grid::unit(1, "npc")`). This means that it might
  change the aspect ratio of the image if it doesn’t equal the aspect
  ratio of the actual plot.
- `element_raster()` defaults to plot the image exactly in the middle of
  the plot (`grid::unit(0.5, "npc")` combined with `just = "centre"`).
  This means you can move around the image if you set it’s size bigger
  than the plot, e.g. with `height = grid::unit(2, "npc")`. See
  `help("unit", "grid")` for further information.

## ggpath Options

The option `"ggpath.cache"` can be used to configure the package cache.
It can be set with

``` r
options(ggpath.cache = "memory")
# or
options(ggpath.cache = "filesystem")
# or
options(ggpath.cache = "off")
```

The default - `"memory"` - caches in the current session, while
`"filesystem"` caches on disk which means that the cache is available
after starting a fresh session. All cache options time out after 24
hours.

## Comparison with Similar Image-Plotting Packages

There are various ggplot2 extensions that provide similar functionality
in terms of plotting images. These include but not limited to

- [ggimage](https://cran.r-project.org/package=ggimage)
- [ggpp](https://docs.r4photobiology.info/ggpp/)
- [ggsvg](https://coolbutuseless.github.io/package/ggsvg/)
- [ggtext](https://wilkelab.org/ggtext/)

ggpath combines the strengths of all of the above by providing

- functions to plot images in both the panel (with
  [`geom_from_path`](https://mrcaseb.github.io/ggpath/reference/geom_from_path.html))
  and all other plot areas (with [`element_path` &
  `element_raster`](https://mrcaseb.github.io/ggpath/reference/theme_elements.html)),
- robust image aspect ratio,
- options for changing the color of images including a grayscale
  transformation,
- options for applying transparency, and
- improved performance through image caching.

There are some downsides compared to the above mentioned packages, e.g.

- cannot combine images and text to a grob as ggtext can with
  [`element_markdown`](https://wilkelab.org/ggtext/reference/element_markdown.html),
- cannot modify css parts of svgs as [ggsvg
  can](https://github.com/coolbutuseless/ggsvg#styling-svg-with-css-aesthetics-example-1).
