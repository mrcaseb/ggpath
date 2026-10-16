# Theme Elements for Image Grobs

In conjunction with the
[`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html)
system, the `element_` functions specify the display of how non-data
components of a ggplot are drawn. Both functions call
[`magick::image_read()`](https://docs.ropensci.org/magick/reference/editing.html)
to process image files from valid image URLs, local paths, raster
objects, or bitmap arrays.

- `element_path()`: draws images as replacement for
  [`ggplot2::element_text()`](https://ggplot2.tidyverse.org/reference/element.html).
  Use this to replace text with images.

- `element_raster()`: draws images as replacement for
  [`ggplot2::element_rect()`](https://ggplot2.tidyverse.org/reference/element.html).
  Use this to put images in plot background.

## Usage

``` r
element_path(
  alpha = 1L,
  colour = NA_character_,
  hjust = 0.5,
  vjust = 0.5,
  color = NULL,
  angle = 0,
  size = grid::unit(0.5, "cm")
)

element_raster(
  image_path,
  x = grid::unit(0.5, "npc"),
  y = grid::unit(0.5, "npc"),
  width = grid::unit(1, "npc"),
  height = grid::unit(1, "npc"),
  just = "centre",
  hjust = 0.5,
  vjust = 0.5,
  interpolate = TRUE
)
```

## Arguments

- alpha:

  The alpha channel, i.e. transparency level, as a numerical value
  between 0 and 1. `1L` skips alpha channel modification for more speed.

- colour, color:

  The image will be colorized with this color. Defaults to
  `NA_character_` which means no change of color at all. Use the special
  character `"b/w"` to set it to black and white. For more information
  on valid color names in ggplot2 see
  <https://ggplot2.tidyverse.org/articles/ggplot2-specs.html?q=colour#colour-and-fill>.

- hjust:

  A numeric vector specifying horizontal justification. If specified,
  overrides the `just` setting.

- vjust:

  A numeric vector specifying vertical justification. If specified,
  overrides the `just` setting.

- angle:

  The angle of the element as a numerical value between 0° and 360°.

- size:

  The output grob size as a
  [grid::unit](https://rdrr.io/r/grid/unit.html). If given a numeric,
  `cm` will be applied as unit. Stored in the `image_size` property so
  the inherited text `size` property retains the numeric type required
  by
  [`ggplot2::element_text()`](https://ggplot2.tidyverse.org/reference/element.html).

- image_path:

  A file path, url, raster object or bitmap array. See
  [`magick::image_read()`](https://docs.ropensci.org/magick/reference/editing.html)
  for further information.

- x:

  A numeric vector or unit object specifying x-location.

- y:

  A numeric vector or unit object specifying y-location.

- width:

  A numeric vector or unit object specifying width.

- height:

  A numeric vector or unit object specifying height.

- just:

  The justification of the rectangle relative to its (x, y) location. If
  there are two values, the first value specifies horizontal
  justification and the second value specifies vertical justification.
  Possible string values are: `"left"`, `"right"`, `"centre"`,
  `"center"`, `"bottom"`, and `"top"`. For numeric values, 0 means left
  alignment and 1 means right alignment.

- interpolate:

  A logical value indicating whether to linearly interpolate the image
  (the alternative is to use nearest-neighbour interpolation, which
  gives a more blocky result).

## Value

An S7 object of class `element`.

## Details

To be able to use the functions correctly, a basic understanding of how
they work is required.

**`element_path()`** can be applied wherever
[`ggplot2::element_text()`](https://ggplot2.tidyverse.org/reference/element.html)
is usually used. It replaces text with an image if the text is a valid
image file location or data.

**`element_raster()`** can be applied wherever
[`ggplot2::element_rect()`](https://ggplot2.tidyverse.org/reference/element.html)
is usually used. A path in the sense of
[`magick::image_read()`](https://docs.ropensci.org/magick/reference/editing.html)
must be explicitly specified here because it cannot read plot data. It
is designed exclusively for inserting an image into the background of a
plot and calls
[`grid::rasterGrob()`](https://rdrr.io/r/grid/grid.raster.html)
internally. Neither `width` nor `height` need to be specified, in which
case, the aspect ratio of the image is preserved. If both `width` and
`height` are specified, it is likely that the image will be distorted.

## See also

[`geom_from_path()`](https://mrcaseb.github.io/ggpath/reference/geom_from_path.md),
[`grid::rasterGrob()`](https://rdrr.io/r/grid/grid.raster.html),
[`grid::unit()`](https://rdrr.io/r/grid/unit.html),
[`magick::image_read()`](https://docs.ropensci.org/magick/reference/editing.html)

## Examples

``` r
library(ggplot2)
library(ggpath)

# compute paths of R logo file and background image file shipped with ggpath
local_r_logo <- system.file("r_logo.png", package = "ggpath")
local_background_image <- system.file("example_bg.jpg", package = "ggpath")

# create dataframe with x-y-coordinates and the above local path
plot_data <- data.frame(x = c(-1, 1), y = 1, path = local_r_logo)

# Replace title, subtitle, the caption, axis labels as well as y-axis text
# the the local image
ggplot(plot_data, aes(x = x, y = local_r_logo)) +
  theme_minimal() +
  labs(
    title = local_r_logo,
    subtitle = local_r_logo,
    x = local_r_logo,
    y = local_r_logo,
    caption = local_r_logo
  ) +
  theme(
    plot.caption = element_path(hjust = 1, size = 0.6),
    axis.text.y.left = element_path(size = 1),
    axis.title.x = element_path(),
    axis.title.y = element_path(vjust = 0.9),
    plot.title = element_path(hjust = 0, size = 2, alpha = 0.5),
    plot.subtitle = element_path(hjust = 0.9, angle = 45),
  )


# Use local image as plot background
ggplot(plot_data, aes(x = x, y = y)) +
  geom_from_path(aes(path = path), width = 0.2) +
  coord_cartesian(xlim = c(-2, 2)) +
  theme_dark() +
  theme(
    plot.background = element_raster(local_background_image),
    panel.background = element_rect(fill = "transparent")
  )
```
