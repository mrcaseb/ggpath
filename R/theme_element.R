#' Theme Elements for Image Grobs
#'
#' @description
#' In conjunction with the [ggplot2::theme()] system, the `element_` functions
#' specify the display of how non-data components of a ggplot are drawn. Both
#' functions call [magick::image_read()] to process image files from valid image
#' URLs, local paths, raster objects, or bitmap arrays.
#'
#'   - `element_path()`: draws images as replacement for [ggplot2::element_text()].
#'   Use this to replace text with images.
#'   - `element_raster()`: draws images as replacement for [ggplot2::element_rect()].
#'   Use this to put images in plot background.
#'
#' @inheritParams grid::rasterGrob
#' @param alpha The alpha channel, i.e. transparency level, as a numerical value
#'   between 0 and 1. `1L` skips alpha channel modification for more speed.
#' @param colour,color The image will be colorized with this color. Defaults to
#' `NA_character_` which means no change of color at all. Use the
#'   special character `"b/w"` to set it to black and white. For more information
#'   on valid color names in ggplot2 see
#'   <https://ggplot2.tidyverse.org/articles/ggplot2-specs.html?q=colour#colour-and-fill>.
#' @param angle The angle of the element as a numerical value between 0° and 360°.
#' @param size The output grob size as a [grid::unit]. If given a numeric,
#'  `cm` will be applied as unit.
#' @param image_path A file path, url, raster object or bitmap array.
#' See [magick::image_read()] for further information.
#'
#' @seealso [geom_from_path()], [grid::rasterGrob()], [grid::unit()], [magick::image_read()]
#' @name element_path
#' @aliases element_raster
#' @return An S7 object of class `element`.
#' @examples
#' library(ggplot2)
#' library(ggpath)
#'
#' # compute paths of R logo file and background image file shipped with ggpath
#' local_r_logo <- system.file("r_logo.png", package = "ggpath")
#' local_background_image <- system.file("example_bg.jpg", package = "ggpath")
#'
#' # create dataframe with x-y-coordinates and the above local path
#' plot_data <- data.frame(x = c(-1, 1), y = 1, path = local_r_logo)
#'
#' # Replace title, subtitle, the caption, axis labels as well as y-axis text
#' # the the local image
#' ggplot(plot_data, aes(x = x, y = local_r_logo)) +
#'   theme_minimal() +
#'   labs(
#'     title = local_r_logo,
#'     subtitle = local_r_logo,
#'     x = local_r_logo,
#'     y = local_r_logo,
#'     caption = local_r_logo
#'   ) +
#'   theme(
#'     plot.caption = element_path(hjust = 1, size = 0.6),
#'     axis.text.y.left = element_path(size = 1),
#'     axis.title.x = element_path(),
#'     axis.title.y = element_path(vjust = 0.9),
#'     plot.title = element_path(hjust = 0, size = 2, alpha = 0.5),
#'     plot.subtitle = element_path(hjust = 0.9, angle = 45),
#'   )
#'
#' # Use local image as plot background
#' ggplot(plot_data, aes(x = x, y = y)) +
#'   geom_from_path(aes(path = path), width = 0.2) +
#'   coord_cartesian(xlim = c(-2, 2)) +
#'   theme_dark() +
#'   theme(
#'     plot.background = element_raster(local_background_image),
#'     panel.background = element_rect(fill = "transparent")
#'   )
#' @details
#' To be able to use the functions correctly, a basic understanding of how they
#' work is required.
#'
#' **`element_path()`** can be applied wherever [ggplot2::element_text()] is
#' usually used. It replaces text with an image if the text is a valid image
#' file location or data.
#'
#' **`element_raster()`** can be applied wherever [ggplot2::element_rect()] is
#' usually used. A path in the sense of [magick::image_read()] must be explicitly
#' specified here because it cannot read plot data. It is designed exclusively
#' for inserting an image into the background of a plot and calls
#' [grid::rasterGrob()] internally.
#' Neither `width` nor `height` need to be specified, in which case, the aspect
#' ratio of the image is preserved. If both `width` and `height` are specified,
#' it is likely that the image will be distorted.
#'
NULL

s7_unit_class <- S7::new_S3_class(class(grid::unit(1, "npc")))

#' @export
#' @rdname element_path
element_path <- S7::new_class(
  "element_path",
  parent = element_text,
  properties = list(
    alpha = S7::new_property(S7::class_numeric),
    colour = S7::new_property(S7::class_character),
    hjust = S7::new_property(S7::class_numeric),
    vjust = S7::new_property(S7::class_numeric),
    angle = S7::new_property(S7::class_numeric),
    size = S7::new_property(S7::class_numeric | s7_unit_class)
  ),
  constructor = function(
    alpha = 1L,
    colour = NA_character_,
    hjust = 0.5,
    vjust = 0.5,
    color = NULL,
    angle = 0,
    size = grid::unit(0.5, "cm")
  ) {
    obj <-
      S7::new_object(
        S7::S7_object(),
        alpha = alpha,
        colour = color %||% colour,
        hjust = hjust,
        vjust = vjust,
        angle = angle,
        size = if (grid::is.unit(size)) size else grid::unit(size, "cm"),
        inherit.blank = FALSE
      )
    class(obj) <- c(
      "ggpath::element_path",
      "ggplot2::element_text",
      "ggplot2::element",
      "S7_object"
    )
    obj
  }
)

#' @export
#' @rdname element_path
element_raster <- S7::new_class(
  "element_raster",
  parent = element,
  properties = list(
    image_path = S7::new_property(S7::class_character),
    x = S7::new_property(S7::class_numeric | s7_unit_class),
    y = S7::new_property(S7::class_numeric | s7_unit_class),
    width = S7::new_property(S7::class_numeric | s7_unit_class),
    height = S7::new_property(S7::class_numeric | s7_unit_class),
    just = S7::new_property(S7::class_character),
    hjust = S7::new_property(S7::class_numeric),
    vjust = S7::new_property(S7::class_numeric),
    interpolate = S7::new_property(S7::class_logical)
  ),
  constructor = function(
    image_path,
    x = grid::unit(0.5, "npc"),
    y = grid::unit(0.5, "npc"),
    width = grid::unit(1, "npc"),
    height = grid::unit(1, "npc"),
    just = "centre",
    hjust = 0.5,
    vjust = 0.5,
    interpolate = TRUE
  ) {
    obj <-
      S7::new_object(
        S7::S7_object(),
        image_path = image_path,
        x = x,
        y = y,
        width = width,
        height = height,
        just = just,
        hjust = hjust,
        vjust = vjust,
        interpolate = interpolate
      )
    class(obj) <- c(
      "ggpath::element_raster",
      "ggplot2::element_rect",
      "ggplot2::element",
      "S7_object"
    )
    obj
  }
)

S7::method(draw_element, element_path) <- function(
  element,
  label = "",
  x = NULL,
  y = NULL,
  alpha = NULL,
  colour = NULL,
  hjust = 0.5,
  vjust = 0.5,
  angle = 0,
  size = grid::unit(0.5, "cm"),
  ...
) {
  if (is.null(label)) return(ggplot2::zeroGrob())

  n <- max(length(x), length(y), 1)
  vj <- element@vjust %||% vjust
  hj <- element@hjust %||% hjust
  angle <- element@angle %||% angle
  x <- x %||% rep(hj, n)
  y <- y %||% rep(vj, n)
  alpha <- alpha %||% rep(element@alpha, n)
  colour <- colour %||% rep(element@colour, n)
  size <- element@size %||% size

  grobs <- lapply(
    seq_along(label),
    build_grobs,
    alpha = alpha,
    colour = colour,
    path = label,
    data = data.frame(
      x = as.numeric(x),
      y = as.numeric(y),
      hjust = rep(hj, n),
      vjust = rep(vj, n),
      width = rep(1, n),
      height = rep(1, n),
      angle = rep(angle, n)
    ),
    is_theme_element = TRUE
  )

  class(grobs) <- "gList"

  grid::gTree(
    gp = grid::gpar(),
    children = grobs,
    size = size,
    cl = "ggpath_element"
  )
}

#' @export
grobHeight.ggpath_element <- function(x, ...) x$size

#' @export
grobWidth.ggpath_element <- function(x, ...) x$size

S7::method(draw_element, element_raster) <- function(element, ...) {
  img <- try(reader_function(element@image_path), silent = TRUE)
  # if the path is invalid we warn the user and insert a NULL grob
  if (inherits(img, "try-error")) {
    cli::cli_warn(
      "{.pkg ggpath} failed to read an image from {.path {element@image_path}}. \\
      It will insert an empty grob instead. Here is the \\
      error message: {img}"
    )
    return(grid::nullGrob())
  }
  grid::rasterGrob(
    image = img,
    x = element@x,
    y = element@y,
    width = element@width,
    height = element@height,
    just = element@just,
    hjust = element@hjust,
    vjust = element@vjust,
    interpolate = element@interpolate,
    default.units = element@default.units
  )
}
