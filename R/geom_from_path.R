#' ggplot2 Layer for Visualizing Images from URLs or Local Paths
#'
#' @description This geom is used to plot images instead
#'   of points in a ggplot. It requires x, y aesthetics as well as a path.
#'
#' @inheritParams ggplot2::geom_point
#' @section Aesthetics:
#' `geom_from_path()` understands the following aesthetics (required aesthetics are in bold):
#' \describe{
#'   \item{**x**}{The x-coordinate.}
#'   \item{**y**}{The y-coordinate.}
#'   \item{**path**}{a file path, url, raster object or bitmap array. See [`magick::image_read()`] for further information.}
#'   \item{`alpha = NULL`}{The alpha channel, i.e. transparency level, as a numerical value between 0 and 1.}
#'   \item{`colour = NULL`}{The image will be colorized with this colour. Use the special character `"b/w"` to set it to black and white. For more information on valid colour names in ggplot2 see <https://ggplot2.tidyverse.org/articles/ggplot2-specs.html?q=colour#colour-and-fill>}
#'   \item{`angle = 0`}{The angle of the image as a numerical value between 0° and 360°.}
#'   \item{`hjust = 0.5`}{The horizontal adjustment relative to the given x coordinate. Must be a numerical value between 0 and 1.}
#'   \item{`vjust = 0.5`}{The vertical adjustment relative to the given y coordinate. Must be a numerical value between 0 and 1.}
#'   \item{`width = 1.0`}{The desired width of the image in `npc` (Normalised Parent Coordinates).
#'                           The default value is set to 1.0 which is *big* but it is necessary
#'                           because all used values are computed relative to the default.
#'                           A typical size is `width = 0.1` (see below examples).}
#'   \item{`height = 1.0`}{The desired height of the image in `npc` (Normalised Parent Coordinates).
#'                            The default value is set to 1.0 which is *big* but it is necessary
#'                            because all used values are computed relative to the default.
#'                            A typical size is `height = 0.1` (see below examples).}
#' }
#' @param ... Other arguments passed on to [ggplot2::layer()]. These are
#'   often aesthetics, used to set an aesthetic to a fixed value. See the below
#'   section "Aesthetics" for a full list of possible arguments.
#' @return A ggplot2 layer ([ggplot2::layer()]) that can be added to a plot
#'   created with [ggplot2::ggplot()].
#' @export
#' @examples
#' library(ggplot2)
#' library(ggpath)
#'
#' # compute path of an R logo file shipped with ggpath
#' local_image_path <- system.file("r_logo.png", package = "ggpath")
#'
#' # create dataframe with x-y-coordinates and the above local path
#' plot_data <- data.frame(x = c(-1, 1), y = 1, path = local_image_path)
#'
#' # plot images directly from local path
#' ggplot(plot_data, aes(x = x, y = y)) +
#'   geom_from_path(aes(path = path), width = 0.2) +
#'   coord_cartesian(xlim = c(-2, 2)) +
#'   theme_minimal()
#'
#' # plot images directly from local path and apply transparency
#' ggplot(plot_data, aes(x = x, y = y)) +
#'   geom_from_path(aes(path = path), width = 0.2, alpha = 0.5) +
#'   coord_cartesian(xlim = c(-2, 2)) +
#'   theme_minimal()
#'
#' # It is also possible and recommended to use the underlying Geom inside a
#' # ggplot2 annotation
#' ggplot() +
#'   annotate(
#'     ggpath::GeomFromPath,
#'     x = 0,
#'     y = 0,
#'     path = local_image_path,
#'     width = 0.4
#'   ) +
#'   theme_minimal()
geom_from_path <- function(mapping = NULL, data = NULL,
                           stat = "identity", position = "identity",
                           ...,
                           na.rm = FALSE,
                           show.legend = FALSE,
                           inherit.aes = TRUE) {

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFromPath,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggpath-package
#' @export
GeomFromPath <- ggplot2::ggproto(
  "GeomFromPath", ggplot2::Geom,
  required_aes = c("x", "y", "path"),
  # non_missing_aes = c(""),
  default_aes = ggplot2::aes(
    alpha = NULL, colour = NULL, angle = 0, hjust = 0.5,
    vjust = 0.5, width = 1.0, height = 1.0
  ),
  draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
    data <- coord$transform(data, panel_params)

    grobs <- lapply(
      seq_along(data$path),
      build_grobs,
      alpha = data$alpha,
      colour = data$colour,
      path = data$path,
      data = data
      )

    class(grobs) <- "gList"

    grid::gTree(children = grobs)
  },
  draw_key = function(...) grid::nullGrob()
)
