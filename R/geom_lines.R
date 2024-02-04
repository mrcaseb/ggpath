#' ggplot2 Layer for Horizontal and Vertical Reference Lines
#'
#' @description These geoms can be used to draw horizontal or vertical reference
#'   lines in a ggplot. They use the data in the aesthetics `x0` and `y0`
#'   to compute their `median` or `mean` and draw them as a line.
#'
#' @inheritParams ggplot2::geom_hline
#' @param inherit.aes If `FALSE`, overrides the default aesthetics,
#'   rather than combining with them. This is most useful for helper functions
#'   that define both data and aesthetics and shouldn't inherit behavior from
#'   the default plot specification.
#' @section Aesthetics:
#' `geom_median_lines()` and `geom_mean_lines()` understand the following
#' aesthetics (at least one of the bold aesthetics is required):
#' \describe{
#'   \item{**`x0`**}{The variable for which to compute the median/mean that is drawn as vertical line.}
#'   \item{**`y0`**}{The variable for which to compute the median/mean that is drawn as horizontal line.}
#'   \item{`alpha = NA`}{The alpha channel, i.e. transparency level, as a numerical value between 0 and 1.}
#'   \item{`color = "red"`}{The color of the drawn lines.}
#'   \item{`linetype = 2`}{The linetype of the drawn lines.}
#'   \item{`size = 0.5`}{The size of the drawn lines. Deprecated as of ggplot2 v3.4.0, use `linewidth` instead.}
#'   \item{`linewidth = 0.5`}{The width of the drawn lines. Starting at ggplot2 v3.4.0.}
#' }
#' @seealso The underlying ggplot2 geoms [`geom_hline()`] and [`geom_vline()`]
#' @name geom_lines
#' @return A ggplot2 layer ([ggplot2::layer()]) that can be added to a plot
#'   created with [ggplot2::ggplot()].
#' @aliases NULL
#' @examples
#' library(ggplot2)
#'
#' # inherit top level aesthetics
#' ggplot(mtcars, aes(x = disp, y = mpg, y0 = mpg, x0 = disp)) +
#'   geom_point() +
#'   geom_median_lines() +
#'   geom_mean_lines(color = "blue") +
#'   theme_minimal()
#'
#' # draw horizontal line only
#' ggplot(mtcars, aes(x = disp, y = mpg, y0 = mpg)) +
#'   geom_point() +
#'   geom_median_lines() +
#'   geom_mean_lines(color = "blue") +
#'   theme_minimal()
#'
#' # draw vertical line only
#' ggplot(mtcars, aes(x = disp, y = mpg, x0 = disp)) +
#'   geom_point() +
#'   geom_median_lines() +
#'   geom_mean_lines(color = "blue") +
#'   theme_minimal()
#'
#' # choose your own value
#' ggplot(mtcars, aes(x = disp, y = mpg)) +
#'   geom_point() +
#'   geom_median_lines(x0 = 400, y0 = 15) +
#'   geom_mean_lines(x0 = 150, y0 = 30, color = "blue") +
#'   theme_minimal()
NULL

#' @rdname geom_lines
#' @export
geom_median_lines <- function(mapping = NULL, data = NULL,
                              ...,
                              na.rm = FALSE,
                              show.legend = NA,
                              inherit.aes = TRUE) {

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = ggplot2::StatIdentity,
    geom = GeomRefLines,
    position = ggplot2::PositionIdentity,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      ref_function = stats::median,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_lines
#' @export
geom_mean_lines <- function(mapping = NULL, data = NULL,
                            ...,
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = ggplot2::StatIdentity,
    geom = GeomRefLines,
    position = ggplot2::PositionIdentity,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      ref_function = base::mean,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggpath-package
#' @export
GeomRefLines <- ggplot2::ggproto("GeomRefLines", ggplot2::Geom,

  optional_aes = c("x0", "y0", "size"),

  default_aes = ggplot2::aes(colour = "red", linewidth = 0.5, linetype = 2, alpha = NA),

  draw_panel = function(data, panel_params, coord, ref_function, na.rm = FALSE) {
    args <- names(data)

    # Can't do anything if either of y0 and x0 are aesthetics
    if (all(!c("x0", "y0") %in% args)) {
      cli::cli_abort("{.var geom_median_lines()} and {.var geom_mean_lines()} \\
                     require at least one of the following aesthetics: \\
                     {.var x0}, {.var y0}")
    }

    if (any(args == "size") && is_ggplot_340()){
      cli::cli_warn(
        "The {.arg size} aesthetic has been deprecated as of ggplot2 v3.4.0! \\
        Please use {.arg linewidth} in the future. \\
        Will proceed with {.arg linewidth = size}"
      )
      data$linewidth <- data$size
    }

    if(!is_ggplot_340()) data$size <- data$linewidth

    # Since y0 and x0 can be in data, it is necessary to select only
    # those variables that are required for the underlying Geoms to work.
    # This could also be achieved by setting inherit.aes to FALSE explicitly but
    # I want to be able to inherit aesthetics so I had to do this differently.
    # We also need to distinguish in the ggplot2 version because the "size"
    # argument has been replaced by "linewidth" in v3.4.0.
    relevant_columns <- if(is_ggplot_340()){
      c("PANEL", "group", "colour", "linewidth", "linetype", "alpha")
    } else{
      c("PANEL", "group", "colour", "size", "linetype", "alpha")
    }

    # if x0 and/or y0 are present in data we have to compute the relevant
    # xintercept and yintercept variables and drop anything irrelevant from data
    # as the underlying Geoms will draw multiple lines if there are multiple
    # unique rows. This caused alpha to not work properly because the geom draws
    # many opaque lines which outputs as non-opaque lines.
    if ("x0" %in% args){
      data_v <- data
      data_v$xintercept <- ref_function(data_v$x0, na.rm = na.rm)
      data_v <- data_v[,c("xintercept", relevant_columns)]
    }

    if ("y0" %in% args){
      data_h <- data
      data_h$yintercept <- ref_function(data_h$y0, na.rm = na.rm)
      data_h <- data_h[,c("yintercept", relevant_columns)]
    }

    if (!"x0" %in% args) {
      ggplot2::GeomHline$draw_panel(data_h, panel_params, coord)
    } else if (!"y0" %in% args) {
      ggplot2::GeomVline$draw_panel(data_v, panel_params, coord)
    } else {
      grid::gList(
        ggplot2::GeomHline$draw_panel(data_h, panel_params, coord),
        ggplot2::GeomVline$draw_panel(data_v, panel_params, coord)
      )
    }
  },

  draw_key = ggplot2::draw_key_path
)

is_ggplot_340 <- function() utils::packageVersion("ggplot2") >= "3.4.0"
