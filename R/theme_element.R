#' Theme Element for Image Grobs
#'
#' @description
#' In conjunction with the [ggplot2::theme] system, the function `element_path()`
#' enables images in non-data components of the plot, e.g. axis text. It draws
#' images from valid image URLs, raster objects, or bitmap arrays.
#'
#' @param alpha The alpha channel, i.e. transparency level, as a numerical value
#'   between 0 and 1.
#' @param colour,color The image will be colorized with this color. Use the
#'   special character `"b/w"` to set it to black and white. For more information
#'   on valid color names in ggplot2 see
#'   <https://ggplot2.tidyverse.org/articles/ggplot2-specs.html?q=colour#colour-and-fill>.
#' @param hjust,vjust The horizontal and vertical adjustment respectively.
#'   Must be a numerical value between 0 and 1.
#' @param angle The angle of the element as a numerical value between 0° and 360°.
#' @param size The output grob size in `cm` (!).
#' @seealso [geom_from_path()] for more information.
#' @return An S3 object of class `element`.
#' @examples
#' \donttest{
#' library(ggplot2)
#'
#' df <- dplyr::mutate(mtcars,
#'   team = sample(c("LAC", "BUF", "DAL", "ARI"), nrow(mtcars), TRUE),
#'   player = sample(
#'   c("00-0033873", "00-0035228", "00-0036355", "00-0019596"),
#'   nrow(mtcars),
#'   TRUE
#'   )
#' )
#'
#' ggplot(df, aes(x = mpg, y = disp)) +
#'   geom_point() +
#'   facet_wrap(vars(team)) +
#'   labs(
#'     title = tools::toTitleCase("These are random teams and data"),
#'     subtitle = "I just want to show how the nflplotR theme elements work",
#'     caption = "https://github.com/nflverse/nflseedR/raw/master/man/figures/caption.png"
#'   ) +
#'   theme_minimal() +
#'   theme(
#'     plot.title.position = "plot",
#'     plot.title = element_text(face = "bold"),
#'     axis.title = element_blank(),
#'     # load image from url in caption
#'     plot.caption = element_path(hjust = 1, size = 0.4)
#'   )
#' }
#' @export
element_path <- function(alpha = NULL, colour = NA, hjust = NULL, vjust = NULL,
                         color = NULL, angle = NULL, size = 0.5) {
  if (!is.null(color))  colour <- color
  structure(
    list(alpha = alpha, colour = colour, hjust = hjust, vjust = vjust, angle = angle, size = size),
    class = c("element_path", "element_text", "element")
  )
}

#' @export
element_grob.element_path <- function(element, label = "", x = NULL, y = NULL,
                                      alpha = NULL, colour = NULL,
                                      hjust = 0.5, vjust = 0.5,
                                      angle = 0, size = NULL, ...) {

  if (is.null(label)) return(ggplot2::zeroGrob())

  n <- max(length(x), length(y), 1)
  vj <- element$vjust %||% vjust
  hj <- element$hjust %||% hjust
  angle <- element$angle %||% angle
  x <- x %||% rep(hj, n)
  y <- y %||% rep(vj, n)
  alpha <- alpha %||% rep(element$alpha, n)
  colour <- colour %||% rep(element$colour, n)
  size <- size %||% element$size

  grobs <- lapply(
    seq_along(label),
    build_grobs,
    alpha = alpha,
    colour = colour,
    path = label,
    data = data.frame(
      x = x,
      y = y,
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
grobHeight.ggpath_element <- function(x, ...) grid::unit(x$size, "cm")

#' @export
grobWidth.ggpath_element <- function(x, ...) grid::unit(x$size, "cm")
