# INTERNAL HELPER THAT BUILDS THE GROBS FOR GeomFromPath
build_grobs <- function(i, alpha, colour, path, data) {
  img <- try(reader_function(path[i]), silent = TRUE)

  if (inherits(img, "try-error")) cli::cli_abort(img)

  if (is.null(alpha)) { # no alpha requested
    modified_img <- resolve_img_color(img = img, col = colour[i])
  }
  else { # alpha is requested
    if (any(as.numeric(alpha) < 0) || any(as.numeric(alpha) > 1)) {
      cli::cli_abort("aesthetics {.var alpha} require values between {.val 0} and {.val 1}")
    }
    modified_img <- magick::image_fx(img, expression = paste0(alpha[i], "*a"), channel = "alpha")
    modified_img <- resolve_img_color(img = modified_img, col = colour[i])
  }

  grid::rasterGrob(
    modified_img,
    vp = grid::viewport(
      x = grid::unit(data$x[i], "native"),
      y = grid::unit(data$y[i], "native"),
      width = grid::unit(data$width[i], "npc"),
      height = grid::unit(data$height[i], "npc"),
      just = c(data$hjust[i], data$vjust[i]),
      angle = data$angle[i]
    ),
    name = paste0("ggpath.grob.", i)
  )
}

# decides if we should read with dedicated svg reader or not
reader_function <- function(img){
  if(is.factor(img)) img <- as.character(img)
  if(is.raw(img) || tools::file_ext(img) != "svg"){
    magick::image_read(img)
  } else if(tools::file_ext(img) == "svg"){
    magick::image_read_svg(img)
  }
}

# applies image colorization depending on the value of col
resolve_img_color <- function(img, col){
  if (!is.null(col) && col %in% "b/w"){
    modified_img <- magick::image_quantize(img, colorspace = 'gray')
  } else {
    opa <- ifelse(is.na(col) || is.null(col), 0, 100)
    col <- ifelse(is.na(col) || is.null(col), "none", col)
    modified_img <- magick::image_colorize(img, opa, col)
  }
  modified_img
}
