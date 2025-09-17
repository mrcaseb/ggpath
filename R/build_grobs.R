# INTERNAL HELPER THAT BUILDS THE GROBS FOR GeomFromPath and element_path
build_grobs <- function(i, alpha, colour, path, data,
                        is_theme_element = FALSE,
                        call = rlang::caller_env()) {
  to_read <- path[i]

  # to_read might be a list of length 1 if ggpath's S7 method is called
  # from other packages, e.g. nflplotR
  # If that is the case, we unlist and check whether the object is NULL.
  # If it is NULL we can return a NULL grob silently as the calling packages
  # are supposed to alert the user about non matches.
  if (is.list(to_read)) {
    to_read <- unlist(to_read, recursive = FALSE, use.names = FALSE)
  }
  if (is.null(to_read)) return(ggpath_null_grob(data = data, i = i))

  img <- try(reader_function(to_read), silent = TRUE)

  # if the path is invalid we warn the user and insert a NULL grob
  if (inherits(img, "try-error")) {
    cli::cli_warn(
      "{.pkg ggpath} failed to read an image from {.path {to_read}}. \\
      It will insert an empty graphic object instead. Here is the \\
      error message: {img}"
    )

    return(ggpath_null_grob(data = data, i = i))
  }

  if (is.null(alpha) | all(alpha == 1L)) { # no alpha requested
    modified_img <- resolve_img_color(img = img, col = colour[i])
  }
  else { # alpha is requested
    if (any(as.numeric(alpha) < 0) || any(as.numeric(alpha) > 1)) {
      cli::cli_abort(c(
        "all values of {.arg alpha} have to be in range {.val {0}}:{.val {1}}",
        "x" = "You've supplied {.val {unique(alpha)}}"
      ), call = call)
    }
    modified_img <- magick::image_fx(img, expression = paste0(alpha[i], "*a"), channel = "alpha")
    modified_img <- resolve_img_color(img = modified_img, col = colour[i])
  }

  # theme elements require justification outside the viewport
  # so we have to do this twice here
  if(isFALSE(is_theme_element)){
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
  } else if (isTRUE(is_theme_element)){
    grid::rasterGrob(
      modified_img,
      vp = grid::viewport(
        x = grid::unit(data$x[i], "npc"),
        y = grid::unit(data$y[i], "npc"),
        width = data$width[i],
        height = data$height[i],
        angle = data$angle[i]
      ),
      just = c(data$hjust[i], data$vjust[i]),
      name = paste0("ggpath.grob.", i)
    )
  }
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

ggpath_null_grob <- function(data, i){
  grid::nullGrob(
    name = paste0("ggpath.grob.", i),
    vp = grid::viewport(
      x = grid::unit(data$x[i], "native"),
      y = grid::unit(data$y[i], "native")
    )
  )
}
