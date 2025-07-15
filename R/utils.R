
# No code coverage here ---------------------------------------------------

# nocov start
release_bullets <- function() {
  c(
    '`devtools::check_mac_release()`',
    '`ggpath:::my_rhub_check()`',
    '`pkgdown::check_pkgdown()`',
    NULL
  )
}

my_rhub_check <- function() {
  cli::cli_text("Please run the following code")
  cli::cli_text(
    "{.run rhub::rhub_check(platforms = ggpath:::rhub_check_platforms())}"
  )
}

rhub_check_platforms <- function() {
  # plts created with
  # out <- paste0('"', rhub::rhub_platforms()$name, '"', collapse = ",\n")
  # cli::cli_code(paste0(
  #   "plts <- c(\n",
  #   out,
  #   "\n)"
  # ))

  plts <- c(
    "linux",
    "m1-san",
    "macos",
    "macos-arm64",
    "windows",
    "atlas",
    "c23",
    "clang-asan",
    "clang-ubsan",
    "clang16",
    "clang17",
    "clang18",
    "clang19",
    "clang20",
    "donttest",
    "gcc-asan",
    "gcc13",
    "gcc14",
    "gcc15",
    "intel",
    "mkl",
    "nold",
    "noremap",
    "nosuggests",
    "rchk",
    "ubuntu-clang",
    "ubuntu-gcc12",
    "ubuntu-next",
    "ubuntu-release",
    "valgrind"
  )
  exclude <- c("rchk", "nosuggests", "valgrind")
  plts[!plts %in% exclude]
}
# nocov end
