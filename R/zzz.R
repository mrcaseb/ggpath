# nocov start
.onLoad <- function(libname, pkgname){

  memoise_option <- getOption("ggpath.cache", default = "memory")

  if(!memoise_option %in% c("memory", "filesystem", "off")) memoise_option <- "memory"

  if(memoise_option == "filesystem"){
    cache_dir <- R_user_dir("ggpath", "cache")
    if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    cache <- cachem::cache_disk(dir = cache_dir)
  }

  if(memoise_option == "memory") cache <- cachem::cache_mem()

  if(memoise_option != "off"){
    assign(x = "reader_function",
           value = memoise::memoise(reader_function, ~ memoise::timeout(86400), cache = cache),
           envir = parent.env(environment()))
  }
}

.onAttach <- function(libname, pkgname){

  # validate ggpath.cache
  memoise_option <- getOption("ggpath.cache", default = "memory")

  if (!memoise_option %in% c("memory", "filesystem", "off")) {
    packageStartupMessage('Note: ggpath.cache is set to "',
                          memoise_option,
                          '" and should be one of c("memory","filesystem", "off"). \n',
                          'Defaulting to "memory".')
    memoise_option <- "memory"
  }
  if(memoise_option == "off") packageStartupMessage('Note: ggpath.cache is set to "off"')
}
# nocov end
