# ggpath (development version)

* adjust how size is applied to theme elements to avoid huge margins around some images. 

# ggpath 1.1.0

ggpath now requires ggplot2 v4! This package is intended to be a ggplot2 extension 
and if ggplot2 version jumps make breaking changes, then it's best for the extension 
to not try to be downwards compatible.

* ggpath now requires R 4.1 because magick needs this R version. This also follows the [Tidyverse R version support rules](https://tidyverse.org/blog/2019/04/r-version-support/). (#14)
* rewrite theme elements in S7 to fully work with ggplot2 v4.0. (#16)
* remove ggplot2 downwards compatibility in `size` argument of `geom_*_lines()` functions as ggpath now requires ggplot2 v4.

# ggpath 1.0.2

* Invalid paths no longer result in an error. Instead, ggpath will warn the user and insert an empty grob.
* Minor adjustment to the format of lists in the documentation to avoid check problems in an upcoming R release.
* Added new theme element `element_raster()` that allows custom images in ggplot backgrounds. (#9)
* Drop dependency to package rappdirs and create an optional user cache with base R's implementation. To support older R versions, ggpath now imports the backports package. (#10)

# ggpath 1.0.1

* Fixed a test that was failing on some operating systems.
* Catch ggplot2 warning on deprecated `size` argument when drawing lines with ggplot2 >= v3.4.0 

# ggpath 1.0.0

* Initial release.
