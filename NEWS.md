# ggpath (development version)

* Invalid paths no longer result in an error. Instead, ggpath will warn the user and insert an empty grob.
* Minor adjustment to the format of lists in the documentation to avoid check problems in an upcoming R release.
* Added new theme element `element_raster()` that allows custom images in ggplot backgrounds. (#9)

# ggpath 1.0.1

* Fixed a test that was failing on some operating systems.
* Catch ggplot2 warning on deprecated `size` argument when drawing lines with ggplot2 >= v3.4.0 

# ggpath 1.0.0

* Initial release.
