.onAttach <- function(libname, pkgname) {
    ggplot2::theme_set(ggplot2::theme_light())
}
.onDetach <- function(libpath) {
    ggplot2::theme_set(ggplot2::theme_gray())
}
