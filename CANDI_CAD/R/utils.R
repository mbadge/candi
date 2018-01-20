theme_set(theme_dark())

date_time_stamp <- function(){format(Sys.time(), "%Y%m%d_%H%M%S")}

`%ni%` <- Negate(`%in%`)

# Boolean predicate fxn operators ----
Or <- function(f1, f2){
    force(f1); force(f2)
    function(...){
        f1(...) || f2(...)
    }
}
Not <- function(f1){
    force(f1)
    function(...){!f1(...)}
}


#' Rasterize an EBImage Image object figure.
#'
#' @param img_arr an EMIage `Image` object
#'
#' @return side-effect: draws figure
#'
#' @importFrom dplyr "%>%"
#' @export
Viz.Image <- function(img_arr) {
    stopifnot(inherits(img_arr, "Image"))
    img_arr %>%
        EBImage::normalize() %>%
        EBImage::display(method="raster")
}
