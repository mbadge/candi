# Store pkg-wide configuration variables here, and they'll be instantiated into
# options when the hips package is loaded.
#
# Package option schema:
# [pkg name] DOT [snake_case_option_name]
# candi.option_one
.onLoad <- function(libname, pkgname) {
    ANNOTATION_TYPES <- c("classificaiton", "segmentation", "clinical_note")

    packageStartupMessage("Setting CANDI Analysis Global Variables:\n",
                          paste("annotation types:", ANNOTATION_TYPES),
                          "call e.g. `candiOpt(annotation_types)` to fetch")

    opt <- options()
    opt_candi <- list(
        candi.annotation_types = ANNOTATION_TYPES
    )

    to_set <- !(names(opt_candi) %in% names(opt))
    if(any(to_set)) options(opt_candi[to_set])
    invisible()
}


# ---- Option accessors ----

#' Convenience fxn to fetch an option set by the hips package.
#'
#' getHipOpt uses NSE to save typing, use getHipOpt_ for SE
#'
#' Package option schema:
#' [pkg name] DOT [snake_case_option_name]
#' eg: hips.option_one
#'
#' @param opt_substr chr(1) specific hips option
#' @param ... optional default if variable isn't set \code{\link[base]{options}}
#'
#' @return set option or ...
#' @export
#'
#' @examples
#' candiOpt_("annotation_types")
candiOpt_ <- function(opt_substr, ...) {
    # Precondition
    stopifnot(is.character(opt_substr), length(opt_substr) == 1)

    getOption(paste("candi", opt_substr, sep = "_"), ...)
}

#' @export
#' @rdname candiOpt_
#' @examples
#' candiOpt(annotation_types)
candiOpt <- function(opt_substr, ...) {
    candiOpt_(deparse(substitute(opt_substr)))
}
