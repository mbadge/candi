# Store pkg-wide configuration variables here, and they'll be instantiated into
# options when the hips package is loaded.
#
# Package option schema:
# [pkg name] DOT [snake_case_option_name]
# candi.option_one
.onLoad <- function(libname, pkgname) {
    # I/O
    SMALL_IMG_DIR <- '/www/app_data/candi/small_jpgs/'  # imageNet size 299x299
    LARGE_IMG_DIR <- '/www/app_data/candi/large_jpgs/'  # original resolution

    USR_INPUT_DIR <- '/www/app_data/candi/usr_input/'  # Parent for all data on users

    # User input types (each will get it's own subfolder with USR_INPT_DIR)
    ANNOTATION_TYPES <- c("classificaiton", "segmentation", "clinical_note")

    # NSE FXN
    #' @examples
    #' Show(SMALL_IMG_DIR)
    Show <- function(FLAG) {
        paste0(str_case_title(deparse(substitute(FLAG))), ": ",
               paste(FLAG, collapse=", "), "\n"
        )
    }

    packageStartupMessage("Setting CANDI Analysis Global Variables:\n\n",
                          Show(SMALL_IMG_DIR),
                          Show(LARGE_IMG_DIR),
                          Show(USR_INPUT_DIR),
                          Show(ANNOTATION_TYPES),
                          "\ncall candiOpt(<snake_case_opt>) to fetch; e.g. `candiOpt(annotation_types)`")

    opt <- options()
    opt_candi <- list(
        candi.small_img_dir = SMALL_IMG_DIR,
        candi.large_img_dir = LARGE_IMG_DIR,
        candi.usr_input_dir = USR_INPUT_DIR,
        candi.annotation_types = ANNOTATION_TYPES
    )

    to_set <- !(names(opt_candi) %in% names(opt))
    if(any(to_set)) options(opt_candi[to_set])
    invisible()
}



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

    getOption(paste("candi", opt_substr, sep = "."), ...)
}

#' @export
#' @rdname candiOpt_
#' @examples
#' candiOpt()
#' candiOpt(annotation_types)
candiOpt <- function(opt_substr, ...) {
    if (missing(opt_substr)) {
        avail = paste0("CANDI pkg options:\n",
                      paste(str_subset(names(options()), "candi"), collapse="\n"))
        cat(avail)
        return(invisible())
    }
    candiOpt_(deparse(substitute(opt_substr)))
}
