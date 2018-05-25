# Store pkg-wide configuration variables here, and they'll be instantiated into
# options when the hips package is loaded.
#
# Package option schema:
# [pkg name] DOT [snake_case_option_name]
# eg: candi.option_one
.onLoad <- function(libname, pkgname) {

    options("EBImage.display" = "browser")  # Show images in browser mode, even if I'm not using R interactively.

    # FSIO ----
    PkgDir <- function(...) {
        fp <- file.path('/www/app_data/candi', ...)
        if (!file.exists(fp)) {warning("Pkg Directory doesn't exist: ", fp)}
        fp
    }

    # Apps can share image repos to consolidate bulky data
    SMALL_IMG_DIR <- PkgDir('small_jpgs')  # imageNet size 299x299 images
    LARGE_IMG_DIR <- PkgDir('large_jpgs')  # original resolution images
    BBOX_IMG_DIR <- PkgDir('bbox')  # Images with CNN bounding box inference results for CAD

    # parent directory for segregating app-specific data collections (eg, user input, log)
    APP_DATA_DIR <- PkgDir('apps')
    # Each app has a subdir under the general candi app_data_dir, with
    # downstream targets fully defined in the app's global.R

    # FLAGS ----
    # User input types (each will get it's own subfolder with USR_INPT_DIR)
    ANNOTATION_TYPES <- c("classification", "segmentation", "clinical_note")
    DXS_CHR <- c('Cardiomegaly', 'Emphysema', 'PleuralEffusion',
                  'HerniaHiatal', 'Nodule', 'PulmonaryAtelectasis',
                  'Pneumonia', 'PulmonaryEdema', 'Consolidation',
                  'CathetersIndwelling', 'TechnicalQualityOfImageUnsatisfactory',
                  'LungHypoinflation', 'LungHyperdistention') %>% purrr::set_names(., str_case_title(.))

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
                          Show(BBOX_IMG_DIR),
                          Show(APP_DATA_DIR),
                          Show(ANNOTATION_TYPES),
                          Show(DXS_CHR),
                          "\ncall candiOpt(<snake_case_opt>) to fetch; e.g. `candiOpt(annotation_types)`")

    opt <- options()
    opt_candi <- list(
        candi.small_img_dir = SMALL_IMG_DIR,
        candi.large_img_dir = LARGE_IMG_DIR,
        candi.bbox_img_dir = BBOX_IMG_DIR,
        candi.app_data_dir = APP_DATA_DIR,
        candi.annotation_types = ANNOTATION_TYPES,
        candi.dxs_chr = DXS_CHR
    )

    to_set <- !(names(opt_candi) %in% names(opt))
    if(any(to_set)) options(opt_candi[to_set])
    invisible()
}



#' Convenience fxn to fetch an option set by the hips package.
#'
#' candiOpt uses NSE to save typing, use candiOpt_ for SE
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

    candi_opts <- str_subset(names(options()), "^candi")
    # if opt_substr empty or invalid, show user all available
    if (missing(opt_substr) || str_c("candi.", opt_substr) %ni% candi_opts) {
        avail = paste0("CANDI pkg options:\n",
                      paste(candi_opts, collapse="\n"))
        stop("Please supply a valid option to candiOpt()...\n", avail, call. = FALSE)
        return(invisible())
    }


    getOption(paste("candi", opt_substr, sep = "."), ...)
}

#' @export
#' @rdname candiOpt_
#' @examples
#' candiOpt()
#' candiOpt(annotation_types)
#' candiOpt(foobar)
candiOpt <- function(opt_substr, ...) {
    candiOpt_(deparse(substitute(opt_substr)))
}


#' Application title based on app directory
#'
#' @export
#' @examples
#' WindowTitle()
WindowTitle <- compose(basename, getwd)


