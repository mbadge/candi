## ---- RAD ----

#' Create a download handler for one of the annotation types in \code{kANNOTATION_TYPES}
#'
#' @param ann_type chr(1), one of the globally assigned
#' @param f_load function to load annotations such as \code{\link{load_gs_annotation}}
#'
#' @return assign return to a slot on shiny's \code{output} and in the UI use \code{\link[shiny]{downloadButton}} or \code{\link[shiny]{downloadLink}}
#' @export
handle_annotation_download <- function(ann_type, f_load) {
    if (ann_type %ni% kANNOTATION_TYPES) {
        warning(glue::glue("invalid annotation type: '{ann_type}';
                            expected one of {kANNOTATION_TYPES %>% str_c(collapse=', ')}"))
    }
    shiny::downloadHandler(
        filename = function() {glue::glue("{ann_type}_annotations.csv")},
        content = function(file) {write.csv(f_load(ann_type), file, row.names=FALSE)},
        contentType="text/csv"
    )
}

#' @export
load_gs_annotation <- function(gSpreadSheet, ann_type) {
    stopifnot(ann_type %in% kANNOTATION_TYPES)
    googlesheets::gs_read(gSpreadSheet, ws=ann_type)
}

#' Load csv files for a given annotation type
#'
#' @export
#' @examples
#' load_csv_annotation(kANN_DIR, "classification")
load_csv_annotation <- function(kANN_DIR, ann_type) {
    stopifnot(ann_type %in% kANNOTATION_TYPES)
    fps <- list.files(file.path(kANN_DIR, ann_type), full.names = TRUE)
    df <- purrr::map_dfr(fps, read.csv, stringsAsFactors=FALSE)
    df
}


## ---- CAD ----
#' Save a data frame with user input to csv
#'
#' The filename is computed as the md5 hash of the data.frame
#'
#' @param x data.frame to save
#' @param dir chr(1)
#'
#' @return called for side effect
#' @export
#'
#' @examples
#' save_usr_input(mtcars, "~/app_data_cxrTargetDiff/usr_inpt")
save_usr_input <- function(x, dir) {
    stopifnot(is.data.frame(x))

    x$timestamp <- MyUtils::date_time_stamp()
    x <- dplyr::select(x, timestamp, dplyr::everything())

    fn <- stringr::str_c(digest::digest(x, algo="md5"), ".csv")
    readr::write_csv(x, path = file.path(dir, fn))
}


#' Load input app data from a user
#'
#' By default, data is loaded form a directory indicated by a
#' global variable defined in the file that defines \code{load_usr_input}.
#'
#' @param user chr(1) user to filter user_name column by
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#'   load_usr_input("Marcus", "/www/app_data_cxrTargetDiff/usr_inpt/")
#' }
load_usr_input <- function(user, usr_inpt_dir) {
    # precondition
    stopifnot(dir.exists(usr_inpt_dir))

    record_fps <- list.files(usr_inpt_dir, pattern = "*.csv$", full.names = TRUE)

    if (length(record_fps) == 0) {
        warning("No user input files found in ", usr_inpt_dir)
        return(NULL)
    }

    all_records <- purrr::map_dfr(record_fps, ~suppressMessages(readr::read_csv(.x)))
    all_records[all_records$user_name == user, ]
}

