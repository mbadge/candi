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

#' Load radiographs into EBImage Images
#'
#' \code{\link[EBImage]{readImage}}
#'
#' @param img_id chr(1)
#' @param img_dir chr(1)
#'
#' @return \code{\link[EBImage]{Image}}
#'
#' @importFrom EBImage "readImage"
#' @export
load_radiograph <- function(img_id, img_dir) {
    fp <- file.path(img_dir, paste0(img_id, ".jpg"))
    img <- EBImage::readImage(fp)
    return(img)
}

#' Randomly select CAD state variables for a new test image: test_img_id and reader_mode
#'
#' @export
getNewCaseDf <- function(test_img_ids) {
    data.frame(
        test_img_id = sample(x = fp_stem(test_img_ids), size = 1),
        reader_mode = sample(x = c("concurrent", "second"), size = 1)
    )
}

#' Utility fxn to search all user input data to identify the most recent case reviewed by a user
#'
#' This function is used by the CANDI_CAD trial state manager to reserve an image
#' for second reader mode after initially having the user review an image without assistance.
#'
#' @param user_name chr(1) must match a col user_name in the user input data frames
#' @param usr_input_dir chr(1) directory containing user input records
#' @return chr(1) img_id of last image reviewed
#'
#' @export
#' @examples
#' getLastCaseChr(user_name="Marcus", usr_input_dir="/www/app_data_candi/CANDI_CAD/usr_inpt")
getLastCaseChr <- function(user_name, usr_input_dir) {
    # Precondition
    stopifnot(dir.exists(usr_input_dir))

    # Load all user input records
    suppressMessages({
        all_records <- list.files(usr_input_dir, full.names = TRUE) %>%
            map_dfr(., readr::read_csv)
    })

    # Isolate just `user_name`s records
    user_records <- all_records[all_records$user_name == user_name, ]
    # Find the most recent case submitted by `user_name`
    user_records %>%
        mutate(rank = rank(desc(lubridate::ymd_hms(timestamp)))) %>%
        filter(rank == 1) %>%
        use_series("test_img_id")
}
