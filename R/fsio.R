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
#' @export
save_usr_input <- function(data_df) {
    stopifnot(dir.exists(file.path("www", "usr_input")))
    fn <- str_c(str_c(data_df$username, data_df$test_img_id, sep="-"), ".csv")
    write_csv(x=data_df, path=file.path('www', "usr_input", fn))
}

#' @export
getNewCaseDf <- function() {
    data.frame(
        test_img_id = sample(x = stem(test_img_fns), size = 1),
        reader_mode = sample(x = c("concurrent", "second"), size = 1)
    )
}

#' @export
getLastCaseChr <- function(user) {
    suppressMessages({
        all_records <- list.files(file.path("www", "usr_input"), full.names = TRUE) %>%
            map_dfr(., read_csv)
    })
    user_records <- all_records[all_records$username == user, ]
    user_records %>%
        mutate(rank = rank(desc(lubridate::ymd_hms(timestamp)))) %>%
        filter(rank == 1) %>%
        use_series("test_img_id")
}
