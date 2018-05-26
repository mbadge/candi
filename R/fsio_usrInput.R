#' Save a data frame with user input to csv
#'
#' A column is added with save time and the file is named by md5 hash.
#'
#' @param x data.frame to save
#' @param dir chr(1)
#'
#' @return called for side effect; but returns target fp for reference
#' @export
#'
#' @examples
#' save_fp <- save_usr_input(mtcars, dir = tempdir())  # Save to tmp
#' read_csv(save_fp)
#' save_fp <- save_usr_input(mtcars, candiOpt(app_data_dir)); print(save_fp)  # save to usr_input_dir
#' read_csv(save_fp); file.remove(save_fp)            # and remove
save_usr_input <- function(x, dir, subdir="") {
    # Preconditions
    dir <- file.path(dir, subdir)
    stopifnot(is.data.frame(x))
    if (!dir.exists(dir)) stop("Directory ", dir, " doesn't exist")

    x$timestamp <- MyUtils::date_time_stamp()
    x <- dplyr::select(x, timestamp, dplyr::everything())

    fn <- stringr::str_c(digest::digest(x, algo="md5"), ".csv")
    readr::write_csv(x, path = file.path(dir, fn))

    file.path(dir, fn)
}


#' Load input app data from a user
#'
#' By default, data is loaded form a directory indicated by a
#'
#' @param user chr(1) user to filter user_name column by
#' @param dir chr(1)
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#'   load_usr_input("Marcus", dir = file.path(candiOpt(app_data_dir), "rad_image", "usr_input"))
#' }
load_usr_input <- function(user, dir) {
    # precondition
    if (!dir.exists(dir)) stop("Directory ", dir, " doesn't exist")

    record_fps <- list.files(dir, pattern = "*.csv$", full.names = TRUE)

    if (length(record_fps) == 0) {
        warning("No user input files found in ", dir)
        return(NULL)
    }

    all_records <- purrr::map_dfr(record_fps,
            ~suppressMessages(readr::read_csv(.x)))
    all_records[all_records$user_name == user, ]
}



#' Save a timestamped record when a user triggers an event.
#'
#' @param usr_chr chr(1) username
#' @param event_chr chr(1) event name
#' @param dir chr(1)
#' @param img_id optional chr(1)
#'
#' @return target file path, but called for saving side effect
#' @export
#'
#' @examples
#' save_fp <- log_usr_event("marcus", "submitBtn", dir = candiOpt(app_data_dir))
#' read_csv(save_fp); file.remove(save_fp)            # and remove
log_usr_event <- function(usr_chr, event_chr, dir, img_id=NA_character_) {
    # Preconditions ----
    stopifnot(is.character(usr_chr), is.character(event_chr))
    if (!dir.exists(dir)) stop("Directory ", dir, " doesn't exist")

    df <- data.frame(
        timestamp = MyUtils::date_time_stamp(),
        user_name = usr_chr,
        img_id = img_id,
        event = event_chr
    )

    fn <- stringr::str_c(digest::digest(df, algo="md5"), ".csv")
    readr::write_csv(df, path = file.path(dir, fn))

    file.path(dir, fn)
}
