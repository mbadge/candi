#' Save a data frame with user input to csv
#'
#' The filename is computed as the md5 hash of the data.frame
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
#' save_fp <- save_usr_input(mtcars); print(save_fp)  # save to usr_input_dir
#' read_csv(save_fp); file.remove(save_fp)            # and remove
save_usr_input <- function(x, dir = candiOpt(usr_input_dir)) {
    stopifnot(is.data.frame(x))

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
#'   load_usr_input("Marcus")
#' }
load_usr_input <- function(user, dir = candiOpt(usr_input_dir)) {
    # precondition
    stopifnot(dir.exists(dir))

    record_fps <- list.files(dir, pattern = "*.csv$", full.names = TRUE)

    if (length(record_fps) == 0) {
        warning("No user input files found in ", dir)
        return(NULL)
    }

    all_records <- purrr::map_dfr(record_fps, ~suppressMessages(readr::read_csv(.x)))
    all_records[all_records$user_name == user, ]
}

