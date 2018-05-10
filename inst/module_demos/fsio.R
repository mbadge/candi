AppDataDir <- function(app, ...) {
    fp <- file.path(candiOpt(app_data_dir), app, ...)
    stopifnot(dir.exists(fp))
    fp
}

kDIR_USR_INPT <- AppDataDir("rad_image", "usr_input")


dir <- kDIR_USR_INPT
user <- "Marcus"

stopifnot(dir.exists(dir))
load_usr_input("Marcus", dir = dir)


record_fps <- list.files(dir, pattern = "*.csv$", full.names = TRUE)

if (length(record_fps) == 0) {
    warning("No user input files found in ", dir)
    return(NULL)
}

purrr::map_dfr(record_fps,
                              ~suppressMessages(readr::read_csv(.x)))
purrr::map(record_fps,
                              ~suppressMessages(readr::read_csv(.x)))


load_usr_input("Marcus", dir = file.path(candiOpt(app_data_dir), "rad_image", "usr_input"))
load_usr_input("Marcus", dir = file.path(candiOpt(app_data_dir), "rad_case", "usr_input"))
load_usr_input("Marcus", dir = file.path(candiOpt(app_data_dir), "rad_multimodal", "usr_input"))
