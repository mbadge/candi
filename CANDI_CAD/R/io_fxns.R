# i/o helper fxns ---------------
save_usr_usage <- function(log_df) {
    stopifnot(dir.exists(file.path("www", "usr_usage")))
    fn <- str_c(date_time_stamp(), ".csv")
    write_csv(x=log_df, path=file.path("www", "usr_usage", fn))
}

save_usr_input <- function(data_df) {
    stopifnot(dir.exists(file.path("www", "usr_input")))
    fn <- str_c(date_time_stamp(), ".csv")
    write_csv(x=data_df, path=file.path('www', "usr_input", fn))
}
