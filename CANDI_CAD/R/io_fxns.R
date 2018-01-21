# i/o helper fxns ---------------
save_usr_input <- function(data_df) {
    stopifnot(dir.exists(file.path("www", "usr_input")))
    fn <- str_c(str_c(data_df$username, data_df$test_img_id, sep="-"), ".csv")
    write_csv(x=data_df, path=file.path('www', "usr_input", fn))
}


getNewCaseDf <- function() {
    data.frame(
        test_img_id = sample(x = stem(test_img_fns), size = 1),
        reader_mode = sample(x = c("concurrent", "second"), size = 1)
    )
}

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
