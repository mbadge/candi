library(candi)
library(lubridate)


kUSER <- "Marcus"

ann_df <- load_usr_input(user = kUSER, dir = file.path(candiOpt(app_data_dir), "rad_case", "usr_input"))
log_df <- load_usr_input(user = kUSER, dir = file.path(candiOpt(app_data_dir), "rad_case", "log"))

# Caseload
dx_df <- ann_df %>%
    tidyr::separate_rows(pathologies, sep = ", ") %>%
    dplyr::select(-timestamp, -user_name, -clinical_note) %>%
    tibble::add_column(dx = TRUE) %>%
    tidyr::complete(img_id, pathologies, fill = list(dx = FALSE)) %>%
    tidyr::spread(key = pathologies, value = dx)
dx_df %<>% rename("Normal" = `<NA>`)


N_cases <- nrow(dx_df)
cat(glue("You've annotated {N_cases} cases.  In these cases the frequency of each finding was:"))
summarise_all(dx_df[, 2:ncol(dx_df)], mean) %>%
    t2idf(name = "finding", value = "prevalence") %>%
    arrange(desc(prevalence))

# Speed stats
log_df %<>% arrange(timestamp)
log_df$delta_t <- log_df$timestamp - lag(log_df$timestamp)
log_df %<>% filter(!is.na(img_id))

log_df$delta_t %<>% seconds() %>% as.integer()
log_df$img_id %<>% as.character()

glue_data(.x = mean_se(log_df$delta_t),
          "You took an average of {y} seconds to interpret each image (std err {round(ymin, 1)} - {round(ymax, 1)}).")

ggplot(log_df, aes(x=delta_t)) + geom_histogram(bins = N_cases / 2) +
    labs(
        title = "Distribution of Interpretation Durations",
        x = "Interpretation Time (seconds)",
        y = "Number of cases",
        subtitle = glue("Data from {kUSER}'s {nrow(log_df)} interpretations on {compose(stringFunc, unique, date)(log_df$timestamp)}")
    )


ggplot(log_df, aes(x = img_id, y=delta_t)) +
    geom_point() +
    geom_line(group = 1) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) +
    labs(
        title = "Time Spent Annotating Cases",
        x = "Case Number",
        y = "Interpretation Time (seconds)",
        subtitle = glue("Data from {kUSER}'s {nrow(log_df)} interpretations on {compose(stringFunc, unique, date)(log_df$timestamp)}")
    )
