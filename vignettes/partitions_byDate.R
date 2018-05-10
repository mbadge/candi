# Evaluate the distribution of cases by date
data(cases, package="candi")


kTARGETS <- candiOpt(dxs_chr)
stopifnot(all(kTARGETS %in% names(cases)))

cases %<>%
    select(case, indication, findings, age, sex, day, one_of(kTARGETS))

# Summarise ----
cases$day %>% hist()

cases %>%
    summarise_at(.vars = kTARGETS, .funs = mean) %>%
    t2idf()

# by partition
day.start_test <- quantile(cases$day, .70)
cases$test_prtn <- cases$day >= day.start_test
cases %>%
    group_by(test_prtn) %>%
    summarise_at(.vars = kTARGETS, .funs = mean) %>%
    t2idf()

# Prevalence by week
week.start_test <- ceiling((day.start_test + 1) / 7)
cases %>%
    mutate(week = ceiling((day + 1) / 7)) %>% select(day, week) %>% arrange(week)

cases_byWeek <- cases %>%
    mutate(week = ceiling((day + 1) / 7)) %>%
    group_by(week)

Ncases_byWeek <- cases_byWeek %>%
    summarise(n=n())
ggplot(Ncases_byWeek, aes(x=week, y=n)) +
    geom_col()

str_(cases_byWeek)
ggplot(cases_byWeek, aes(x=week)) +
    geom_bar()

cases_byWeek %>%
    summarise_at(kTARGETS, mean) %>%
    gather(key = "dx", value="prevalence", -week) %>%
    {
        ggplot(., aes(x=week, y=prevalence, col = dx)) +
            stat_smooth(se=FALSE) +
            geom_vline(xintercept = week.start_test)
    }

cases_byWeek %>%
    select(week, one_of(kTARGETS)) %>%
    gather(key = "dx", value = "is_diagnosed", -week) %>%
    {
        ggplot(., aes(x=week, fill=dx, col=dx)) +

            geom_smooth(data = as_mapper(~group_by(., week, dx) %>% summarise(prev = mean(is_diagnosed))),
                        aes(y = prev), se = FALSE) +
            geom_vline(xintercept = week.start_test) +
            geom_density(aes(group=1), col=NA_character_, fill="black", alpha=0.4)
    }



cases %>%
    group_by(test_prtn) %>%
    summarise_at(.vars = kTARGETS, .funs = mean) %>%
    gather(key = "dx", value = "prev", -test_prtn) %>%
    {
        ggplot(., aes(x=test_prtn, y=prev, col=dx, fill=dx)) + geom_point()
    }

