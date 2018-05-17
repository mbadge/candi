data(cases, package = "candi")
data(radiographs, package = "candi")

str(cases)
cases[c("age", "sex")] %>% DF.assess_missingness()

# Inject age and gender into the indication when possible

# Explore indication style ----
cases$indication
str_view(cases$indication, pattern = "year.old")

# Check what word comes after yo
str_match_all(cases$indication, "year.old.([a-zA-Z]+)") %>%
    compact() %>%
    map_int(nrow) %>% unique()
str_match_all(cases$indication, "year.old.([a-zA-Z]+)") %>%
    compact() %>%
    lift_dl(rbind)() %>%
    as.df() %>%
    drop_na() %>%
    `$`(V2) %>%
    table()
# I should remove some tags when immediately following year.old:
# XXXX woman female or male presents complaining for with

# What words come before y.o.
str_match_all(cases$indication, "^(.+)year.old") %>%
    compact() %>%
    lift_dl(rbind)() %>%
    as.df() %>%
    drop_na() %>%
    `$`(V2) %>%
    table()
# In general, anything preceding the y.o. has no information content


# Reconcile Indication ----
rm_tbl <- c("woman", "XXXX", "male", "female")
has_extra_word <- str_match_all(cases$indication, "year.old.([a-zA-Z]+)") %>%
    lift_dl(rbind)() %>%
    as.df() %>%
    `$`(V2) %>%
    `%in%`(table = rm_tbl)

has_extra_word <- str_match_all(cases$indication, "year.old.([a-zA-Z]+)") %>%
    map(2) %>%
    map(`%in%`, table = rm_tbl) %>%
    map_lgl(isTRUE)

# From these cases, remove everything before and the word after year.old
cases$indication[has_extra_word] <- cases$indication[has_extra_word] %>%
    str_replace_all("^(.+)year.old.([a-zA-Z]+)", "") %>%
    str_replace_all("^[\\s,.]+", "")

cases$indication[!has_extra_word] <- cases$indication[!has_extra_word] %>%
    str_replace_all("^(.+)year.old.", "") %>%
    str_replace_all("^[\\s,.]+", "")

cases$sex %<>% fct_recode("Female" = "f", "Male" = "m")

cases$indication[is.na(cases$indication)] <- ""
cases$indication[str_detect(cases$indication, "^XXXX\\.?$")] <- ""

HPI <- glue("{cases$age} year old {cases$sex} {cases$indication}")

cases$hpi <- HPI
devtools::use_data(cases, overwrite = TRUE)
