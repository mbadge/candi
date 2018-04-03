# Ingest data for candi learning trial (see gitlab mabadgeley/candi#9)

# Codec (let's just save images under the new img_id system)
# the prepended "iu_" will have no IC but I'll be able to point to the same image files without adjustment
data("lut_img_id2original", package="ProjUtilsRads")


# Original IU scalars table with indication text
kOG_SCALARS <- "inst/extdata/originalIuScalars.csv"
# Imputed values
data("iu_scalars_imputed", package="ProjUtilsRads")
iu_scalars_imputed

# Recode image ids ----
# in original scalars df
og_df <- read_csv_(fp = kOG_SCALARS)
og_df %<>%
    inner_join(lut_img_id2original, by=c("img_id"="original_id"))
og_df %<>% select(-img_id, -case)
og_df %<>% select(img_id = img_id.y, everything())
rm(lut_img_id2original)


# Reconcile Tables ----
# Cols: og has the additional column of interest: indication
imp <- names(iu_scalars_imputed)
og <- names(og_df) %>% str_case_camel(lower = FALSE)
setdiff(imp, og)
setdiff(og, imp)
#! Only OG has indication, findings
#! Only imp has sex

# Rows: imp has 161 images excluded
list("og"=og_df, "imp"=iu_scalars_imputed) %>%
    map("img_id") %>%
    Venn()

# Check missing data
list("og"=og_df, "imp"=iu_scalars_imputed) %>%
    imap(~DF.assess_missingness(df = .x, title=.y))
# after removing unmatched cols
og_df %>%
    select(-indication, -findings) %>%
    DF.assess_missingness()
iu_scalars_imputed %>%
    select(-sex) %>%
    DF.assess_missingness()
#! wtf @ equivalent missingness

# Check if all info is unique by case (that all data is constant within a case except the img_id)
unique_vars <- function(df) {
    df %>%
        sep_id() %>%
        group_by(case) %>%
        summarise_at(.vars = vars(-image), .funs = compose(length, unique)) %>%
        map_int(max) %>%
        keep(.p = ~.x > 1)
}
unique_vars(og_df)
unique_vars(iu_scalars_imputed)

# same results w/ both og and iu imputed: only view and cassette orientation have multiple values w/n a case
# Make a radiographs table with just these cols, and then a cases table with the shared data

# Typing of scalars
dfs <- list("og"=og_df, "imp"=iu_scalars_imputed)
walk(dfs, str_)
(fct_colnms <- iu_scalars_imputed %>% keep(.p=is.factor) %>% names)
map(fct_colnms, ~`[`(iu_scalars_imputed, .x)) %>%
    map(table_)
og_df[c("CassetteOrientation", "ViewPosition")] %>%
    map(table_)
map(fct_colnms, ~`[`(iu_scalars_imputed, .x)) %>%
    map(table_)
# No fct levels removed or disproportionately filtered between data frames

#!? Should I be basing this on the filtered iu_imputed or the full og
#! for the moment, work off of iu_scalars_imputed
og_txt <- og_df %>%
    select(img_id, indication, findings)
scalars <- inner_join(og_txt, iu_scalars_imputed, by="img_id")



# Reframe data schema byCase
radiographs <- scalars[c("img_id", "view", "cassette_orientation")]
cases <- scalars %>%
    select(-view, -cassette_orientation) %>%
    sep_id() %>%
    group_by(case) %>%
    sample_n(1) %>%
    ungroup() %>%
    select(-dataset, -image)

DF.assess_missingness(cases)

devtools::use_data(radiographs)
devtools::use_data(cases)
