library(MyUtils)
library(magrittr)

# Env
options(shiny.reactlog=TRUE)  # allows reactivity inspection in browser with Ctrl-F3


# Flags -----------------------
# pkg data
data(test_df, package="cxrTargetDiff")
data(dx_df, package="cxrTargetDiff")

# fsio
kDIR_USR_INPT <- "/www/app_data/app_data_cxrTargetDiff/usr_inpt/target_difficulty_trial"  # Directory with user input records
kDIR_LARGE_IMGS <- "/www/app_data/app_data_cxrTargetDiff/large_jpgs/"

# Dir for non-user input user session data; event logger with timestamps
kDIR_USR_LOG <- file.path(kDIR_USR_INPT, "log")
# Usr_Inpt is referenced by the application logic, whereas log files should only be used in downstream analysis

# Preconditions ----
stopifnot(dir.exists(kDIR_USR_INPT),
          dir.exists(kDIR_LARGE_IMGS),
          dir.exists(kDIR_USR_LOG))


# Main ----
# use diagnoses from pkg data dx_df for user impression ui
kDXS_CHR <- dx_df$targets
kDXS_CHR %<>% purrr::set_names(., MyUtils::str_case_title(.))
rm(dx_df)

# Check data.table and image file overlap
large_img_ids <- list.files(kDIR_LARGE_IMGS, pattern = "*.jpg", full.names=TRUE) %>% MyUtils::fp_stem()

# Check whether all test_df images are available
# If not, warn and discard test_df records without an image...
if (any(test_df$img_id %ni% (large_img_ids))) {
    warning("Not all images available")
    test_df %<>%
        dplyr::filter(img_id %in% (large_img_ids))
}
# ...and vice versa
if (any(large_img_ids %ni% test_df$img_id)) {
    warning("There are excess images with no associated test_df record")
    large_img_ids <- intersect(large_img_ids, test_df$img_id)
}
kAVAIL_IMG_IDS <- test_df$img_id

