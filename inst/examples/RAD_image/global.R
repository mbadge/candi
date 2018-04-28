library(MyUtils)
library(candi)
library(magrittr)

# Env
options(shiny.reactlog=TRUE)  # allows reactivity inspection in browser with Ctrl-F3


# FSIO ----
# pkg data
data(test_df, package="cxrTargetDiff")  # EHR data for test app test cases

kDIR_LARGE_IMGS <- candiOpt(large_img_dir)
kDIR_USR_INPT <- candiOpt(usr_input_dir)
kDIR_USR_LOG <- file.path(kDIR_USR_INPT, "log")  # Dir for non-user input user session data - timestamped event logger
# Usr_Inpt is referenced by the application logic, whereas log files should only be used in downstream analysis

# FLAGS ----
kDXS_CHR <- c('Cardiomegaly', 'Emphysema', 'PleuralEffusion',
              'HerniaHiatal', 'Nodule', 'PulmonaryAtelectasis',
              'Pneumonia', 'PulmonaryEdema', 'Consolidation',
              'CathetersIndwelling', 'TechnicalQualityOfImageUnsatisfactory',
              'LungHypoinflation', 'LungHyperdistention')

# Preconditions ----
stopifnot(dir.exists(kDIR_USR_INPT),
          dir.exists(kDIR_LARGE_IMGS),
          dir.exists(kDIR_USR_LOG))
stopifnot(all(kDXS_CHR %in% names(test_df)))


# Main ----
# use diagnoses from pkg data dx_df for user impression ui
kDXS_CHR %<>% purrr::set_names(., MyUtils::str_case_title(.))

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

