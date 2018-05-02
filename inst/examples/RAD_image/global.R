library(MyUtils)
library(candi)
library(magrittr)

# Env
options(shiny.reactlog=TRUE)  # allows reactivity inspection in browser with Ctrl-F3


# FSIO ----
# pkg data
data(test_df, package="cxrTargetDiff")  # EHR data for test app test cases

kDIR_LARGE_IMGS <- candiOpt(large_img_dir)

AppDir <- function(...) {
    fp <- file.path(candiOpt(app_data_dir), "rad_image", ...)
    stopifnot(dir.exists(fp))
    fp
}
kDIR_USR_INPT <- AppDir("usr_input")
kDIR_LOG <- AppDir("log")    # Dir for non-user input user session data - event logger
# Usr_Inpt is referenced by the application logic to decide the remaining work queue for a user
# log files should only be used in downstream analysis

# FLAGS ----
kDXS_CHR <- candiOpt(dxs_chr)

# Preconditions ----
stopifnot(dir.exists(kDIR_LARGE_IMGS))
stopifnot(all(kDXS_CHR %in% names(test_df)))


# Main ----
# Check data.table and image file overlap
large_img_ids <- list.files(kDIR_LARGE_IMGS, pattern = "*.jpg") %>% MyUtils::fp_stem()

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
}
kAVAIL_IMG_IDS <- intersect(large_img_ids, test_df$img_id)
