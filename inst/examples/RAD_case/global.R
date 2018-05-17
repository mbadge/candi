library(MyUtils)
suppressPackageStartupMessages(library(candi))
library(magrittr)

# Env
options(shiny.reactlog=TRUE)  # allows reactivity inspection in browser with Ctrl-F3


# FLAGS ----
kDXS_CHR <- candiOpt(dxs_chr)
kINCLUDE_DEMOGRAPHICS <- TRUE
kINCLUDE_TECHNICAL <- TRUE

# FSIO ----
# pkg data
data(test_imgs, package = "candi")
kDIR_LARGE_IMGS <- candiOpt(large_img_dir)

AppDataDir <- function(...) {
    fp <- file.path(
        candiOpt(app_data_dir),
        compose(str_case_snake, basename, getwd)(),
        ...)
    if (!dir.exists(fp)) stop("No directory found at: ", fp)
    fp
}

kDIR_USR_INPT <- AppDataDir("usr_input")
kDIR_LOG <- AppDataDir("log")    # Dir for non-user input user session data - event logger
# Usr_Inpt is referenced by the application logic to decide the remaining work queue for a user
# log files should only be used in downstream analysis



# Main ----
# Check data.table and image file overlap
large_img_ids <- list.files(kDIR_LARGE_IMGS, pattern = "*.jpg") %>% MyUtils::fp_stem()

# Confirm all test images are available
# If not, warn and remove records without an image...
if (any(test_imgs %ni% (large_img_ids))) {
    warning("Not all images available")
    test_imgs <- intersect(large_img_ids, test_imgs)
}
kAVAIL_IMG_IDS <- test_imgs
