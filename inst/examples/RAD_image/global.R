library(MyUtils)
library(candi)
library(magrittr)

# Env
options(shiny.reactlog=TRUE)  # allows reactivity inspection in browser with Ctrl-F3


# FSIO ----
# pkg data
data(test_imgs, package = "candi")

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
kINCLUDE_DEMOGRAPHICS <- TRUE
kINCLUDE_TECHNICAL <- TRUE

# Preconditions ----
stopifnot(dir.exists(kDIR_LARGE_IMGS))


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
