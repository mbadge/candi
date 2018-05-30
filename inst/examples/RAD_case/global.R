library(MyUtils)
suppressPackageStartupMessages(library(candi))
library(magrittr)

# Env
options(shiny.reactlog=TRUE)  # allows reactivity inspection in browser with Ctrl-F3


# FLAGS ----
kDXS_CHR <- candiOpt(dxs_chr)
kINCLUDE_DEMOGRAPHICS <- FALSE
kINCLUDE_TECHNICAL <- FALSE


# pkg data
data(test_imgs, package = "candi")
data(cases, package = "candi")

help_fp <- system.file('examples', 'RAD_case', 'instructions.txt', package="candi", mustWork = TRUE)
kHELP_TXT <- readLines(help_fp)


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

kAVAIL_TEST_IDS <- id_2Case(test_imgs)  # TEST BY CASE
cases %<>% filter(case %in% kAVAIL_TEST_IDS)  # Filter cases to only test cases


df_filter_trans <- function(df, case) {
    df[df$case == case, ] %>%
        dplyr::select(-case) %>%
        AnalysisToolkit::t2idf()
}
