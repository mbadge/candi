library(MyUtils)
suppressPackageStartupMessages(library(candi))
library(magrittr)


# FLAGS ----
kDXS_CHR <- c("cardiomegaly", "emphysema", "effusion") %>% purrr::set_names(., str_case_title(.))  #candiOpt(dxs_chr)
kANN_TYPES <- candiOpt(annotation_types)
kINCLUDE_DEMOGRAPHICS <- FALSE
kINCLUDE_TECHNICAL <- FALSE


# pkg data
data(test_imgs, package = "candi")
data(cases, package = "candi")

help_fp <- system.file('examples', 'RAD_institution', 'instructions.txt', package="candi", mustWork = TRUE)
kHELP_TXT <- readLines(help_fp)

kDIR_SMALL_IMGS <- candiOpt(small_img_dir)

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
small_img_ids <- list.files(kDIR_SMALL_IMGS, pattern = "*.jpg") %>% MyUtils::fp_stem()

# Confirm all test images are available
# If not, warn and remove records without an image...
if (any(test_imgs %ni% (small_img_ids))) {
    warning("Not all images available")
    test_imgs <- intersect(small_img_ids, test_imgs)
}

kAVAIL_TEST_IDS <- test_imgs
cases %<>% filter(case %in% id_2Case(kAVAIL_TEST_IDS))  # Filter cases to only test cases


df_filter_trans <- function(df, case) {
    df[df$case == case, ] %>%
        dplyr::select(-case) %>%
        AnalysisToolkit::t2idf()
}


# i/o helper fxns
#! IMPURE FXNS: use of global variables
save_segmentation <- function(data, dx) {
    stopifnot(dx %in% kDXS_CHR)
    df <- data %>%
        tibble::add_column(Pathology=dx)
    save_usr_input(df, dir = kDIR_USR_INPT, subdir = "segmentation")
}

load_annotation <- partial(load_csv_annotation, dir = kDIR_USR_INPT)
