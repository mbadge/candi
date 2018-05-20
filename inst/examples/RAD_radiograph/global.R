library(MyUtils)
suppressPackageStartupMessages(library(candi))
library(magrittr)


# FLAGS ----
kDXS_CHR <- c("cardiomegaly", "emphysema", "effusion") %>% set_names(., str_case_title(.))  #candiOpt(dxs_chr)
kANN_TYPES <- c("Classification", "Segmentation", "ClinicalNote")
kINCLUDE_DEMOGRAPHICS <- FALSE
kINCLUDE_TECHNICAL <- FALSE


# pkg data
data(test_imgs, package = "candi")
data(cases, package = "candi")


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
save_annotation <- function(data, ann_type) {
    stopifnot(ann_type %in% kANN_TYPES)
    resp_fn <- format(Sys.time(), "%m.%d_%H.%M.%S") %>% stringr::str_c(".csv")
    write.csv(x=data, file=file.path(kDIR_USR_INPT, ann_type, resp_fn), row.names=FALSE)
}

load_annotation <- function(ann_type) {
    stopifnot(ann_type %in% kANN_TYPES)
    fps <- list.files(file.path(kDIR_USR_INPT, ann_type), full.names = TRUE)
    df <- map_dfr(fps, read.csv, stringsAsFactors=FALSE)
    df
}

handle_annotation_download <- function(ann_type) {
    downloadHandler(
        filename = function() {glue::glue("{ann_type}_annotations.csv")},
        content = function(file) {write.csv(load_annotation(ann_type), file, row.names=FALSE)},
        contentType="text/csv"
    )
}
