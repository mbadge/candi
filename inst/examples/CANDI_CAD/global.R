library("candi")
library("dplyr")
library("tibble")
library("magrittr")
library("stringr")
library("rebus")
library("purrr")
library("tidyr")
library("ggplot2")
library("forcats")


# Env
options(shiny.reactlog=TRUE)  # allows reactivity inspection in browser with Ctrl-F3


# Flags -----------------------
kDXS_CHR <- c("cardiomegaly", "emphysema", "effusion")  # Dx options to include in ui checkbox
kINCLUDE_DEMOGRAPHICS <- FALSE
kINCLUDE_TECHNICAL <- FALSE
kN_HIST_IMGS <- 100

# fs i/o
kDIR_SMALL_IMGS <- candiOpt(small_img_dir)  # 299 x 299 normalized jpgs
kDIR_BBOX_IMGS <- candiOpt(bbox_img_dir)  # Images annotated with bbox localization

AppDir <- function(...) {
    fp <- file.path(candiOpt(app_data_dir), "candi_cad", ...)
    if(!dir.exists(fp)) stop("No directory found at: ", fp)
    fp
}
kDIR_USR_INPT <- AppDir("usr_input")
kDIR_LOG <- AppDir("log")


# load info tables
data("radiographs", package="candi")
data("cases", package="candi")
data("test_imgs", package = "candi")

# Non-reactive data crafting
test_img_df <- radiographs %>%
    filter(img_id %in% test_imgs) %>%
    with_sep(left_join, cases, by="case")
hist_img_df <- radiographs %>%
    filter(img_id %ni% test_imgs) %>%
    sample_n(kN_HIST_IMGS) %>%
    with_sep(left_join, cases, by="case")


# Check data.table and image file overlap
small_img_ids <- list.files(kDIR_SMALL_IMGS, pattern="*.jpg$", full.names=TRUE) %>% MyUtils::fp_stem()

# Check whether all images are available
# If not, warn and discard records without an image...
if (any(test_imgs %ni% (small_img_ids))) {
    warning("Not all images available")
    test_imgs %<>% intersect(small_img)
}
kAVAIL_TEST_IDS <- test_imgs  # CAD operates by Image



df_filter_trans <- function(df, img_id) {
    df[df$img_id == img_id, ] %>%
        dplyr::select(-img_id) %>%
        AnalysisToolkit::t2idf()
}
