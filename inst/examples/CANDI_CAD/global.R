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


# Flags -----------------------
kDXS_CHR <- c("cardiomegaly", "emphysema", "effusion")  # Dx options to include in ui checkbox
kINCLUDE_DEMOGRAPHICS <- FALSE
kINCLUDE_TECHNICAL <- FALSE

# fs i/o
kDIR_SMALL_IMGS <- candiOpt(small_img_dir)  # 299 x 299 normalized jpgs
kDIR_BBOX_IMGS <- candiOpt(bbox_img_dir)  # Images annotated with bbox localization

AppDir <- function(...) {
    fp <- file.path(candiOpt(app_data_dir), "candi_cad", ...)
    stopifnot(dir.exists(fp))
    fp
}
kDIR_USR_INPT <- AppDir("usr_input")
kDIR_LOG <- AppDir("log")


# load info tables
data("hist_imgs_df", package="candi")
data("test_imgs_df", package="candi")


# Non-reactive data crafting
test_py_df <- test_imgs_df %>%
    select(img_id, starts_with("pY_")) %>%
    set_colnames(., value = colnames(.) %>% str_replace("pY_", ""))


# Check data.table and image file overlap
small_img_ids <- list.files(kDIR_SMALL_IMGS, pattern="*.jpg$", full.names=TRUE) %>% MyUtils::fp_stem()

# Check whether all test_df images are available
# If not, warn and discard test_df records without an image...
if (any(test_imgs_df$img_id %ni% (small_img_ids))) {
    warning("Not all images available")
    test_imgs_df %<>%
        dplyr::filter(img_id %in% (small_img_ids))
}
# ...and vice versa
if (any(small_img_ids %ni% test_imgs_df$img_id)) {
    warning("There are images with no associated test_df record")
    small_img_ids <- intersect(small_img_ids, test_imgs_df$img_id)
}
kAVAIL_IMG_IDS <- test_imgs_df$img_id



df_filter_trans <- function(df, img_id) {
    df[df$img_id == img_id, ] %>%
        dplyr::select(-img_id) %>%
        AnalysisToolkit::t2idf()
}
