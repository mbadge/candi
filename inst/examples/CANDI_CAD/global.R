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

# fs i/o
kDIR_APP_DATA <- '/www/app_data_candi/CANDI_CAD'
kDIR_SMALL_IMGS <- file.path(kDIR_APP_DATA, 'images')  # 299 x 299 normalized jpgs
kDIR_BBOX_IMGS <- file.path(kDIR_APP_DATA, 'bbox')  # Images annotated with bbox localization
kDIR_USR_INPT <- file.path(kDIR_APP_DATA, 'usr_inpt')

# Check Flags ----
inpt_dirs <- c(kDIR_APP_DATA, kDIR_SMALL_IMGS, kDIR_BBOX_IMGS, kDIR_USR_INPT)
stopifnot(all(map_lgl(inpt_dirs, dir.exists)))

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
