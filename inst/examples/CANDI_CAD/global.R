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


df_filter_trans <- function(df, img_id) {
    df[df$img_id == img_id, ] %>%
        dplyr::select(-img_id) %>%
        AnalysisToolkit::t2idf()
}
