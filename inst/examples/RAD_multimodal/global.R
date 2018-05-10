library(MyUtils)
library(magrittr)
library(candi)

# Flags -----------------------
# pkg data
data(test_df, package="cxrTargetDiff")

# fsio
kDIR_LARGE_IMGS <- candiOpt(large_img_dir)

AppDir <- function(...) {
    fp <- file.path(candiOpt(app_data_dir), "rad_multimodal", ...)
    stopifnot(dir.exists(fp))
    fp
}

kDIR_USR_INPT <- AppDir("usr_input")
kDIR_LOG <- AppDir("log")


# medical record components
kEMR_DEMOGRAPHICS <- c("age", "sex", "view", "cassette_orientation")
kEMR_NOTE <- c("findings")

# Check Flags ----
stopifnot(all(purrr::map_lgl(kEMR_DEMOGRAPHICS, `%in%`, table=colnames(test_df))),
          all(purrr::map_lgl(kEMR_NOTE, `%in%`, table=colnames(test_df))))

# Main ----
kDXS_CHR <- candiOpt(dxs_chr)
kINCLUDE_DEMOGRAPHICS <- TRUE
kINCLUDE_TECHNICAL <- TRUE

# Check data.table and image file overlap
large_img_ids <- list.files(kDIR_LARGE_IMGS, pattern = "*.jpg", full.names=TRUE) %>% MyUtils::fp_stem()

# Check whether all test_df images are available
# If not, warn and discard test_df records without an image...
if (any(test_df$img_id %ni% (large_img_ids))) {
    warning("Not all images available")
    test_df %<>%
        dplyr::filter(img_id %in% (large_img_ids))
}
# ...and vice versa
if (any(large_img_ids %ni% test_df$img_id)) {
    warning("There are images with no associated test_df record")
}

kAVAIL_IMG_IDS <- test_df$img_id

df_filter_trans <- function(df, img_id) {
    df[df$img_id == img_id, ] %>%
        dplyr::select(-img_id) %>%
        AnalysisToolkit::t2idf()
}
