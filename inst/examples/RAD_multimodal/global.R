library(MyUtils)
library(magrittr)
library(candi)

# Env
options(shiny.reactlog=TRUE)  # allows reactivity inspection in browser with Ctrl-F3


# Flags -----------------------
kINCLUDE_DEMOGRAPHICS <- TRUE
kINCLUDE_TECHNICAL <- TRUE


# pkg data
data(test_imgs, package = "candi")
data(cases, package = "candi")

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
kDXS_CHR <- candiOpt(dxs_chr)
kEMR_DEMOGRAPHICS <- c("age", "sex")
kEMR_NOTE <- c("findings")


# Check Flags ----
stopifnot(all(purrr::map_lgl(kEMR_DEMOGRAPHICS, `%in%`, table=colnames(cases))),
          all(purrr::map_lgl(kEMR_NOTE, `%in%`, table=colnames(cases))))


# Check data.table and image file overlap
large_img_ids <- list.files(kDIR_LARGE_IMGS, pattern = "*.jpg", full.names=TRUE) %>% MyUtils::fp_stem()

# Check whether all test images are available
# If not, warn and discard records without an image...
if (any(test_imgs %ni% (large_img_ids))) {
    warning("Not all images available")
    test_imgs <- intersect(large_img_ids, test_imgs)
}
kAVAIL_IMG_IDS <- test_imgs
cases %<>% filter(case %in% imgIds2Cases(test_imgs))

df_filter_trans <- function(df, case) {
    df[df$case == case, ] %>%
        dplyr::select(-case) %>%
        AnalysisToolkit::t2idf()
}
