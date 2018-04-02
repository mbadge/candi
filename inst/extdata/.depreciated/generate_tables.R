# Run once from desktop ubuntu to build example_input_data folder to support all candi apps and
# scp to public remotes
library(AnalysisToolkit)
library(MlAnalysis)

# FLAGS ----
kDXS_CHR <- c("cardiomegaly", "emphysema", "effusion")

# fsio
# Imgs
kIMG_DIR <- "/media/marcus/Projects/radiology/data/images"

# CNN inference
kPC_DIR <- "/media/marcus/Projects/radiology/data/dl/pretrained/pcs_iuFit.rds"  # Image PCs fit to IU
kBBOX_DIR <- "/media/marcus/Vulcan/radiology/iu_cxr/bbox_inference/"  # IU BBox inference

# Data from other pkgs
# data("lut_img_id2original", package="ProjUtilsRads")
# attr(lut_img_id2original, "spec") <- NULL
# codec_imgId <- lut_img_id2original
# devtools::use_data(codec_imgId)

data(scalars_df, package="ProjUtilsRads")

#! CANDI CAD needs the most input data
#! CANDI_INSTITUTIONAL: needs 50 msh input images
# CANDI_CONSORTIA: nothing needed; prompts user for upload
# CANDI_PUBLIC: need image id 2 url map to fetch images from openI RESTFUL API


# MODELS ----
kMODEL_FP <- "/media/marcus/Vulcan/radiology/cxrRoundRobin/iter_train_dataset_combo_X_predictor_set_batches/iuMshNih_all_train_by_trainDatasetCombo_X_predictorSet_model_lst.Rdata"
kTRAIN_PARAMS_FP <- "/media/marcus/Projects/cxrRoundRobin/analysis/ml/iter_train_dataset_combo_X_predictor_set/batches/iuMshNih_train_params.csv"
# Load and semi_join
load(kMODEL_FP)
mymodels.info <- read_csv(kTRAIN_PARAMS_FP)

# Use the full predictor set since it has the highest perf
mymodel_lst <- model_lst %>%
    `[`(str_detect(names(.), pattern = "full"))

# Prime plot data ----
prepend_model_info <- function(df, info_df=mymodels.info, join_col="model_id") {
    stopifnot(join_col %in% names(df))

    res <- left_join(df, info_df, by=join_col)
    res <- select(res, one_of(names(info_df)),
                  one_of(names(df)))
    stopifnot(names(res)[1L] == join_col)
    res[-1L]  # Remove key col
}
prepend_model_gg <- partial(prepend_model_info, info_df=model.gg)

# Curate model frame ----
# craft and derive addition variables for tabulation
kMODEL_NAME_PIECES <- c("trainDatasetCombo", "indication", "predictors")
mymodels.info <- names(cCs) %>%
    str_split("_", n=length(kMODEL_NAME_PIECES), simplify = TRUE) %>% as.df %>%
    set_names(., kMODEL_NAME_PIECES) %>%
    select(indication, predictors) %>%
    tibble::add_column(model_id = names(cCs), .before = 1)

# make 2 plotting tables of interest and augmented labels table ----
# Performance Table ----
bare_perf_tbl <- (lift_dl(rbind))(map(cCs, glance_ClassifierCurve))
bare_perf_tbl %<>%
    tibble::rownames_to_column(var="model_id")
# Version used in kable table
perf_tbl <- bare_perf_tbl %>%
    prepend_model_info() %>%
    select(-predictors) %>%
    arrange(desc(auc))
# SAVE ----
devtools::use_data(perf_tbl)

## LABEL FRAMES TO EMBED AND SAVE
tidy_cCs <- map(cCs, tidy_ClassifierCurve)



# Roc
bare_roc_tbl <- map_dfr(cCs, roc, .id="model_id")
abridged_roc_tbl <- bare_roc_tbl %>%
    select(-alpha) %>%
    split(.$model) %>%
    map_if(.p = ~nrow(.x) > 1000, .f = ~sample_n(.x, size = 1000)) %>%
    map(~arrange(.x, x)) %>%
    lift_dl(bind_rows)(.)
abridged_roc_tbl %<>%
    prepend_model_info() %>%
    select(-predictors)

# SAVE ----
roc_tbl <- gg_roc_tbl
devtools::use_data(roc_tbl)



# --------------------------------- IMAGES ------------------------------------------
# Test Images ----
# CNN predictions: btlnck_pcs, pYs
kIMG_FNS <- list.files(kIMG_DIR, pattern="*.jpg")
nih_fns <- kIMG_FNS %>% str_subset("^nih_")
iu_fns <- kIMG_FNS %>% str_subset("^iu_")



test_img_ids <- kIMG_FNS %>% stem()
test_img_pcs <- PCsNihIu_pretrainedFeat_nihFit %>%
    filter(img_id %in% test_img_ids)

#! TODO: snag dl predictions on iu data
test_img_pys <- rerun(.n=length(kDXS_CHR), runif(n=length(fp_stem(kIMG_FNS))) %>% round(digits = 2)) %>%
    set_names(str_c("pY", kDXS_CHR, sep="_")) %>%
    as.data.frame() %>%
    tibble::add_column(img_id = fp_stem(kIMG_FNS))

test_imgs <- inner_join(test_img_pcs, test_img_pys, by="img_id")
write_csv(test_imgs, path="www/test_images.csv")

# Historical Images ----
historical_imgs <- iu_scalars_df %>%
    filter(img_id %ni% test_img_ids) %>%
    select(-original_id) %>%
    rename(Effusion=PleuralEffusion) %>%
    left_join(PCsNihIu_pretrainedFeat_nihFit, by="img_id")
stopifnot(str_case_camel(kDXS_CHR, lower=FALSE) %in% names(historical_imgs))
write_csv(historical_imgs, path="www/historical_images.csv")


# BBoxs ----
IMG_DIR <- "V://radiology/iu_cxr/bbox_inference/iu_inference"
SHINY_BBOX_DIR <- "www/bbox"
kIMG_FNS <- list.files(IMG_DIR)

f_patterns <- kIMG_FNS %>%
    str_match("result_" %R%
                  capture(one_or_more(DGT) %R% "_IM-" %R% one_or_more(DGT) %R%
                              "-" %R% one_or_more(DGT) %R% zero_or_more("-" %R% one_or_more(DGT))) %R%
                  ".jpg") %>%
    as.df %>%
    inner_join(iu_scalars_df, by=c("V2" = "original_id")) %>%
    select(from_fn=V1, to_stem=img_id)

walk2(f_patterns$from_fn, f_patterns$to_stem,
      ~ file.copy(from = file.path(IMG_DIR, .x),
                  to = file.path(SHINY_BBOX_DIR, str_c(.y, ".jpg")))
)
apply(f_patterns, MARGIN = 1, FUN = function(row) copy_file(from_fn = row["from_fn"], to_stem = row["to_stem"]))



# Pkg data ----
# fs i/o
# Source IU BBox inference
kDIR_BBOX_ORIG <- "/media/marcus/Vulcan/radiology/iu_cxr/bbox_inference/"

kDIR_APP_DATA <- '/www/app_data_candi/CANDI_CAD'
kDIR_SMALL_IMGS <- file.path(kDIR_APP_DATA, 'images')  # 299 x 299 normalized jpgs
kDIR_BBOX_IMGS <- file.path(kDIR_APP_DATA, 'bbox')  # Images annotated with bbox localization
kFP_TEST <- file.path(kDIR_APP_DATA, 'test_images.csv')
kFP_HIST <- file.path(kDIR_APP_DATA, 'historical_images.csv')

# Check Flags ----
inpt_dirs <- c(kDIR_APP_DATA, kDIR_SMALL_IMGS, kDIR_BBOX_IMGS)
stopifnot(all(map_lgl(inpt_dirs, dir.exists)))
inpt_fps <- c(kFP_TEST, kFP_HIST)
stopifnot(all(map_lgl(inpt_fps, file.exists)))

# find available images
avail_input_ids <- list.files(kDIR_SMALL_IMGS, pattern = ".jpg") %>% MyUtils::fp_stem()
avail_bbox_ids <- list.files(kDIR_BBOX_IMGS, pattern= ".jpg") %>% MyUtils::fp_stem()


# load info tables ----
# CNN provides probability of each diagnosis, as well as PCs used to compute image similarity scores
test_imgs_df <- suppressMessages(read_csv(kFP_TEST))
hist_imgs_df <- suppressMessages(read_csv(kFP_HIST))

attr(test_imgs_df, "spec") <- NULL
attr(hist_imgs_df, "spec") <- NULL

# QA ----
# there should be no img_id overlap between hist and test tables
stopifnot(purrr::is_empty(purrr::map(list(test_imgs_df, hist_imgs_df), "img_id") %>%
    purrr::lift_dl(intersect)(.)))

# find available images
avail_input_imgs <- list.files(kDIR_SMALL_IMGS, pattern = ".jpg") %>% MyUtils::fp_stem()
avail_bbox_ids <- list.files(kDIR_BBOX_IMGS, pattern= ".jpg") %>% MyUtils::fp_stem()
avail_bbox_orig <- list.files(kDIR_BBOX_ORIG, pattern= ".jpg") %>% MyUtils::fp_stem()

# there should be perfect overlap between available bbox images and test
stopifnot(all(purrr::map_lgl(test_imgs_df$img_id, `%in%`, table = avail_bbox_ids)))
#! FAIL!
# Move bbox of all test images from kDIR_BBOX_IMGS to kDIR_BBOX_IMGS
# avail_bbox_orig
# data("codec_imgId", package="candi")
# bbox_fp_orig <-list.files(kDIR_BBOX_ORIG, pattern=".jpg", full.names = TRUE)
#
# bbox_fp_dest <- file.path(kDIR_BBOX_ORIG, fn_dest)
# map2(bbox_fp_orig, bbox_fp_dest, file.rename)
# fps_in_test_bbox <- file.path(kDIR_BBOX_ORIG, str_c(test_imgs_df$img_id, ".jpg"))
# stopifnot(all(map_lgl(fps_in_test_bbox, file.exists)))
# fps_out_test_bbox <- file.path(kDIR_BBOX_IMGS, str_c(test_imgs_df$img_id, ".jpg"))
#
# map2(fps_in_test_bbox, fps_out_test_bbox, file.copy)



# there should be perfect overlap between available images and hist
img_ids <- map(list(hist_imgs_df, test_imgs_df), "img_id") %>% unlist
# stopifnot(all(img_ids %in% avail_input_imgs))
#! FAIL - only 1315 images are currently in the app folder

hist_imgs_df %<>%
    filter(img_id %in% avail_input_imgs)
img_ids <- map(list(hist_imgs_df, test_imgs_df), "img_id") %>% unlist
stopifnot(all(img_ids %in% avail_input_imgs))


# Craft ----
hist_imgs_df %<>%
    mutate_at(.vars=vars(sex, view, cassette_orientation), .f=as.factor)


# Save to pkg ----
# devtools::use_data(hist_imgs_df, overwrite = TRUE)
# devtools::use_data(test_imgs_df, overwrite = TRUE)
