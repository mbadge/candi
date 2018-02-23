# Run once from desktop ubuntu to build example_input_data folder to support all candi apps and
# scp to public remotes
PROJECT_DIR <- Sys.getenv('PROJECT_DIR')

# FLAGS ----
kIU_SCALARS_FP <- file.path(PROJECT_DIR, 'data', 'datasets', 'iu_cxr', 'scalars.csv')
# ----
data(PCsNihIu_pretrainedFeat_nihFit, package="ProjUtils")  # test images cnn data
iu_scalars_df <- read_csv(kIU_SCALARS_FP)  # historical image data

# Test Images ----
# CNN predictions: btlnck_pcs, pYs
test_img_ids <- img_fns %>% stem()
test_img_pcs <- PCsNihIu_pretrainedFeat_nihFit %>%
    filter(img_id %in% test_img_ids)

#! TODO: snag dl predictions on iu data
test_img_pys <- rerun(.n=length(kDXS_CHR), runif(n=length(test_img_ids)) %>% round(digits = 2)) %>%
    set_names(str_c("pY", kDXS_CHR, sep="_")) %>%
    as.data.frame() %>%
    add_column(img_id = test_img_ids)

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
img_fns <- list.files(IMG_DIR)

f_patterns <- img_fns %>%
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

