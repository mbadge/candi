# Run once from desktop ubuntu to build cnn_inference folder to support all candi apps and
# scp to public remotes

library(AnalysisToolkit)
library(MlAnalysis)

# FLAGS ----
#kDXS_CHR <- candiOpt(dxs_chr)

# classification models
# kMODEL_FP <- "/media/marcus/Vulcan/radiology/cxrRoundRobin/iter_train_dataset_combo_X_predictor_set_batches/iuMshNih_all_train_by_trainDatasetCombo_X_predictorSet_model_lst.Rdata"
#
#
# load(kMODEL_FP)
# # Use the full predictor set since it has the highest perf
# mymodel_lst <- model_lst %>%
#     `[`(str_detect(names(.), pattern = "full"))
# save(mymodel_lst, file = "inst/extdata/models.Rdata")
load("inst/extdata/models.Rdata")

carets <- mymodel_lst %>% map(`@`, caret)

# expect error if names aren't aligned
map(carets, "finalModel") %>%
    map(predict, type = "response") %>%
    map_dfr(names) %>%
    apply(MARGIN=1, FUN=unique)


# Extract predicitons on training data
pYs <- map(carets, "finalModel") %>%
    map_dfc(predict, type = "response")

# Label imgs
img_ids <- pYs$iuMshNih_Cardiomegaly_full %>% names()
pYs %<>%
    map_dfc(unname)
pYs$img_id <-img_ids

# Filter to just IU
pYs %<>%
    with_sep(filter, dataset == "iu")

# Set col names
names(pYs) %<>% str_replace("iuMshNih_", "") %>% str_replace("_full", "")
names(pYs) %<>% str_c("pY_", .)
names(pYs) %<>% str_replace("pY_img_id", "img_id")

# merge with btlncks
kBTLNCKS_PTH <- "inst/extdata/pcs_iuFit.rds"
btlnck_df <- readRDS(kBTLNCKS_PTH)

df <- btlnck_df %>%
    with_sep(filter, dataset == "iu") %>%
    inner_join(pYs, by="img_id")

# Merge with radiographs data
data(radiographs)
radiographs %<>%
    left_join(df, by="img_id")



# Save to pkg

#!!! PATCH DX NAMES
stop()  # I hacked this, it won't run w/n the full script
cnn_pY_dxs <- names(radiographs) %>%
    str_subset(pattern = "pY_") %>%
    str_replace("pY_", "")
pkg_dxs <- candiOpt(dxs_chr)
lift_ld(Venn)(cnn_pY_dxs, pkg_dxs)


cnn_pY_dxs
pkg_dxs

names(radiographs)[19] <- "pY_PulmonaryAtelectasis"
names(radiographs)[16] <- "pY_PleuralEffusion"
names(radiographs)[21] <- "pY_PulmonaryEdema"
names(radiographs)[17] <- "pY_HerniaHiatal"


#devtools::use_data(radiographs, overwrite = TRUE)

