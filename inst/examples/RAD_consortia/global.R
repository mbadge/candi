library("candi")
library("googlesheets")
library("magrittr")

# FLAGS ----
# ui config
kID_FIELDS <- c("user_name", "img_id") %>% purrr::set_names()
kDXS_CHR <- c("cardiomegaly", "emphysema", "effusion")
kANNOTATION_TYPES <- c("classification", "segmentation", "clinical_note")

# fsio
kFP_SAMPLE_IMGS <- "/www/app_data_candi/CANDI_RAD_consortia/sample_images.tar.gz"
kSHEET_NAME <- "Consortia Radiograph Annotations"
gsURL <- "https://docs.google.com/spreadsheets/d/1RXSgxHmcmDXdQjj0fkRn8DId0_yh2IPJA60i23WSDI0"

# ----
gs <- gs_title(kSHEET_NAME)

# i/o helper fxns
save_annotation <- function(data, ann_type, gS=gs) {
    stopifnot(ann_type %in% kANNOTATION_TYPES)
    gs_add_row(gS, ws = ann_type, input=data)
}