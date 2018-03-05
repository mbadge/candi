library("candi")
library("googlesheets")


# FLAGS ----
# ui config
kID_FIELDS <- c("user_name", "img_id") %>% set_names()
kDXS_CHR <- c("cardiomegaly", "emphysema", "effusion")
kANNOTATION_TYPES <- c("classification", "segmentation", "clinical_note")

# fsio
kSHEET_NAME <- "Consortia Radiograph Annotations"
gsURL <- "https://docs.google.com/spreadsheets/d/1RXSgxHmcmDXdQjj0fkRn8DId0_yh2IPJA60i23WSDI0"

# ----
gs <- gs_title(kSHEET_NAME)

# i/o helper fxns
save_annotation <- function(data, ann_type, gs=gs) {
    stopifnot(ann_type %in% kANNOTATION_TYPES)
    gs_add_row(gs, ws = ann_type, input=data)
}
