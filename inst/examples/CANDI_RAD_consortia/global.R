library("candi")

library("googlesheets")


# ui config
kID_FIELDS <- c("user_name", "img_id") %>% set_names()
kDXS_CHR <- c("cardiomegaly", "emphysema", "effusion")
kANNOTATION_TYPES <- c("classification", "segmentation", "clinical_note")

# FLAGS ----
kSHEET_NAME <- "Consortia Radiograph Annotations"


gs <- gs_title(kSHEET_NAME)
gsURL <- "https://docs.google.com/spreadsheets/d/1RXSgxHmcmDXdQjj0fkRn8DId0_yh2IPJA60i23WSDI0"

# i/o helper fxns
save_annotation <- function(data, ann_type) {
    stopifnot(ann_type %in% kANNOTATION_TYPES)
    gs_add_row(gs, ws = ann_type, input=data)
}