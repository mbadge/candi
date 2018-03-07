library("candi")
library("readr")
library("EBImage")
library("googlesheets")


# ui config
kID_FIELDS <- c("user_name", "img_id") %>% purrr::set_names(., .)
kDXS_CHR <- c("cardiomegaly", "emphysema", "effusion")
kANNOTATION_TYPES <- c("classification", "segmentation", "clinical_note")

# FLAGS --------------------------------
kSHEET_NAME <- "OpenI IU CXR Public Annotations"
kFP_DB_LUT <- "/www/app_data_candi/CANDI_RAD_public/iu_cxr_db.csv"  # mapping from image id to iu download url
stopifnot(file.exists(kFP_DB_LUT))

# i/o helper fxns
gs <- gs_title(kSHEET_NAME)
gsURL <- "https://docs.google.com/spreadsheets/d/1J3pDL8h2cv-zHEQobkXBOvtbyU8FSAsmOl9JzTnekFA"


save_annotation <- function(data, ann_type, gS=gs) {
    stopifnot(ann_type %in% kANNOTATION_TYPES)
    gs_add_row(gS, ws = ann_type, input=data)
}

# load input metadata
iu_db_lut <- read_csv(kFP_DB_LUT) %>%
    tibble::deframe()   # OpenI image2url named character vector
