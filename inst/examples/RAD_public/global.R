library("candi")

library("readr")

library("EBImage")
library("googlesheets")


# ui config
kID_FIELDS <- c("user_name", "img_id") %>% set_names(., .)
kDXS_CHR <- c("cardiomegaly", "emphysema", "effusion")
kANNOTATION_TYPES <- c("classification", "segmentation", "clinical_note")

# FLAGS --------------------------------
kSHEET_NAME <- "OpenI IU CXR Public Annotations"


# i/o helper fxns
gs <- gs_title(kSHEET_NAME)
gsURL <- "https://docs.google.com/spreadsheets/d/1J3pDL8h2cv-zHEQobkXBOvtbyU8FSAsmOl9JzTnekFA"


save_annotation <- function(data, sheet) {
    stopifnot(sheet %in% c("Classification", "Segmentation", "ClinicalNote"))
    gs_add_row(gs, sheet, input=data)
}


# load input metadata
iu_db_lut <- read_csv("www/iu_cxr_db.csv") %>%
    deframe()   # OpenI image2url named character vector
