library("candi")
library("readr")
library("EBImage")
library("googlesheets")


# ui config
kID_FIELDS <- c("user_name", "img_id") %>% purrr::set_names(., .)
kDXS_CHR <- c("cardiomegaly", "emphysema", "effusion")
kINCLUDE_DEMOGRAPHICS <- FALSE
kINCLUDE_TECHNICAL <- FALSE

# FLAGS --------------------------------
kSHEET_NAME <- "OpenI IU CXR Public Annotations"


# i/o helper fxns
gs <- gs_title(kSHEET_NAME)
gsURL <- "https://docs.google.com/spreadsheets/d/1J3pDL8h2cv-zHEQobkXBOvtbyU8FSAsmOl9JzTnekFA"


save_annotation <- function(data, ann_type, gS=gs) {
    stopifnot(ann_type %in% candiOpt(annotation_types))
    gs_add_row(gs, ws = ann_type, input=data)
}

load_annotation <- partial(load_gs_annotation, gSpreadSheet = gs)

# load input metadata
data("lut_img_id2original", package = "candi")
iu_db_lut <- lut_img_id2original %>%
    with_sep(filter, case == 2) %>%
    select(img_id, url) %>%
    tibble::deframe()  # OpenI image2url named character vector
