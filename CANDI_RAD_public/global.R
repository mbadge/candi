library("purrr")
library("dplyr")
library("tibble")
library("magrittr")
library("stringr")
library("rebus")
library("glue")
library("readr")

library("shinythemes")
library("shinyjs")
library("shiny")

library("EBImage")
library("googlesheets")


# FLAGS --------------------------------
id_fields <- c("user_name", "img_id") %>% set_names(., .)
indications <- c("cardiomegaly", "emphysema", "effusion")
# output config
gSHEET_NAME <- "OpenI IU CXR Public Annotations"
gsURL <- "https://docs.google.com/spreadsheets/d/1J3pDL8h2cv-zHEQobkXBOvtbyU8FSAsmOl9JzTnekFA"



# i/o helper fxns
gs <- gs_title(gSHEET_NAME)
save_annotation <- function(data, sheet) {
    stopifnot(sheet %in% c("Classification", "Segmentation", "ClinicalNote"))
    gs_add_row(gs, sheet, input=data)
}
load_annotation <- function(sheet) {
    stopifnot(sheet %in% c("Classification", "Segmentation", "ClinicalNote"))
    gs_read(gs, sheet)
}
handle_annotation_download <- function(ann_type) {
    downloadHandler(
        filename = function() {glue("{ann_type}_annotations.csv")},
        content = function(file) {
            write.csv(load_annotation(ann_type), file, row.names=FALSE)},
        contentType="text/csv"
    )
}

# load input metadata
iu_db_lut <- read_csv("www/iu_cxr_db.csv") %>%
    deframe()   # OpenI image2url named character vector
