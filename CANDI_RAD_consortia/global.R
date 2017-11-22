library("dplyr")
library("tibble")
library("magrittr")
library("stringr")
library("rebus")
library("purrr")

library("shinythemes")
library("shinyjs")
library("shiny")

library("googlesheets")


# ui config
id_fields <- c("user_name", "img_id") %>% set_names()
indications <- c("cardiomegaly", "emphysema", "effusion")

# output config
gSHEET_NAME <- "Consortia Radiograph Annotations"
gs <- gs_title(gSHEET_NAME)
gsURL <- "https://docs.google.com/spreadsheets/d/1RXSgxHmcmDXdQjj0fkRn8DId0_yh2IPJA60i23WSDI0"

# i/o helper fxns
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
        content = function(file) {write.csv(load_annotation(ann_type), file, row.names=FALSE)},
        contentType="text/csv"
    )
}
