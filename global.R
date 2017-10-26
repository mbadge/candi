library("dplyr")
library("tibble")
library("magrittr")
library("stringr")
library("rebus")
library("purrr")

library("shinythemes")
library("shinyjs")
library("shiny")

source("text_manipulation.R")

# UI Variables
id_fields <- c("radiologist", "img_id") %>% set_names()
indications <- c("cardiomegaly", "emphysema", "effusion")

# FS
img_dir <- getwd() %>% file.path('www')
img_fns <- list.files(img_dir)

ann_dir <- getwd() %>% file.path('annotations')
class_fp <- file.path(ann_dir, "classification")
seg_fp <- file.path(ann_dir, "segmentation")


save_classification <- function(data) {
    resp_fn <- format(Sys.time(), "%m.%d_%H.%M.%S") %>% str_c(".csv")
    write.csv(x=data, file = file.path(class_fp, resp_fn),
              row.names = FALSE)
}

save_segmentation <- function(data) {
    resp_fn <- format(Sys.time(), "%m.%d_%H.%M.%S") %>% str_c(".csv")
    write.csv(x=data, file=file.path(seg_fp, resp_fn),
              row.names=FALSE)
}

load_classifications <- function() {
    fps <- list.files(class_fp, full.names = TRUE)
    df <- map_dfr(fps, read.csv, stringsAsFactors=FALSE)
    colnames(df) %<>% str_case_periods2snake()
    df
}

load_segmentations <- function() {
    fps <- list.files(seg_fp, full.names=TRUE)
    df <- map_dfr(fps, read.csv, stringsAsFactors=FALSE)
    colnames(df) %<>% str_case_periods2snake()
    df
}
