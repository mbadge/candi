library("candi")


# ui config
id_fields <- c("user_name", "img_id") %>% set_names()
indications <- c("cardiomegaly", "emphysema", "effusion")

# i/o config
img_dir <- getwd() %>% file.path('www/images')
img_fns <- list.files(img_dir)

ann_dir <- getwd() %>% file.path('www/annotations')


# i/o helper fxns
save_annotation <- function(data, ann_type) {
    stopifnot(ann_type %in% c("Classification", "Segmentation", "ClinicalNote"))
    resp_fn <- format(Sys.time(), "%m.%d_%H.%M.%S") %>% str_c(".csv")
    write.csv(x=data, file=file.path(ann_dir, ann_type, resp_fn), row.names=FALSE)
}

load_annotation <- function(ann_type) {
    stopifnot(ann_type %in% c("Classification", "Segmentation", "ClinicalNote"))
    fps <- list.files(file.path(ann_dir, ann_type), full.names = TRUE)
    df <- map_dfr(fps, read.csv, stringsAsFactors=FALSE)
    df
}

handle_annotation_download <- function(ann_type) {
    downloadHandler(
        filename = function() {glue("{ann_type}_annotations.csv")},
        content = function(file) {write.csv(load_annotation(ann_type), file, row.names=FALSE)},
        contentType="text/csv"
    )
}
