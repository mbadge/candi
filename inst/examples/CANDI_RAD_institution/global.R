library("candi")


# ui config
kID_FIELDS <- c("user_name", "img_id") %>% set_names()
kINDICATIONS <- c("cardiomegaly", "emphysema", "effusion")
kANNOTATION_TYPES <- c("classification", "segmentation", "clinical_note")

# i/o config
img_dir <- getwd() %>% file.path('www/images')
img_fns <- list.files(img_dir)

ann_dir <- getwd() %>% file.path('www/annotations')


# i/o helper fxns
save_annotation <- function(data, ann_type) {
    stopifnot(ann_type %in% kANNOTATION_TYPES)
    resp_fn <- format(Sys.time(), "%m.%d_%H.%M.%S") %>% str_c(".csv")
    write.csv(x=data, file=file.path(ann_dir, ann_type, resp_fn), row.names=FALSE)
}

