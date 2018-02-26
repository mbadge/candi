library("candi")


# ui config
kID_FIELDS <- c("user_name", "img_id") %>% set_names()
kDXS_CHR <- c("cardiomegaly", "emphysema", "effusion")
kANNOTATION_TYPES <- c("classification", "segmentation", "clinical_note")

# i/o config
kIMG_DIR <- getwd() %>% file.path('www/images')
kIMG_FNS <- list.files(kIMG_DIR)

kUSR_INPT_OUT_DIR <- 'usr_input'


# i/o helper fxns
save_annotation <- function(data, ann_type) {
    stopifnot(ann_type %in% kANNOTATION_TYPES)
    resp_fn <- format(Sys.time(), "%m.%d_%H.%M.%S") %>% str_c(".csv")
    write.csv(x=data, file=file.path(kUSR_INPT_OUT_DIR, ann_type, resp_fn), row.names=FALSE)
}

