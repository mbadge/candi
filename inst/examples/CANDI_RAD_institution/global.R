library("candi")


# ui config
kID_FIELDS <- c("user_name", "img_id") %>% purrr::set_names()
kDXS_CHR <- c("cardiomegaly", "emphysema", "effusion")
kANNOTATION_TYPES <- c("classification", "segmentation", "clinical_note")

# i/o config
kIMG_DIR <- '/www/app_data_candi/CANDI_RAD_institution/images'
kIMG_FNS <- list.files(kIMG_DIR)

kDIR_USR_INPT <- '/www/app_data_candi/CANDI_RAD_institution/usr_input'
stopifnot(dir.exists(kDIR_USR_INPT))

# i/o helper fxns
save_annotation <- function(data, ann_type) {
    stopifnot(ann_type %in% kANNOTATION_TYPES)
    resp_fn <- format(Sys.time(), "%m.%d_%H.%M.%S") %>% stringr::str_c(".csv")
    write.csv(x=data, file=file.path(kDIR_USR_INPT, ann_type, resp_fn), row.names=FALSE)
}

