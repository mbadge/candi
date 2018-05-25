library("candi")
library("googlesheets")
library("magrittr")

# FLAGS ----
# ui config
kID_FIELDS <- c("user_name", "img_id") %>% purrr::set_names()
kDXS_CHR <- c("cardiomegaly", "emphysema", "effusion")
kINCLUDE_DEMOGRAPHICS <- FALSE
kINCLUDE_TECHNICAL <- FALSE


kDIR_SMALL_IMGS <- candiOpt(small_img_dir)

AppDataDir <- function(...) {
    fp <- file.path(
        candiOpt(app_data_dir),
        compose(str_case_snake, basename, getwd)(),
        ...)
    if (!dir.exists(fp)) stop("No directory found at: ", fp)
    fp
}


# fsio
kSHEET_NAME <- "Consortia Radiograph Annotations"
gsURL <- "https://docs.google.com/spreadsheets/d/1RXSgxHmcmDXdQjj0fkRn8DId0_yh2IPJA60i23WSDI0"

kFP_SAMPLE_IMGS <- file.path(AppDataDir(), "sample_images.tar.gz")


# ----
gs <- gs_title(kSHEET_NAME)
load_annotation <- partial(load_gs_annotation, gSpreadSheet = gs)


# i/o helper fxns
save_annotation <- function(data, ann_type, gS=gs) {
    stopifnot(ann_type %in% candiOpt(annotation_types))
    gs_add_row(gS, ws = ann_type, input=data)
}

save_segmentation <- function(data, dx) {
    stopifnot(dx %in% kDXS_CHR)
    df <- data %>%
        tibble::add_column(Pathology=dx)
    save_annotation(df, ann_type = "segmentation", gS = gs)
}
