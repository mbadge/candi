source('global.R')

img_ids <- kIMG_FNS %>%
    str_sub(end=-5)

# Simulate Inference
pY_df <- rerun(.n=length(kDXS_CHR), runif(n=length(img_ids)) %>% round(digits = 2)) %>%
    set_names(str_c("pY", kDXS_CHR, sep="_")) %>%
    as.data.frame()
PC_df <- rerun(.n=10, runif(n=length(img_ids))) %>%
    set_names(str_c("PC", 1:10, sep="_")) %>%
    as.data.frame()
cnn_inference_df <- bind_cols(pY_df, PC_df) %>%
    tibble::add_column(img_id = img_ids, .before=1)
write_csv(cnn_inference_df, path = "www/cnn_inference.csv")


# Read and subset IU scalars
iu_df <- read_csv(file = .FS$PRO %>% file.path('data', 'datasets', 'iu_cxr', 'scalars.csv'))
iu_df %<>%
    filter(img_id %in% img_ids) %>%
    select(img_id:findings)
Ys_df <- read_csv(file = .FS$PRO %>% file.path('data', 'scalars.csv'))
Ys_df %<>%
    filter(img_id %in% img_ids) %>%
    select(img_id, one_of(str_to_title(kDXS_CHR)))
image_records_df <-inner_join(iu_df, Ys_df)

write_csv(image_records_df, path = "www/image_records.csv")
