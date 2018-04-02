# Copied from cxrTargetDiff multi-img @4645b18

devtools::load_all()

kDIR_IN_SMALL_IMAGES <- "/media/marcus/Projects/radiology/data/images"
kDIR_IN_LARGE_IMAGES <- "/media/marcus/Vulcan/radiology/iu_cxr/01_parsed_dicoms/large_jpgs"

kDIR_OUT_SMALL_IMAGES <- "/www/app_data_cxrTargetDiff/small_jpgs"; stopifnot(dir.exists(kDIR_OUT_SMALL_IMAGES))
kDIR_OUT_LARGE_IMAGES <- "/www/app_data_cxrTargetDiff/large_jpgs"; stopifnot(dir.exists(kDIR_OUT_LARGE_IMAGES))

data("codec_imgId")
codec_imgId_nmdX <- codec_imgId %>% select(original_id, img_id) %>% tibble::deframe()
data("test_df")

# Move large test images
# Switch from original file stem to my universal file stem img id
large_img_fps <- list.files(kDIR_IN_LARGE_IMAGES, pattern = "*.jpg")
available_large_img_fps <- large_img_fps %>%
    tibble::enframe(value = "original_id") %>%
    mutate(original_id = str_sub(original_id, end = -5)) %>%
    inner_join(codec_imgId, by="original_id") %>%
    semi_join(test_df, by="img_id")

large_img_fps_in <- file.path(kDIR_IN_LARGE_IMAGES, str_c(available_large_img_fps$original_id, ".jpg"))
stopifnot(all(map_lgl(large_img_fps_in, file.exists)))

large_img_fps_out <- file.path(kDIR_OUT_LARGE_IMAGES, str_c(available_large_img_fps$img_id, ".jpg"))
stopifnot(all(map_lgl(large_img_fps_out, compose(dir.exists, dirname))))

# Check what images already exist in the target dir, and remove them from the work queue
already_moved_img_ids <- large_img_fps_out[file.exists(large_img_fps_out)] %>% fp_stem()

is_input_done <- large_img_fps_in %>% fp_stem() %>% `[`(codec_imgId_nmdX, .) %in% already_moved_img_ids
large_img_fps_in <- large_img_fps_in[!is_input_done]
is_output_done <- large_img_fps_out %>% fp_stem() %in% already_moved_img_ids
large_img_fps_out <- large_img_fps_out[!is_output_done]

# Write shell script
map2_chr(large_img_fps_in, large_img_fps_out, ~str_c("cp", .x, .y, sep=" ")) %>%
    cat(sep = "\n", file = "inst/extdata/import_imgs.sh")


# Move small test images
small_img_fps_in <- list.files(kDIR_IN_SMALL_IMAGES, pattern="*.jpg", full.names = TRUE)
small_img_fps_in <- small_img_fps_in[fp_stem(small_img_fps_in) %in% test_df$img_id]

small_img_fps_out <- file.path(kDIR_OUT_SMALL_IMAGES, basename(small_img_fps_in))

# Check what images already exist in the target dir, and remove them from the work queue
already_moved_img_ids <- small_img_fps_out[file.exists(small_img_fps_out)] %>% fp_stem()
stopifnot(is_empty(already_moved_img_ids))  # defer writing handling until it's a problem

map2_chr(small_img_fps_in, small_img_fps_out, file.copy)



####
# Add all images from test cases to large imgs so I can provide
# radiologists with all pertinent data for Au generation

# Are all cases unique?!
test_df %>%
    sep_id() %>%
    use_series(case) %>%
    duplicated() %>%
    any()


test_cases <- test_df %>%
    sep_id() %>%
    use_series(case)
test_case_imgs <- iu_scalars_df %>%
    sep_id(remove = FALSE) %>%
    filter(case %in% test_cases) %>%
    use_series(img_id)

out_ids <- list.files(kDIR_OUT_LARGE_IMAGES) %>%
    fp_stem()
to_mv_ids <- setdiff(test_case_imgs, out_ids)

from_fps <- to_mv_ids %>%
    tibble::enframe() %>%
    left_join(codec_imgId, by=c(value="img_id")) %>%
    use_series(original_id) %>%
    str_c(".jpg") %>%
    file.path(kDIR_IN_LARGE_IMAGES, .)
stopifnot(all(map_lgl(from_fps, file.exists)))
to_fps <- file.path(kDIR_OUT_LARGE_IMAGES, str_c(to_mv_ids, ".jpg"))
any(map_lgl(to_fps, file.exists))

walk2(from_fps, to_fps, file.copy)
all(map_lgl(to_fps, file.exists))
