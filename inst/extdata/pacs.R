# Ingest data for candi learning trial (see gitlab mabadgeley/candi#9)

# Partition train/test images by time
# pull all images from the test time period
kLARGE_IMG_DIR = "/media/marcus/Vulcan/radiology/iu_cxr/01_parsed_dicoms/large_jpgs"
candiOpt(large_img_dir)

data(lut_img_id2original, package="ProjUtilsRads")
data(radiographs, package="candi")

# Check overlap
list.files(candiOpt(large_img_dir)) %>% len
id_codec <- lut_img_id2original$img_id %>% set_names(., lut_img_id2original$original_id)


#! 19 images without an entry in radiographs
list.files(candiOpt(large_img_dir)) %>%
    fp_stem() %>%
    `[`(id_codec, .) %>%
    setdiff(radiographs$img_id)

# Images available for all radiograph table entries
list.files(candiOpt(large_img_dir)) %>%
    fp_stem() %>%
    `[`(id_codec, .) %>%
    setdiff(radiographs$img_id, .)


# Rename images by the shorthand codec
orig_fps <- list.files(candiOpt(large_img_dir), full.names = TRUE)
new_fps <- fp_stem(orig_fps) %>% `[`(id_codec, .) %>% file.path(dirname(orig_fps), .)
map2(orig_fps, new_fps, file.rename)
map2(new_fps, str_c(new_fps, ".jpg"), file.rename)
