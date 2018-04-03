# Ingest data for candi learning trial (see gitlab mabadgeley/candi#9)

# Partition train/test images by time
# pull all images from the test time period
kLARGE_IMG_DIR = "/media/marcus/Vulcan/radiology/iu_cxr/01_parsed_dicoms/large_jpgs"


data(lut_img_id2original, package="ProjUtilsRads")
data(radiographs, package="candi")

# Check overlap
list.files(kLARGE_IMG_DIR) %>% len
id_codec <- lut_img_id2original$img_id %>% set_names(., lut_img_id2original$original_id)


#! 19 images without an entry in radiographs
list.files(kLARGE_IMG_DIR) %>%
    fp_stem() %>%
    `[`(id_codec, .) %>%
    setdiff(radiographs$img_id)

# Images available for all radiograph table entries
list.files(kLARGE_IMG_DIR) %>%
    fp_stem() %>%
    `[`(id_codec, .) %>%
    setdiff(radiographs$img_id, .)


