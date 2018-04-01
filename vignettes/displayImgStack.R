# Example demo app to stack all imgs from a case into an EBImage::renderDisplay\
data(test_df, package="cxrTargetDiff")

kDIR_LARGE_IMGS <- "/www/app_data_cxrTargetDiff/large_jpgs/"; stopifnot(dir.exists(kDIR_LARGE_IMGS))
# kDIR_LARGE_IMGS <- "P:/radiology/data/images/"; stopifnot(dir.exists(kDIR_LARGE_IMGS))


case_img_ids <- c("iu_910_1", "iu_910_2")
img <- load_radiograph(case_img_ids[[1]], kDIR_LARGE_IMGS)

# EBImage fxns
library(EBImage)
display(img)
resize(img, w = 500, h = 200) %>% display



case_imgs <- map(case_img_ids, load_radiograph, img_dir = kDIR_LARGE_IMGS)

# Ugh fml the images have to be the same exact size to combine
map(case_imgs, resize, w = 2320, h=2320) %>%
    combine() %>%
    display()

# Match image sizes, stack, and display
min_dims <- case_imgs %>%
    map(dim) %>%
    lift_dl(rbind)(.) %>%
    apply(MARGIN = 2, min)
map(case_imgs, resize, w=min_dims[1], h=min_dims[2]) %>%
    combine() %>%
    display()


display_case <- function(case_id, img_dir = kDIR_LARGE_IMGS) {
    case_img_ids <- list.files(kDIR_LARGE_IMGS) %>%
        fp_stem() %>%
        str_subset(str_c("iu_", case_id, "_"))
    case_imgs <- map(case_img_ids, load_radiograph, img_dir = kDIR_LARGE_IMGS)

    # Match image sizes, stack, and display
    min_dims <- vapply(case_imgs, dim, FUN.VALUE = integer(2)) %>%
        apply(MARGIN = 2, min)
    map(case_imgs, resize, w=min_dims[1], h=min_dims[2]) %>%
        combine() %>%
        display()
}


test_df %>% sep_id() %>% use_series(case)
display_case("910")
display_case(28)
