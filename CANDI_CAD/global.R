library("dplyr")
library("tibble")
library("magrittr")
library("stringr")
library("rebus")
library("purrr")
library("readr")
library("tidyr")
library("ggplot2")
library("forcats")

library("shinythemes")
library("shinyjs")
library("shiny")

walk(list.files("R", full.names = TRUE), source)  # Load all R helpers
walk(list.files("modules", full.names=TRUE), source)  # Load all shiny modules



# Flags -----------------------
kDXS_CHR <- c("cardiomegaly", "emphysema", "effusion")  # Dx options to include in ui checkbox

# fs i/o
kTEST_IMG_IN_DIR <- file.path('www', 'test_images')  # images to be interpreted
kHIST_IMG_IN_DIR <- file.path('www', 'historical_images')  # images for similar search
kTEST_CNN_IN_PTH <- file.path('www', 'test_images.csv')  # CNN inference of test images
kHIST_REC_IN_PTH <- file.path('www', 'historical_images.csv')  # scalars + CNN inference for similar


# FS I/O Interface -------------
# find available images
stopifnot(dir.exists(c(kTEST_IMG_IN_DIR, kHIST_IMG_IN_DIR)))
test_img_fns <- list.files(kTEST_IMG_IN_DIR, pattern = ".jpg")  # used for image select dropdown menu
hist_img_fns <- list.files(kHIST_IMG_IN_DIR, pattern= ".jpg")


# load data ----
# CNN provides probability of each diagnosis, as well as PCs used to compute image similarity scores
test_imgs_df <- suppressMessages(read_csv(kTEST_CNN_IN_PTH)) %>%
    filter(img_id %in% stem(test_img_fns))
hist_imgs_df <- suppressMessages(read_csv(kHIST_REC_IN_PTH)) %>%
    filter(img_id %in% stem(hist_img_fns),
           img_id %ni% stem(test_img_fns)) %>%
    mutate_at(.vars = vars(sex, view, cassette_orientation), .f=as.factor)

# non-rxtive processing ----
# cnn inferences
test_py_df <-test_imgs_df %>%
    select(img_id, starts_with("pY_")) %>%
    set_colnames(., value = colnames(.) %>% str_replace("pY_", ""))
test_pc_df <- test_imgs_df %>%
    select(img_id, starts_with("PC"))
rm(test_imgs_df)
