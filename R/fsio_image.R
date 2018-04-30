#' Load and Display radiographs via EBImage Images
#'
#' Wrapper for \code{\link[EBImage]{readImage}}
#'
#' @param img_id chr(1)
#' @param img_dir chr(1)
#'
#' @return \code{\link[EBImage]{Image}} with 3 axes
#'
#' @name load_radiograph
#' @importFrom EBImage "readImage"
#' @export
#' @examples
#' load_radiograph("iu_1_1")
#' load_radiograph("iu_1_1") %>% EBImage::display(.)
load_radiograph <- function(img_id, img_dir = candiOpt(large_img_dir)) {
    fp <- file.path(img_dir, paste0(img_id, ".jpg"))
    stopifnot(file.exists(fp))

    img <- EBImage::readImage(fp)
    return(img)
}


#' @rdname load_radiograph
#' @export
#' @examples
#' display_radiograph("iu_1_1")
display_radiograph <- function(img_id, img_dir = candiOpt(large_img_dir)) {
    load_radiograph(img_id = img_id, img_dir = img_dir) %>%
        EBImage::display()
}


#' Load radiographs from dataset-case into an EBImage stack
#'
#' All images named <dataset>_<case_id>_* in img_dir are loaded.
#'
#' @param dataset chr(1) default "iu"
#' @param case_id chr(1) or int(1)
#' @param img_dir chr(1) default candiOpt(large_img_dir)
#'
#' @return \code{\link[EBImage]{Image}} with 4 axes
#' @export
#' @import purrr
#'
#' @name case
#' @examples
#' load_case("1") %>% EBImage::display(.)
#' load_case(1) %>% EBImage::display(.)
load_case <- function(case_id, dataset="iu", img_dir=candiOpt(large_img_dir)) {
    if (compose(`!`, dir.exists)(img_dir)) {
        stop(glue::glue("image directory not found: \n{img_dir}"))
    }

    case_img_ids <- list.files(img_dir,
                               pattern = str_c("^", dataset, "_", case_id, "_")) %>%
        MyUtils::fp_stem(.)

    case_imgs <- purrr::map(case_img_ids, load_radiograph, img_dir = img_dir)

    # Match image sizes and stack
    min_dims <- case_imgs %>%
        purrr::map(dim) %>%
        purrr::lift_dl(rbind)(.) %>%
        apply(MARGIN=2, min)
    purrr::map(case_imgs, EBImage::resize, w=min_dims[1], h=min_dims[2]) %>%
        EBImage::combine()
}

#' @rdname case
#' @export
#' @examples
#' display_case("3")
display_case <- function(case_id, dataset="iu", img_dir=candiOpt(large_img_dir)) {
    load_case(dataset=dataset, case_id=case_id, img_dir=img_dir) %>%
        EBImage::display()
}

