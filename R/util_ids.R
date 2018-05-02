#' Validation functions for flexibly handling different id scoping
#'
#' @param x_chr character vector to test for id format
#'
#' @examples
#' isImgId("iu_1_1")
#' isImgId("iu")
#' isImgId(1)
#' isImgId(c("iu_1_1", "iu_4_4"))
isImgId <- function(x_chr) {
    str_detect(x_chr, pattern = "^[[:alpha:]]+_[0-9]+_[0-9]+$")
}

#' @rdname isImgId
#' @examples
#' isCaseId("iu_1_1")
#' isCaseId("iu")
#' isCaseId(1)
#' isCaseId("1")
#' isCaseId(c("1", "5", "5000"))
isCaseId <- function(x_chr) {
    str_detect(x_chr, pattern = "^[0-9]+$")
}


#' Convert a vector of image IDs to corresponding unique case IDs.
#'
#' @param img_ids x_chr of image ids to convert to case ids
#' @return x_int of case identifiers
#'
#' @export
#' @examples
#' imgs_avail = candiOpt(large_img_dir) %>% list.files() %>% fp_stem()
#' imgIds2Cases(imgs_avail)
#' imgIds2Cases("iu_1_1")
#' imgIds2Cases(1)
#' imgIds2Cases("foobar")
imgIds2Cases <- function(img_ids) {
    if (compose(`!`, all, isImgId)(img_ids)) {
        if (compose(all, isCaseId)(img_ids)) {
            message('input is already case-identifier')
            return(img_ids)
        } else {
            stop("Input is not formatted as case or image identifier: ", img_ids[1])
        }
    }

    img_ids %>%
        str_split(pattern = "_") %>%
        map_chr(2) %>% unique() %>% as.integer()
}
