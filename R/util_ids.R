#' Validation functions for flexibly handling different id scoping
#'
#' @param x_chr character vector to test for id format
#'
#' @examples
#' id_isImg("iu_1_1")
#' id_isImg("iu")
#' id_isImg(1)
#' id_isImg(c("iu_1_1", "iu_4_4"))
id_isImg <- function(x_chr) {
    str_detect(x_chr, pattern = "^[[:alpha:]]+_[0-9]+_[0-9]+$")
}

#' @rdname id_isImg
#' @examples
#' id_isCase("iu_1_1")
#' id_isCase("iu")
#' id_isCase(1)
#' id_isCase("1")
#' id_isCase(c("1", "5", "5000"))
id_isCase <- function(x_chr) {
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
#' id_2Case(imgs_avail)
#' id_2Case(imgs_avail, unique=FALSE)
#' id_2Case("iu_1_1")
#' id_2Case(1)
#' id_2Case("foobar")
id_2Case <- function(img_ids, unique = TRUE) {
    if (compose(`!`, all, id_isImg)(img_ids)) {
        if (compose(all, id_isCase)(img_ids)) {
            message('input is already case-identifier')
            return(img_ids)
        } else {
            stop("Input is not formatted as case or image identifier: ", img_ids[1])
        }
    }

    case_ids <- img_ids %>%
        str_split(pattern = "_") %>%
        map_chr(2) %>% as.integer()

    if (unique) case_ids <- unique(case_ids)

    case_ids
}



kIMG_ID_X_VARS <- c("dataset", "case", "image")
kIMG_ID_S_CMPD <- c("img_id")

#' Image Identifier Helper Transformations
#'
#' Image Ids can be represented in 2 ways:
#' \enumerate{
#'     \item chr(1) unique compound key
#'     \item chr(n) vector of individual id component variables
#' }
#'
#' The default names of id vars are configured with the global `kID_CHRS`,
#' which is defined in this img id helpers script
#'
#' Mutating Fxn to separate `img_id` column of a data.frame into separate component identifiers.
#'
#' Assumes the project schema of img_id=<dataset>_<case>_<image>
#' Attempts to attach original attributes onto the product (except names and spec)
#'
#' dataset, case, image cols.
#'
#' Unite id components into a single unique image identifier.
#'
#' Convenience wrapper to directly operate on a joint id as if it were split
#'
#' @param df data.frame with column 'img_id' or columns `img_vars`
#' @param img_id_cmpd s_chr compound key identifier representation
#' @param img_id_vars x_chr names of individual tidy id variables
#' @param ... args passed to \code{\link[tidyr]{separate}} or \code{\link[tidyr]{unite}}
#' @param f function to call on df after separating ids
#'
#' @return data.frame with transformed id column representation
#'
#' @examples
#' data(lut_img_id2original)
#' sep_id(lut_img_id2original)
#' sep_id(lut_img_id2original, remove=FALSE)
#' sep_id(bind_rows(lut_img_id2original, lut_img_id2original))  # Expect warning
#' @name img_id
NULL


#' @export
#' @rdname img_id
sep_id <- function(df,
                   img_id_cmpd = kIMG_ID_S_CMPD,
                   img_id_vars = kIMG_ID_X_VARS,
                   sep = "_", remove = TRUE, convert = TRUE, ...
)
{
    # Preconditions
    stopifnot(is.data.frame(df))
    stopifnot(img_id_cmpd %in% names(df))

    # i.f.f img_ids are unique, save rowname attr as the scalar compound key
    if (!any(duplicated(df[[img_id_cmpd]]))) {
        df <- df %>% tibble::remove_rownames()
        suppressWarnings(rownames(df) <- df[[img_id_cmpd]])
    } else {
        warning("Duplicate img_ids exist in df")
    }

    # save the original df attrs to add back to new return data.frame
    attrs <- df %>% attributes
    my_attr_nms <- setdiff(names(attrs), c("names", "spec"))  # don't include default attrs
    attrs <- attrs[my_attr_nms]

    # ENGINE
    out <- df %>%
        tidyr::separate(!!img_id_cmpd, img_id_vars,
                        sep=sep, remove=remove, convert=convert, ...)

    attributes(out) <- modifyList(attributes(out), attrs)
    out
}

#' @export
#' @rdname img_id
unite_id <- function(df,
                     img_id_cmpd = kIMG_ID_S_CMPD,
                     img_id_vars = kIMG_ID_X_VARS,
                     sep="_", remove=TRUE) {
    # Preconditions
    stopifnot(is.data.frame(df))
    stopifnot(all(img_id_vars %in% names(df)))

    # Save attrs
    attrs <- df %>% attributes
    my_attr_nms <- setdiff(names(attrs), c("names", "spec"))  # don't include default attrs
    attrs <- attrs[my_attr_nms]

    # ENGINE
    out <- df %>%
        tidyr::unite(!!img_id_cmpd, dplyr::one_of(img_id_vars),
                     sep=sep, remove=remove)

    attributes(out) <- modifyList(attributes(out), attrs)
    out
}

#' @export
#' @rdname img_id
with_sep <- function(df, f, ...) {
    df %>%
        sep_id() %>%
        f(., ...) %>%
        unite_id()
}
