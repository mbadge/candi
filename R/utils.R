#' Convert a matrix with rownames into a column indexed data.frame
#'
#' @param mtx matrix with rownames
#'
#' @return data.frame with column "id" containing mtx's rownames
#' @export
#'
#' @examples
#' as.matrix(mtcars) %>% mtx2idf()
mtx2idf <- function(mtx, id_col = "id") {
    stopifnot(is.matrix(mtx))
    mtx %>%
        as.data.frame() %>%
        tibble::rownames_to_column(id_col)
}


#' Transpose a df to another indexed data.frame
#'
#' The base \code{\link[base]{t}} produces a matrix.
#' This wrapper calls \code{\link[base]{t}} followed by
#' \code{\link{mtx2idf}} to produce a data.frame instead
#'
#' @param df data.frame
#' @param name chr(1) column name for original data.frame colnames
#' @param value chr(1) colname for original row 1 values
#'
#' @return data.frame
#' @export
#'
#' @examples
#' summarise_if(mtcars, .p=is.numeric, .f=mean) %>% t2idf()
#' summarise_if(mtcars, .p=is.numeric, .f=mean) %>% t2idf("variable", "mean")
#' summarise_if(group_by(mtcars, cyl), .p=is.numeric, .f=mean) %>% t2idf("variable", "mean")
t2idf <- function(df, name="column", value="value") {
    if (nrow(df) > 1) {GrpColNms <- c(names(df)[[1]], df[[1]]); df[[1]] <- NULL}

    out <- df %>%
        t() %>%
        mtx2idf()

    # for transposing a summary table, use name and value colnames
    if (ncol(out) == 2) {names(out) <- c(name, value)}
    # for transposing a grouped summary table, use the first col as grouping colnames
    if (ncol(out) > 2) {names(out) <- c(GrpColNms)}

    out
}




#' Rasterize an EBImage Image object figure.
#'
#' @param img_arr an EMIage `Image` object
#'
#' @return side-effect: draws figure
#'
#' @importFrom dplyr "%>%"
#' @export
Viz.Image <- function(img_arr) {
    stopifnot(inherits(img_arr, "Image"))
    img_arr %>%
        EBImage::normalize() %>%
        EBImage::display(method="raster")
}



#' @export
date_time_stamp <- function(){format(Sys.time(), "%Y%m%d_%H%M%S")}
#' @export
`%ni%` <- Negate(`%in%`)

# Boolean predicate fxn operators ----
Or <- function(f1, f2){
    force(f1); force(f2)
    function(...){
        f1(...) || f2(...)
    }
}
Not <- function(f1){
    force(f1)
    function(...){!f1(...)}
}



# String manipulation

#' Split a string vector into a list of string piece vectors.
#'
#' First, a replacement is performed at alphanumeric piece edges to create
#' pieces that are delimited by a space or punctuation character.
#' Second, the string is split by split_pat into pieces
#'
#' @param x_chr character vector of strings to be split
#' @param split_pat regex pattern to delimit pieces
#'
#' @return pieces list with same length as x_chr, whose i-th element contains a
#' character vector of split products of x_chr[i]
#'
#' @import purrr stringr
#' @importFrom rebus "%R%" capture one_or_more
#' @export
split_pieces <- function(x_chr, split_pat = character()) {
    if (is_empty(split_pat)) {
        split_pat <- rebus::one_or_more(rebus::or("[[:space:]]", "[[:punct:]]"))
    }
    nms <- names(x_chr) %||% x_chr

    pieces <- x_chr %>%
        str_replace_all(pattern=capture(rebus::LOWER) %R% capture(rebus::UPPER),
                        replacement = "\\1 \\2") %>%
        str_replace_all(pattern = capture(rebus::ALPHA) %R% capture(rebus::DIGIT),
                        replacement = "\\1 \\2") %>%
        str_replace_all(pattern = capture(rebus::DIGIT) %R% capture(rebus::ALPHA),
                        replacement = "\\1 \\2") %>%
        str_split(pattern=split_pat)

    # Remove empty strings pieces
    pieces <- lapply(pieces, FUN = discard, .p=assertive.strings::is_empty_character)

    names(pieces) <- nms
    pieces
}


# Case Transformations ----

#' Convert string to title case.
#'
#' Transform a character vector into a title case representation.
#' First, separate camelCase and alphanumeric boundaries into spaces
#' Second, split pieces by symbols or spaces
#' Third, use \code{\link[stringr]{str_to_title}} to glue pieces into title.
#'
#' @param x_chr character vector
#'
#' @return x_title character vector with same length as x_chr, but title format
#'
#' @import stringr purrr
#' @export
#'
#' @family str_case
#'
#' @examples
#' str_case_title(rownames(mtcars))
str_case_title <- function(x_chr) {
    pieces <- split_pieces(x_chr)
    nms <- names(pieces)

    x_title <- pieces %>%
        map_chr(str_c, collapse=" ") %>%
        stringr::str_to_title()

    names(x_title) <- nms
    x_title
}


#' Convert string to snake_case.
#'
#' Transform a character vector into a snake case representation.
#' First, separate camelCase and alphanumeric boundaries into spaces
#' Second, split pieces by symbols or spaces
#' Third, convert all alpha to lower with \code{\link[stringr]{str_to_lower}}
#' Finally, collapse with "_"
#'
#' @param x_chr character vector
#'
#' @return x_snake character vector with same length as x_chr, but snake format
#'
#' @importFrom stringr "str_to_lower"
#' @importFrom stringr "str_c"
#' @export
#'
#' @family str_case
str_case_snake <- function(x_chr) {
    pieces <- split_pieces(x_chr)

    pieces %>%
        map(stringr::str_to_lower) %>%
        map_chr(str_c, collapse="_")
}



#' @importFrom rebus one_or_more "%R%"
#' @export
fp_stem <- function(x_chr) {
    stopifnot(is.character(x_chr))

    bn <- basename(x_chr)
    stem <- str_replace(bn,
                        pattern= rebus::DOT %R%
                            one_or_more(rebus::ALNUM) %R%
                            rebus::END,
                        replacement="")
    stem
}
