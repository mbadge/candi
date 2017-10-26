#' Convert a string with words separated by "."(s) and/or spaceless title case
#' into snake case
#'
#' used for indications so they can serve as valid colnames
#'
#' @param x_chr a character vector with elements containing word(s) separated by
#' periods
#'
#' @return character the same length as x_chr, with elements formatted in a
#' snake case
#' @export
#'
#' @examples
#' x_chr <- stringr::str_c(letters, lead(letters, 1), ".", lag(letters, 1))
#' str_case_periods2snake(x_chr)
str_case_periods2snake <- function(x_chr) {
    x_chr %>%
        str_case_camel2title() %>%
        stringr::str_replace_all(rebus::one_or_more(DOT), " ") %>%
        stringr::str_to_lower() %>%
        stringr::str_replace_all(" ", "_")
}


#' Convert a chr vector of snake case text into title case
#'
#' @param x_chr a character vector with snake case
#'
#' @return a character vector the same length as x_chr but with title case
#' @export
str_case_snake2title <- function(x_chr) {
    x_chr %>%
        stringr::str_replace_all(
            pattern = "_",
            replacement= " ") %>%
        stringr::str_to_title()
}


#' Add spacing to a camel case chr to make title case
#'
#' Used to make pretty titles, eg for plot labels and `message()`
#'
#' @param x_chr A character vector with spaceless title case
#'
#' @return A character vector the same len as x_chr, with spaces added
#' @export
str_case_camel2title <- function(x_chr) {
    x_chr %>%
        stringr::str_replace_all(
            pattern = rebus::capture("[a-z]") %R% rebus::capture("[A-Z]"),
            replacement = rebus::REF1 %R% " " %R% rebus::REF2)
}

#' Convert snake case to camel case by removing "_" and making initial letters cap
#'
#' @param x_chr A character vector with snake_case_formatting
#'
#' @return A character vector the same len as x_chr, with CamelCaseFormatting
#' @export
str_case_snake2camel <- function(x_chr) {
    x_chr %>%
        stringr::str_replace_all(
            pattern = "_",
            replacement= " ") %>%
        stringr::str_to_title() %>%
        stringr::str_replace_all(
            pattern = " ",
            replacement= "")
}
