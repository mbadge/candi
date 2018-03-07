# This file contains utility functions to help manage the queue of test
# radiographs in cxrTargetDiff shiny apps


#' Convert a username into a coded integer ID using sha256 digests trimmed to 4 bytes
#'
#' @param username chr(1) user name with correct case
#'
#' @return a 4 or 5 digit integer
#'
#' @examples
#' digest_user_to_id("Marcus")
#' digest_user_to_id("Nick")
#' map_int(letters, digest_user_to_id)
digest_user_to_id <- function(username) {
    digest::digest(username, "sha256") %>%
        stringr::str_sub(end=4) %>%
        strtoi(base=16L)
}


#' Deterministically randomize a queue order for a user
#'
#' @param username chr(1)
#' @param input_ids int(1)
#'
#' @return a vector the same length as input_ids, with a deterministically shuffled order
#' @export
#'
#' @examples
#' randomize_user_queue("marcus", letters)
randomize_user_queue <- function(username, input_ids) {
    user.int <- digest_user_to_id(username)
    set.seed(user.int)
    sample(x = input_ids, size = length(input_ids), replace = FALSE)
}
