
#' Tabulate Performance Scores for package models
#'
#' @return a \code{\link[knitr]{kable}} table with various metrics for each model
#' @export
#' @importFrom knitr "kable"
#'
#' @examples
#' Tab.Perf()
Tab.Perf <- function() {
    data(perf_tbl, package="candi")
    perf_tbl %>%
        knitr::kable(digits=2,
                     caption = "Classification Performance for 9 Cardiothoracic Imaging Indications.")
}

