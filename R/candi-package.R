#' candii
#'
#' Demonstrate shiny candi radiograph dashboard implementations.
#'
#' @name candi
#' @docType package
"_PACKAGE"



.PROJECT_DIR <- function() {
    .PROJECT_DIR <- switch (Sys.info() %>% `[`("sysname"),
                            "Windows" = "P:/candi",
                            "Linux"   = "/media/marcus/Projects/candi")
    # Windows Bash subsystem edge case
    if (Sys.info()["sysname"] == "Linux" && stringr::str_detect(Sys.info()["release"], "Microsoft")) {.PROJECT_DIR <- "/mnt/p/candi"}
    return(.PROJECT_DIR)
}
