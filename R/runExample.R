#' Run candi cad example application.
#'
#' Launch a Shiny app to demo the CAD clinical trial app.
#'
#' @examples
#' if (interactive()) {
#'    runCAD()
#' }
#' @export
runCAD <- function() {
    appDir <- system.file("init", "examples", "CANDI_CAD", package="candi")
    if (appDir == "") {
        stop("Could not find example directory. Try re-installing `candi`.", call.=FALSE)
    }


    shiny::runApp(appDir, display.mode = "normal")
}

#' @export
runExample <- function() {
    validExamples <- list.files(system.file("examples", package = "candi"))

    validExamplesMsg <-
        paste0(
            "Valid examples are: '",
            paste(validExamples, collapse = "', '"),
            "'")

    # if an invalid example is given, throw an error
    if (missing(example) || !nzchar(example) ||
        !example %in% validExamples) {
        stop(
            'Please run `runExample()` with a valid example app as an argument.\n',
            validExamplesMsg,
            call. = FALSE)
    }

    # find and launch the app
    appDir <- system.file("init", "examples", example, package="candi")
    shiny::runApp(appDir, display.mode = "normal")
}
