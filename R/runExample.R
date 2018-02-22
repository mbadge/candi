#' Run candi cad example or candi rad applications.
#'
#' \code{runCAD()} Launch a Shiny app to demo the CAD clinical trial app.
#' \code{runExample()} Lists all available example apps and runs any of them.
#'
#' @examples
#' if (interactive()) {
#'    runCAD()
#'    runExample()
#' }
#' @name run
NULL



#' @rdname run
#' @export
runCAD <- function() {
    appDir <- system.file("examples", "CANDI_CAD", package="candi")
    if (appDir == "") {
        stop("Could not find example directory. Try re-installing `candi`.", call.=FALSE)
    }


    shiny::runApp(appDir, display.mode = "normal")
}


#' @rdname run
#' @export
runExample <- function(example) {
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
    appDir <- system.file("examples", example, package="candi")
    shiny::runApp(appDir, display.mode = "normal")
}
