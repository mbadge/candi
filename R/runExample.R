#' @export
runCAD <- function() {
    appDir <- system.file("examples", "CANDI_CAD", package="candi")
    if (appDir == "") {
        stop("Could not find example directory. Try re-installing `candi`.", call.=FALSE)
    }


    shiny::runApp(appDir, display.mode = "normal")
}
