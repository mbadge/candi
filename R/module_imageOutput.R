#' Radiograph display modules for shiny apps
#'
#' Wraps \code{\link[EBImage]{displayOutput}} in UI fxn
#' and \code{\link{display_radiograph}} in server fxn
#'
#' @param id chr(1) namespace for the module, decided by the caller at the time the module is used
#'
#' @return shiny module that outputs the requested image
#' @export
#' @family shiny_module
#' @aliases radiograph
#'
#' @examples
#' radiographOutput(id = "img_out")
#' if (interactive()) {
#' library(shiny)
#'
#' shinyApp(
#' ui = fluidPage(
#'     selectInput("imgIdIn", "Image Id:", choices = imgs_avail),
#'     verbatimTextOutput("selectedImgTxt"),
#'     radiographOutput("show_rad")
#' ),
#' server = function(input, output, session) {
#'     output$selectedImgTxt <- renderPrint(input$imgIdIn)
#'     callModule(radiograph, "show_rad", imgIdIn = reactive(input$imgIdIn))
#' })}
radiographOutput <- function(id) {
    ns <- NS(id)

    div(id = ns("imgOut"), align = "center",
        EBImage::displayOutput(ns("mainImage")))
}


#' @param imgIdIn reactive evaluating to a chr(1) with the file stem of the desired image
#' @export
#' @rdname radiographOutput
radiograph <- function(input, output, session, imgIdIn) {
    output$mainImage <- EBImage::renderDisplay({
        candi::display_radiograph(imgIdIn())
    })
}
