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
#' if (interactive()) {
#' library(shiny)
#'
#' imgs_avail = candiOpt(large_img_dir) %>% list.files() %>% fp_stem()
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


#' @param input,output,session shiny server io
#' @param imgIdIn reactive evaluating to a chr(1) with the file stem of the desired image
#' @export
#' @rdname radiographOutput
radiograph <- function(input, output, session, imgIdIn) {
    output$mainImage <- EBImage::renderDisplay({
        candi::display_radiograph(imgIdIn())
    })
}



#' Interactive display of radiographs associated with a requested case
#'
#' Wraps \code{\link{display_case}}
#'
#' @param id chr(1) namespace for the module, decided by the caller at the time the module is used
#'
#' @return shiny module with interactive browsing of case imgs
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'
#' cases_avail = candiOpt(large_img_dir) %>% list.files() %>% fp_stem() %>%
#'     str_split(pattern = "_") %>%
#'     map_chr(2) %>% unique() %>% as.integer() %>% sort()
#'
#' shinyApp(
#'     ui = fluidPage(
#'         selectInput("caseIdIn", "Case Id:", choices = cases_avail),
#'         verbatimTextOutput("selectedCaseTxt"),
#'         caseOutput("show_case"),
#'         caseOutput("show_case2", height = "1000px")
#'     ),
#'     server = function(input, output, session) {
#'         output$selectedCaseTxt <- renderPrint(input$caseIdIn)
#'         callModule(case, "show_case", caseIdIn = reactive(input$caseIdIn))
#'         callModule(case, "show_case2", caseIdIn = reactive(input$caseIdIn))
#'     }
#' )}
caseOutput <- function(id, ...) {
    ns <- NS(id)

    div(id = ns("caseOut"), align = "center",
                EBImage::displayOutput(ns("mainImage"), ...))
}


#' @param input,output,session shiny server io
#' @param caseIdIn reactive expression that returns chr(1) case id to display
#' @param ... passed to \code{\link[EBImage]{renderDisplay}}
#' @export
#' @rdname caseOutput
case <- function(input, output, session, caseIdIn)
{
    output$mainImage <- EBImage::renderDisplay({
        candi::display_case(case_id = caseIdIn())
    })
}
