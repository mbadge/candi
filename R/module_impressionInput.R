# Module UI Function


#' Impression User Interface Module
#'
#' Create a collection of ui inputs for a user to indicate binary classification of
#' each dx in \code{dx_chr} along with a free text note.
#'
#' @param id chr(1) namespace for the module, decided by the caller at the time the module is used
#' @param dx_chr chr(n) diagnoses to include in the classification input
#'
#' @return \code{\link[shiny]{tagList}} of impression ui components
#'
#' @family shiny_module
#' @seealso \code{\link{impression}}
#' @export
#' @examples
#' impressionInput(id="imp", dx="Cardiomegaly")
#' if (interactive()) {
#' library(shiny)
#'
#' shinyApp(
#'     ui = fluidPage(
#'         impressionInput("impression", dx_chr = candiOpt(dxs_chr)),
#'         shiny::tags$strong("User Impression:"),
#'         tableOutput("usrImpressionTable")
#'     ),
#'     server = function(input, output) {
#'         usrImpressionDf <- callModule(impression, "impression")
#'         output$usrImpressionTable <- renderTable(usrImpressionDf())
#'     }
#' )}
impressionInput <- function(id, dx_chr) {
    ns <- NS(id)

    tagList(
        h3("Your Impression"),
        p(strong("Is there radiographic evidence for a diagnosis?")),
        # Diagnosis_lglndx ----------
        checkboxGroupInput(ns("dxChkbxIn"), label=NULL, choices=dx_chr),
        hr(),
        # Impression_chr1 ---------
        p(strong("Clinical Note:")),
        textAreaInput(ns("noteTxtIn"), NULL, "Clinical Impression", resize = "both")
    )
}


#' CANDI User Impression Interface Module Server Function
#'
#' @param input,output,session shiny module server-client mgmt
#'
#' @return a function, wrapped in S3 class reactive.  see \code{\link[shiny]{reactive}}
#'
#' @family shiny_module
#' @seealso \code{\link{impressionInput}}
#' @export
impression <- function(input, output, session) {
    usrImpressionDf <- shiny::reactive({
        data.frame(
            pathologies = toString(input$dxChkbxIn),
            clinical_note = input$noteTxtIn)
    })
    return(usrImpressionDf)
}
