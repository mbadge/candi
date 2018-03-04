# Module UI Function


#' Shiny Module User Impression Input Interface
#'
#' @param id chr(1) namespace for the module, decided by the caller at the time the module is used
#' @param dx_chr chr(n) diagnosis to include in the classification input
#' @return shiny module ui component
#'
#' @family shiny_module
#' @export
impressionUi <- function(id, dx_chr) {
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



#' Title
#'
#' @param input
#' @param output
#' @param session
#'
#' @return
#' @export
#'
#' @examples
impression <- function(input, output, session) {
    usrImpressionDf <- reactive({
        data.frame(
            pathologies = toString(input$dxChkbxIn),
            clinical_note = input$noteTxtIn)
    })
    return(usrImpressionDf)
}
