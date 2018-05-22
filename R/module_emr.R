#' Scalar data display modules for shiny apps
#'
#' @param id chr(1) namespace for the module, decided by the caller at the time the module is used
#' @param input,output,session standard args to shiny server routine
#' @param idIn reactive that returns a chr(1) case or image identifier to fetch data for
#'
#' @return \code{\link[shiny]{tagList}} of emr ui components
#'
#' @name emrModule
#'
#' @import shiny
#' @export
#'
#' @examples
#' if (interactive()) {
#' library(shiny)
#'
#' shinyApp(
#'     ui = fluidPage(
#'         selectInput("idIn", "Case ID:", choices = candi::radiographs$img_id),
#'         hpiOutput("hpi")
#'     ),
#'     server = function(input, output, session) {
#'         callModule(hpiModule, "hpi", idIn = reactive(input$idIn))
#'     }
#' )
#'
#' shinyApp(
#'     ui = fluidPage(
#'         selectInput("idIn", "Case ID:", choices = radiographs$img_id),
#'         histImpOutput("histImp")
#'     ),
#'     server = function(input, output, session) {
#'         callModule(histImpModule, "histImp", idIn = reactive(input$idIn))
#'     }
#' )
#' }
hpiOutput <- function(id) {
    ns <- NS(id)
    textOutput(ns("hpiTxt"))
}

#' @export
#' @rdname emrModule
hpiModule <- function(input, output, session, idIn) {
    caseId <- reactive( {
        #req(idIn)
        id_2Case(idIn)
    })

    output$hpiTxt <- renderText({
        narrative_str <- candi::cases[candi::cases$case == caseId(), "hpi"] %>% as.character()
        narrative_str
    })
}


#' @export
#' @rdname emrModule
histImpOutput <- function(id) {
    ns <- NS(id)

    tagList(
        div(align="center",
            p(strong( "Curators Interpretation of Findings:" )),
            tableOutput(ns("dxTbl")),
            hr(),
            p(strong( "Documented Case Findings:" )),
            textOutput(ns("findingsTxt"))
        )
    )
}

#' @export
#' @rdname emrModule
histImpModule <- function(input, output, session, idIn) {
    caseId <- reactive({
        #req(idIn)
        id_2Case(idIn)
    })

    output$findingsTxt <- renderText({
        narrative_str <- candi::cases[candi::cases$case == caseId(), "findings"] %>% as.character()
        narrative_str
    })

    output$dxTbl <- renderTable({
        candi::cases[candi::cases$case == caseId(), ] %>%
            dplyr::select(dplyr::one_of( candiOpt(dxs_chr) )) %>%
            t2idf() %>%
            dplyr::mutate(column = map_chr(column, str_case_title))
    }, hover = TRUE, spacing = "xs", colnames = FALSE)
}
