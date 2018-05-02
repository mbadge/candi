#' EMR display module for shiny apps
#'
#' @param id chr(1) namespace for the module, decided by the caller at the time the module is used
#' @param section_label chr(1) text to print at the top of the ui section
#'
#' @return \code{\link[shiny]{tagList}} of emr ui components
#' @export
#' @examples
#' if (interactive()) {
#' shinyApp(
#'     ui = fluidPage(
#'         selectInput("idIn", "Case ID", choices = cases$case),
#'         patientMedicalRecordOutput("emr")
#'     ),
#'     server = function(input, output, session) {
#'         callModule(patientMedicalRecord, "emr", idIn = reactive(input$idIn))
#'     }
#' )}
patientMedicalRecordOutput <- function(id, section_label="Patient Medical Record") {
    ns <- NS(id)

    tagList(
        div(align="center", p(strong(section_label))),
        tableOutput(ns("demographicsTbl")),
        hr(),
        div(align="center", p(strong("Historical Impression"))),
        tableOutput(ns("dxTbl")),
        textOutput(ns("noteTxt"))
    )
}



#' @rdname patientMedicalRecordOutput
patientMedicalRecord <- function(input, output, session,
                                 idIn) {
    output$demographicsTbl <- renderTable({
        candi::cases[candi::cases$case == imgIds2Cases(idIn()), ] %>%
            dplyr::select(one_of(c("age", "sex", "day"))) %>%
            AnalysisToolkit::t2idf()
    }, colnames = FALSE)

    output$dxTbl <- renderTable({
        candi::cases[candi::cases$case == imgIds2Cases(idIn()), ] %>%
            dplyr::select(one_of(candiOpt(dxs_chr))) %>%
            AnalysisToolkit::t2idf() %>%
            mutate(column = map_chr(column, str_case_title))
    }, colnames = FALSE)

    output$noteTxt <- renderText({
        candi::cases[candi::cases$case == imgIds2Cases(idIn()), ] %>%
            use_series(indication)
    })
}


