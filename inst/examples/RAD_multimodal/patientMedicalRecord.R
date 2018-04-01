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



patientMedicalRecord <- function(input, output, session,
                                 imgIdIn,
                                 test_df,
                                 demographics_cols = character(),
                                 dx_cols = character(),
                                 note_col = character()) {
    # Preconditions
    stopifnot(is.data.frame(test_df))

    # Parse test_df
    pt_df        <- test_df[c("img_id", demographics_cols)]
    hist_dx_df   <- test_df[c("img_id", dx_cols)]
    hist_note_df <- test_df[c("img_id", note_col)]

    output$demographicsTbl <- renderTable({
        req(imgIdIn())
        pt_df %>%
            df_filter_trans(imgIdIn())
    }, colnames = FALSE)

    output$dxTbl <- renderTable({
        req(imgIdIn())
        out <- hist_dx_df %>%
            df_filter_trans(imgIdIn())
        out$column %<>% MyUtils::str_case_title()
        out
    }, colnames = FALSE)

    output$noteTxt <- renderText({
        req(imgIdIn())
        hist_note_df %>%
            dplyr::filter(img_id == imgIdIn()) %>%
            magrittr::use_series(findings)
    })
}
