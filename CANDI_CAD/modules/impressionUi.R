# Module UI Function
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
        textAreaInput(ns("noteTxtIn"), NULL, "Clinical Impression", resize = "both"),
        actionButton(ns("submit_impression"), "Submit Impression")
    )
}


# Module Server Function
impression <- function(input, output, session, idDf, usage_data) {
    usrInptDf <- reactive({
        data.frame(
            pathologies = toString(input$dxChkbxIn),
            clinical_note = input$noteTxtIn)
    })

    observeEvent(input$submit_impression, {
        usr_input <- bind_cols(idDf(), usrInptDf()) %>%
            add_column(timestamp = date_time_stamp(), .before=1)
        log <- bind_cols(idDf(), usage_data()) %>%
            add_column(timestamp = date_time_stamp(), .before=1)
        save_usr_input(usr_input)
        save_usr_usage(log)
    })

    return(usrInptDf)
}
