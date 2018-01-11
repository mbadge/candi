# Module UI Function
impressionUi <- function(id, dx_chr) {
    ns <- NS(id)

    tagList(
        h3("Your Impression"),
        p(strong("Is there radiographic evidence for a diagnosis?")),
        checkboxGroupInput(ns("dxChkbxIn"), label=NULL, choices=dx_chr),
        hr(),
        p(strong("Clinical Note:")),
        textAreaInput(ns("noteTxtIn"), NULL, "Clinical Impression", resize = "both"),
        actionButton(ns("submit_impression"), "Submit Impression")
    )
}


# Module Server Function
impression <- function(input, output, session) {
    usrInptDf <- reactive({
        data.frame(
            pathologies = toString(input$dxChkbxIn),
            clinical_note = input$noteTxtIn)
    })

    return(usrInptDf)
}
