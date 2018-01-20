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
        textAreaInput(ns("noteTxtIn"), NULL, "Clinical Impression", resize = "both")
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
