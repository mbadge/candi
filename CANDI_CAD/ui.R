fluidPage(
    shinyjs::useShinyjs(),
    theme = shinytheme("darkly"),
    div(id="title", align="center",
        titlePanel("CANDI for Computer Assisted Diagnosis", windowTitle="CANDI CAD")),

    # User name / submission button
    fluidRow(
        column(6, textInput("radiologist", "Radiologist Name:", value = "Marcus")),
        column(6, actionButton("submitBtn", "Submit Impression"))
    ),

    # Test Radiograph Row (Display test radiograph + user impression input form)
    fluidRow(
        column(8,
            div(id="mainImageUi", align="center",
                imageOutput("mainImage", width = 299, height=299))
        ),
        column(4,
            impressionUi("impression", dx_chr=kDXS_CHR)  # Shiny module
        )
    ),

    # #! INSERT MODE RANDOMIZER STATE BANNER
    textOutput("readerModeTxt"),

    #! ConvNet Assistance
    #! \/ Control from py to similarImgUi output as a function of randomizer state
    shinyjs::hidden(div(id = "cnnCadUi",
        h3('CNN Prediction'),
        tableOutput("cnnPyTbl")
    )),
    hr(),

    # Optional Similar Image Browser Utility
    #similarImgUi("similarImg"),  # Shiny module
    hr(),
    #! /\ Control from py to similarImgUi output as a function of randomizer state

    traceOutput("trace")
)
