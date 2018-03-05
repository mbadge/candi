fluidPage(
    shinyjs::useShinyjs(),
    theme = shinythemes::shinytheme("darkly"),
    div(id="title", align="center",
        titlePanel("CANDI for Computer Assisted Diagnosis", windowTitle="CANDI CAD")),

    # User name / submission button
    fluidRow(
        column(3,
               textInput("user_name", "Radiologist Name:", value = "Marcus"),
               hr(),
               actionButton("submitBtn", "Begin Trial"),  # After first click, the label is updated to "Submit Impression"
               hr(),
               textOutput("readerModeTxt")
        ),
        # Test Radiograph Row (Display test radiograph + user impression input form)
        column(6,
            div(id="mainImageUi", align="center",
                imageOutput("mainImage", width = 299, height=299))
        ),
        column(3,
             shinyjs::hidden(div(id="impressionPanel",
                 impressionUi("impression", dx_chr=kDXS_CHR)))  # Shiny module
        )
    ),

    hr(),
    # ConvNet Assistance
    shinyjs::hidden(div(id = "cnnCadUi",
        h3('CNN Prediction'),
        fluidRow(
            column(8,
                div(id="bboxImageUi", align="center",
                    imageOutput("bboxImage", width=299, height=299))
            ),
            column(4, tableOutput("cnnPyTbl"))
        ),
        similarImgUi("similarImg")
    ))
    #hr(),

    #traceOutput("trace")
)
