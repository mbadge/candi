fluidPage(
    shinyjs::useShinyjs(),
    theme = shinythemes::shinytheme("darkly"),
    div(id="title", align="center",
        titlePanel("CANDI for Computer Assisted Diagnosis", windowTitle="CANDI CAD")),

    # 2 horizontal sections
    # top: userInfo | mainRadiograph | userImpression
    # bottom: CNN toolkit
    fluidRow(
        column(3,
            # User Info
            textInput("user_name", "Radiologist Name:", value = "Marcus"),
            uiOutput("imgIdUi"),  # shinyjs::hidden(uiOutput("imgIdUi"))
            hr(),
            # Submitting
            div(align="center",
                actionButton("submitBtn", "Begin Trial"),
                textOutput("progressTxt")
        )),
        # Test Radiograph
        column(6, shinyjs::hidden(div(id="mainImageUi", align="center",
                            radiographOutput("main_image")
        ))),
        # User Impression
        column(3,
             shinyjs::hidden(div(id="impressionPanel",
                 impressionInput("impression", dx_chr=kDXS_CHR)))  # Shiny module
        )
    ),

    hr(),
    textOutput("readerModeTxt"),
    hr(),

    # ConvNet Assistance
    shinyjs::hidden(div(id = "cnnCadUi",
        fluidRow(
            column(3,
                h3('CNN Prediction')
            ),
            column(6,
                div(id="bboxImageUi", align="center",
                    imageOutput("bboxImage", width=299, height=299))
            ),
            column(3, tableOutput("cnnPyTbl"))
        ),
        similarImgUi("similarImg")
    )),

    hr(),
    traceOutput("trace")
)
