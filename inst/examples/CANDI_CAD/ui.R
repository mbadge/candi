fluidPage(
    shinyjs::useShinyjs(),
    theme = shinythemes::shinytheme("darkly"),
    div(id="title", align="center",
        titlePanel("CAD Evaluation Trial", windowTitle=WindowTitle())),

    # Id row: user name In / progress Out / test img/case In
    fluidRow(
        column(4, div(textInput("user_name", "User Name:", value = "Marcus"), align = "center")),
        column(4, div(textOutput("progressText"), align = "center")),
        column(4, div(uiOutput("imgIdUi"), align = "center"))  #shinyjs::hidden(uiOutput("imgIdUi")),
    ),
    hr(),
    div(textOutput("readerModeText"), align = "center"),
    hr(),

    # Image and Impression row:
    # hpi / user impression / hist impression / submit / main image
    fluidRow(
        # User Impression ----
        # Wrap in hidden div so I can reveal only after user begins
        column(4,
               shinyjs::hidden(div(id = "user_impression_panel", align = "center",
                                   hpiOutput(id = "hpi_output"),
                                   hr(),
                                   fluidRow(
                                      impressionInput(id = "user_impression",
                                                      dx_chr = candiOpt(dxs_chr),
                                                      include_demographics = kINCLUDE_DEMOGRAPHICS,
                                                      include_technical = kINCLUDE_TECHNICAL)
                                   )
               )),
               div(actionButton(inputId = "submit_btn", label = "Begin Trial"),
                   align = "center")
        ),
        column(8,
               shinyjs::hidden(div(id = "main_img_panel", align = "center",
                                   radiographOutput("main_image", height = "500px")))
        )
    ),
    hr(),

    # CNN toolkit
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
