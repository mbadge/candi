fluidPage(
    shinyjs::useShinyjs(),
    theme = shinythemes::shinytheme("darkly"),
    div(id="title", align="center",
        titlePanel("Annotate Cases", windowTitle=WindowTitle())),

    # Id row: user name In / progress Out / test img/case In
    fluidRow(
        column(4, div(textInput("user_name", "User Name:", value = "Marcus"), align = "center")),
        column(4, div(textOutput("progressText"), align = "center")),
        column(4, div(uiOutput("imgIdUi"), align = "center"))  #shinyjs::hidden(uiOutput("imgIdUi")),
    ),
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
                    column(6,
                           impressionInput(id = "user_impression",
                                           dx_chr = candiOpt(dxs_chr),
                                           include_demographics = kINCLUDE_DEMOGRAPHICS,
                                           include_technical = kINCLUDE_TECHNICAL)),
                    column(6,
                           histImpOutput(id = "historical_impression"))
                )
            )),
            div(actionButton(inputId = "submit_btn", label = "Begin Annotating"),
                align = "center")
        ),
        column(8,
            shinyjs::hidden(div(id = "image_panel",
                shinycssloaders::withSpinner(caseOutput("main_image"))
            ))
        )
    )
)
