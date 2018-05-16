fluidPage(
    shinyjs::useShinyjs(),
    theme = shinythemes::shinytheme("darkly"),
    div(id="title", align="center",
        titlePanel("Annotate Testing Data", windowTitle=WindowTitle())),

    # 2 horizontal sections -- the radiograph will be the only thing on the second section so
    # the page can adapt to the large variation in full radiograph sizes.
    fluidRow(
        column(3,
            div(align="center", p(strong("User Controls"))),
            # User name / progress / submit
            textInput("user_name", "User Name:", value = "Marcus"),
            uiOutput("imgIdUi"),  #shinyjs::hidden(uiOutput("imgIdUi")),
            hr(),

            # Submitting
            actionButton(inputId = "submit_btn", label = "Begin Annotating"),
            textOutput("progressText")
        ),

        # User Impression ----
        # Wrap in hidden div so I can reveal only after user begins
        column(4, shinyjs::hidden(div(id = "user_impression_panel", align = "center",
            impressionInput(id = "user_impression",
                            dx_chr = candiOpt(dxs_chr),
                            include_demographics = kINCLUDE_DEMOGRAPHICS,
                            include_technical = kINCLUDE_TECHNICAL)
        ))),

        # Patient Records ----
        column(5,
               patientMedicalRecordOutput(id = "display_emr")
        )
    ),
    hr(),

    radiographOutput("main_image")
)
