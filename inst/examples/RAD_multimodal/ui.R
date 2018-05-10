fluidPage(
    shinyjs::useShinyjs(),
    theme = shinythemes::shinytheme("darkly"),
    div(id="title", align="center",
        titlePanel("Gold Standard Benchmarking Dataset", windowTitle="RAD_multimodal")),

    # 2 horizontal sections -- the radiograph will be the only thing on the second section so
    # the page can adapt to the large variation in full radiograph sizes.
    fluidRow(
        column(3,
            div(align="center", p(strong("User Controls"))),
            # User name / progress / submit
            textInput("userNameIn", "User Name:", value = "Marcus"),
            uiOutput("imgIdUi"),  #shinyjs::hidden(uiOutput("imgIdUi")),
            hr(),

            # Submit
            actionButton("submitBtn", "Submit Impression"),
            hr(),

            # Progress
            textOutput("progressTxt")
        ),

        # User Impression ----
        column(4,
            impressionInput(id = "usrImpression",
                            dx_chr = candiOpt(dxs_chr),
                            include_demographics = kINCLUDE_DEMOGRAPHICS,
                            include_technical = kINCLUDE_TECHNICAL)
        ),

        # Patient Records ----
        column(5,
               patientMedicalRecordOutput(id = "displayEMR")
        )
    ), hr(),

    radiographOutput("main_image")
)
