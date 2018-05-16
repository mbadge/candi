fluidPage(
    shinyjs::useShinyjs(),
    theme = shinythemes::shinytheme("darkly"),
    div(id="title", align="center",
        titlePanel("CXR Training Interpretation Trial", windowTitle="RAD_image")),

    # 2 horizontal sections -- the radiograph will be the only thing on the second section so
    # the page can adapt to the large variation in full radiograph sizes.
    fluidRow(
        column(3,
            div(align="center", p(strong("User Controls"))),
            # User name / progress / submit
            textInput("user_name", "User Name:", value = "Marcus"),
            uiOutput("imgIdUi"),  #shinyjs::hidden(uiOutput("imgIdUi")),
            hr(),

            # Submit
            actionButton("submit_btn", "Submit Impression"),
            hr(),

            # Progress
            textOutput("progressTxt")
        ),

        column(9,
            impressionInput(id = "user_impression",
                            dx_chr = candiOpt(dxs_chr),
                            include_demographics = kINCLUDE_DEMOGRAPHICS,
                            include_technical = kINCLUDE_TECHNICAL)
        )
    ), hr(),

    radiographOutput("main_image")
)
