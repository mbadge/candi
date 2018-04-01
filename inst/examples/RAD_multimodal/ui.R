fluidPage(
    shinyjs::useShinyjs(),
    theme = shinythemes::shinytheme("darkly"),
    div(id="title", align="center",
        titlePanel("Gold Standard Benchmarking Dataset", windowTitle="Benchmarking Trial")),

    # 2 horizontal sections -- the radiograph will be the only thing on the second section so
    # the page can adapt to the large variation in full radiograph sizes.
    fluidRow(
        column(3,
            div(align="center", p(strong("User Controls"))),
            # User name / progress / submit
            textInput("userNameIn", "User Name:", value = "Marcus"),
            uiOutput("imgIdUi"),  #shinyjs::hidden(uiOutput("imgIdUi")),
            hr(),
            # Control the main iamge
            checkboxInput("invertImgIn", "Invert Image?", value=FALSE),
            hr(),

            # Submit
            textInput("txtIn", "Comments", placeholder = "optionally submit comment"),
            actionButton("submitBtn", "Submit Impression"),
            hr(),

            # Progress
            textOutput("progressTxt")
        ),

        # User Impression ----
        column(4, div(align="center", p(strong("User Impression"))),
               checkboxGroupInput("dxChkbxIn", label="Diagnoses", choices=kDXS_CHR),
               sliderInput("ageIn", "Patient Age", min = 0, max=100, value=60),
               radioButtons("sexIn", "Patient Sex", choices = c("female"="f", "male"="m"), inline = TRUE),
               radioButtons("viewIn", "Radiograph View", choices = c("ap", "lateral", "pa"), inline=TRUE),
               radioButtons("cassetteIn", "Cassette Orientation", choices = c("portrait", "landscape"), inline=TRUE)
        ),

        # Patient Records ----
        column(5,
               patientMedicalRecordOutput(id = "displayEMR")
        )
    ), hr(),

    # Main Radiograph ----
    div(id = "imgOut", align="center",
        EBImage::displayOutput("mainImage")
    )
)
