fluidPage(
    shinyjs::useShinyjs(),
    theme = shinytheme("darkly"),
    div(id="title", align="center",
        titlePanel("CANDI for Computer Assisted Diagnosis", windowTitle="CANDI CAD")),

    # Main image row: 3 columns [img/id, radiologist impression, cnn prediction]
    fluidRow(
        # Main ID/Image panel
        column(4,
            fluidRow(
                column(6,
                    textInput("radiologist", "Radiologist Name:", value = "Marcus")),
                column(6,
                    selectInput("testImgId", "Image:", choices=stem(test_img_fns)))
            ),
            hr(),
            # Radiograph --------------
            div(id="mainImageUi", align="center",
                imageOutput("mainImage", width = 299, height=299,
                    hover = hoverOpts(id = "radHover", delayType = "throttle")))
        ),
        # Current Radiologist Impression ----------
        column(4,
            impressionUi("impression", dx_chr=kDXS_CHR),  # Shiny module
            actionButton("submit_impression", "Submit Impression")
        )
    ),

    # #! INSERT MODE RANDOMIZER STATE BANNER
    # textOutput("cadModeStr")

    #! conditionalPanel("output.is_assist_mode", )
    #! \/ Control from py to similarImgUi output as a function of randomizer state
    fluidRow(
        # ConvNet Assistance ---------
        column(4,
	        a(id="toggleCnnPy", "Show/hide CNN Prediction"),
	        #shinyjs::hidden(
	        div(id="cnnPyUi", wellPanel(
                h3('CNN Prediction'),
	            tableOutput("cnnPyTbl")
    )))),  # end cnn py fluidrow
    hr(),

    # Optional Similar Image Browser Utility
    similarImgUi("similarImg"),  # Shiny module
    hr(),
    #! /\ Control from py to similarImgUi output as a function of randomizer state

    traceOutput("trace")
)
