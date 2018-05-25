fluidPage(
    shinyjs::useShinyjs(),
    theme = shinythemes::shinytheme("darkly"),
    div(id="title", align="center",
        titlePanel("Annotate Your Radiographs", windowTitle=WindowTitle())),

    sidebarLayout(
    	# User Controls --------------------
        sidebarPanel(
            textInput("user_name", "User Name:", value = "Marcus"),
            fileInput("filesInputDf", label="Upload files:",
                      multiple=TRUE, accept="image/jpeg"),

            conditionalPanel("output.isUploaded",
                             uiOutput("img_select")),  # Drop-down menu of all images files avail
            hr(),

            p("Data Locations:"),
            conditionalPanel("output.isUploaded",
                tagList(
                    p("Image temporarily stored on server:"),
                    verbatimTextOutput("imgTmpFp")
                )
            ),
            a(href=gsURL, p("Annotations stored on Google Drive:"),
              verbatimTextOutput("annotationURL")),
            hr(),

            p("Downloads:"),
            div(align="center",
                downloadButton("downloadClassification", label="Classification Annotations"),
                downloadButton("downloadSegmentation", label="Segmentation Annotations"),
                downloadButton("downloadClinicalNote", label="Clinical Impression Notes")
            )
        ),

        mainPanel(
            # Offer example images only until the user uploads some
            conditionalPanel("output.notUploaded",
                div(align="center",
                    h1("Need sample images?? Try these:"),
                    downloadButton("downloadSampleImages", label = "Sample Images")
                )
            ),
            # Then and only then, display the radiograph
            conditionalPanel("output.isUploaded",
                tagList(
                    # Display Radiograph ---------------
                    fluidRow(
                        column(4, offset=3,
                            div(id = "imgOut",
                                imageOutput("radiographImage", width = 299, height=299,
                                            brush = brushOpts(id = "bbox_brush", resetOnNew = TRUE)
                                )
                            )
                        )
                    ),

                    # Classification ----------
                    p(strong("What pathologies are present in the image?")),
                    fluidRow(
                        column(width = 5, offset=1,
                               checkboxGroupInput("pathologies", label=NULL, choices=kDXS_CHR)
                        ),
                        column(width = 4,
                               actionButton("submit_classification", "Submit Classifications")
                        )
                    ),

                    # Segmentation ----------
                    div(id="segmentation",
                        p(strong("Draw a box around the evidence for each identified pathology.")),
                        fluidRow(
                            div(id="seq_in_panel", align="center",
                                column(width = 5,
                                       fluidRow(actionButton("submit_cardiomegaly", "Submit Cardiomegaly Segmentation")),
                                       fluidRow(actionButton("submit_emphysema", "Submit Emphysema Segmentation")),
                                       fluidRow(actionButton("submit_effusion", "Submit Effusion Segmentation"))
                                ),
                                column(width = 4, shiny::tags$br(), tableOutput("coordinatesTable"))
                    ))),

                    # Note ---------------
                    p(strong("Note Impression:")),
                    textAreaInput("note", NULL, "Clinical Impression", width="650px"),
                    actionButton("submit_note", "Submit Impression")
    ))))
)
