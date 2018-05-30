fluidPage(
    shinyjs::useShinyjs(),
    theme = shinythemes::shinytheme("darkly"),
    div(id="title", align="center",
        titlePanel("CANDI Public Radiograph Annotation Dashboard", windowTitle=WindowTitle())),

    sidebarLayout(
        sidebarPanel(
            textInput("user_name", "User Name:", value = "Marcus"),
            textInput("image_url", "Public Image URL:", value = "http://www.catster.com/wp-content/uploads/2017/12/A-gray-kitten-meowing.jpg"),

            fluidRow(
                column(width = 8,
                    selectInput("img_id", "Fetch image from OpenI", choices=names(iu_db_lut))
                ),
                column(width = 4,
                    shiny::tags$br(),
                    actionButton("openiBtn", "Load OpenI Image")
                )
            ),

            hr(),

            p("Data Locations:"),
            a(href=gsURL, p("Annotations stored on Google Drive"),
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
            # Display Radiograph ---------------
            fluidRow(
                column(4, offset=3,
                    div(id="image", align="center",
                        imageOutput("radiographImage", width = 299, height = 299,
                                    brush = brushOpts(id = "bbox_brush", resetOnNew = TRUE))
            ))),

            # Classification ----------
            fluidRow(
                column(width = 8,
                       checkboxGroupInput("pathologies", "What pathologies are present in the image?",
                                          choices=kDXS_CHR)
                ),
                column(width = 4,
                       shiny::tags$br(),
                       actionButton("submit_classification", "Submit Classifications"))
            ),

            # Segmentation -------------
            div(id="segmentation",
                p(strong("Draw a box around the evidence for each identified pathology (submit 1 box per pathology).")),
                fluidRow(
                    div(id="seq_in_panel", align="center",
                        column(width = 5,
                               fluidRow(actionButton("submit_cardiomegaly", "Submit Cardiomegaly Segmentation")),
                               fluidRow(actionButton("submit_emphysema", "Submit Emphysema Segmentation")),
                               fluidRow(actionButton("submit_effusion", "Submit Effusion Segmentation"))
                        ),
                        column(width = 4, shiny::tags$br(), tableOutput("coordinatesTable"))
            ))),

            # Note --------------------
            p(strong("Note Impression:")),
            textAreaInput("note", NULL, "Clinical Impression", width="650px"),
            actionButton("submit_note", "Submit Impression")
    ))
)
