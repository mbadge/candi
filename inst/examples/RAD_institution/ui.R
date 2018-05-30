fluidPage(
    shinyjs::useShinyjs(),
    theme = shinythemes::shinytheme("darkly"),
    div(id="title", align="center",
        titlePanel("Annotate Radiographs", windowTitle=WindowTitle())),

    # Id row: user name In / progress Out / test img/case In
    fluidRow(
        column(4, div(textInput("user_name", "User Name:", value = "Marcus"), align = "center")),
        column(4, div(textOutput("progressText"), align = "center")),
        column(4, div(uiOutput("imgIdUi"), align = "center"))  #shinyjs::hidden(uiOutput("imgIdUi")),
    ),
    hr(),

    # Image and Impression row:
    fluidRow(
        # User Impression ----
        column(4,
            shinyjs::hidden(div(id = "user_impression_panel", align = "center",
               # Classification ----------
               p(strong("What pathologies are present in the image?")),
               fluidRow(
                   column(width = 8,
                          checkboxGroupInput("pathologies", label=NULL, choices=kDXS_CHR)
                   ),
                   column(width = 4,
                          shiny::tags$br(),
                          actionButton("submit_classification", "Submit Classifications"))
               ),
               shiny::tags$br(),
               # Segmentation -------------
               div(id="segmentation",
                    p(strong("Draw a box around the evidence for each identified pathology (submit 1 box per pathology).")),
                    fluidRow(
                           div(id="seg_in_panel", align="center",
                               column(width = 6, shiny::tags$br(), tableOutput("coordinatesTable")),
                               column(width = 6,
                                      fluidRow(actionButton("submit_cardiomegaly", "Submit Cardiomegaly Segmentation")),
                                      fluidRow(actionButton("submit_emphysema", "Submit Emphysema Segmentation")),
                                      fluidRow(actionButton("submit_effusion", "Submit Effusion Segmentation"))
                               )
               ))),
               # Note --------------------
               p(strong("Note Impression:")),
               fluidRow(
                   column(width = 8,
                          textAreaInput("note", NULL, "Clinical Impression")
                   ),
                   column(width = 4,
                          actionButton("submit_note", "Submit Impression")
                   )
               )
        ))),

        # Radiograph
        column(width = 4, offset = 1,
               shinyjs::hidden(
                   div(id="image_panel", align="center",
                       imageOutput("radiographImage", width = 299, height=299,
                                   brush = brushOpts(id = "bbox_brush", resetOnNew = TRUE))
                   )),
               shiny::tags$br(),
               div(actionButton(inputId = "submit_btn", label = "Begin Annotating"),
                   align = "center")
        ),

        column(3,
               div(align = "center", p("Data Access: Institutional")),
               p("Image stored on institutional server"),
               verbatimTextOutput("imgFp"),
               p("Annotations stored on institutional server"),
               verbatimTextOutput("annFp"),  # Superfluous rendering to match appearance w/ previous verbatimTextOutput
               hr(),

               p("Downloads:"),
               div(align="center",
                   downloadButton("downloadClassification", label="Classification Annotations"),
                   downloadButton("downloadSegmentation", label="Segmentation Annotations"),
                   downloadButton("downloadClinicalNote", label="Clinical Impression Notes")
               )
        )
    ), hr(),

    # Initial Usage Instructions
    div(id = "init_help_panel", algin = "center",
        verbatimTextOutput("helpPrint")
    )
)
