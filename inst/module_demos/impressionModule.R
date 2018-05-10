# dx_chr <- candiOpt(dxs_chr)
# include_demographics = TRUE
# include_technical = FALSE
#
# ns <- NS("tempId")
# tagList(
#     h3("Your Impression"),
#     p(strong("Is there radiographic evidence for a diagnosis?")),
#
#     # Diagnosis_lglndx ----------
#     checkboxGroupInput(ns("dxChkbxIn"), label=NULL, choices=dx_chr),
#     hr(),
#
#     # Demographics ----
#     if (include_demographics) {
#         tagList(
#             p(strong("Demographics:")),
#             sliderInput("ageIn", "Patient Age", min = 0, max=100, value=60),
#             radioButtons("sexIn", "Patient Sex", choices = c("female"="f", "male"="m"), inline = TRUE)
#         )
#     },
#
#     # Impression_chr1 ---------
#     p(strong("Clinical Note:")),
#     textAreaInput(ns("noteTxtIn"), NULL, "Clinical Impression", resize = "both")
# )




impressionInput <- function(id, dx_chr,
                            include_demographics = FALSE,
                            include_technical = FALSE)
{
    ns <- NS(id)

    tagList(
        h3("Your Impression"),
        p(strong("Is there radiographic evidence for a diagnosis?")),

        # Diagnosis_lglndx ----------
        checkboxGroupInput(ns("dxChkbxIn"), label=NULL, choices=dx_chr),
        hr(),

        # Demographics ----
        if (include_demographics) {
            tagList(
                p(strong("Demographics:")),
                sliderInput(inputId = ns("ageIn"), label = "Patient Age",
                            min = 0, max=100, value=60),
                radioButtons(inputId = ns("sexIn"), label = "Patient Sex",
                             choices = c("female"="f", "male"="m"), inline = TRUE)
            )
        },

        # Technical ----
        if (include_technical) {
            tagList(
                p(strong("Technical:")),
                radioButtons(inputId = ns("viewIn"), label = "Radiograph View",
                             choices = c("ap", "lateral", "pa"), inline=TRUE),
                radioButtons(inputId = ns("cassetteIn"), label = "Cassette Orientation",
                             choices = c("portrait", "landscape"), inline=TRUE)
            )
        },

        # Impression_chr1 ---------
        p(strong("Clinical Note:")),
        textAreaInput(ns("noteTxtIn"), NULL, "Clinical Impression", resize = "both")
    )
}


#' CANDI User Impression Interface Module Server Function
#'
#' @param input,output,session shiny module server-client mgmt
#'
#' @return a function, wrapped in S3 class reactive.  see \code{\link[shiny]{reactive}}
#'
#' @family shiny_module
#' @seealso \code{\link{impressionInput}}
#'
#' @importFrom tibble add_column
#' @export
impression <- function(input, output, session,
                       include_demographics = FALSE,
                       include_technical = FALSE)
{
    usrImpressionDf <- shiny::reactive({
        impression_df <- data.frame(
            pathologies = toString(input$dxChkbxIn),
            clinical_note = input$noteTxtIn
        )

        if (include_demographics) {
            impression_df %<>% tibble::add_column(age = input$ageIn)
            impression_df %<>% tibble::add_column(sex = input$sexIn)
        }

        if (include_technical) {
            impression_df %<>% tibble::add_column(view = input$viewIn)
            impression_df %<>% tibble::add_column(cassette = input$cassetteIn)
        }

        impression_df
    })

    return(usrImpressionDf)
}



# FLAGS
include_demographics <- TRUE
include_technical <- TRUE

shinyApp(
    ui = fluidPage(fluidRow(
        column(6,
            impressionInput("impression", dx_chr = candiOpt(dxs_chr),
                            include_demographics = include_demographics,
                            include_technical = include_technical)
        ),
        column(6,
               shiny::tags$strong("User Impression:"),
               tableOutput("usrImpressionTable")
        )
    )),

    server = function(input, output, session) {
        usrImpressionDf <- callModule(impression, "impression",
                                      include_demographics = include_demographics,
                                      include_technical = include_technical)
        output$usrImpressionTable <- renderTable(usrImpressionDf())
    }
)
