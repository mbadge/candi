#' Show statistics of a user's saved annotations data
#'
#' @param id chr(1) namespace for the module, decided by the caller at the time the module is used
#' @param input,output,session standard args to shiny server routine
#' @param app chr(1) name of application data directory in candiOpt(app_data_dir)
#' @param userName reactive that returns a chr(1) with the users name of data to fetch
#'
#' @return \code{\link[shiny]{tagList}} of ui components
#' @export
#'
#' @examples
#' shinyApp(
#'     ui = fluidPage(
#'         shinyjs::useShinyjs(),
#'         shiny::textInput("user_name", "User Name", "Marcus"),
#'         annotationReportOutput("ann_rpt")
#'     ),
#'     server = function(input, output, session) {
#'         callModule(annotationReportModule, "ann_rpt", app = "rad_case", userName = reactive(input$user_name))
#'     }
#' )
annotationReportOutput <- function(id) {
    ns <- shiny::NS(id)

    shiny::tagList(
        a(id = ns("toggleStats"), "Show/hide your annotation stats"),
        shinyjs::hidden(div(id = ns("annStats"),
            fluidRow(
                column(width = 6,
                    actionButton(ns("refresh_btn"), "Refresh Stat Display"),
                    shiny::div(align = "center",
                        p(strong("Your Findings:")),
                        textOutput(ns("nAnnotatedTxt")),
                        tableOutput(ns("dxFreqTbl"))
                    )
                ),
                column(width = 6,
                    shiny::div(align = "center",
                       p(strong("Your Speeds:")),
                       textOutput(ns("speedTxt")),
                       plotOutput(ns("speedHist")),
                       plotOutput(ns("speedTimeseries"))
                    )
                )
            )
        ))
    )
}

#' @export
#' @rdname annotationReportOutput
annotationReportModule <- function(input, output, session, app, userName) {
    ggplot2::theme_set(ggplot2::theme_dark())

    # ---- Conductors ----
    dxDf <- reactive({
        input$refresh_btn

        ann_df <- load_usr_input(user = userName(),
                       dir = file.path(candiOpt(app_data_dir), app, "usr_input"))

        if (is.null(ann_df) || nrow(ann_df) == 0) {
            cat("No records found")
            return(NULL)
        }

        dx_df <- ann_df %>%
            tidyr::separate_rows(pathologies, sep = ", ") %>%
            dplyr::select(-timestamp, -user_name, -clinical_note) %>%
            tibble::add_column(dx = TRUE) %>%
            tidyr::complete(img_id, pathologies, fill = list(dx = FALSE)) %>%
            tidyr::spread(key = pathologies, value = dx)
        dx_df %<>% rename("Normal" = `<NA>`)

        dx_df
    })

    logDf <- reactive({
        input$refresh_btn

        log_df <- load_usr_input(user = userName(),
                       dir = file.path(candiOpt(app_data_dir), app, "log"))

        if (is.null(log_df) || nrow(log_df) < 2) {
            cat("No records found")
            return(NULL)
        }
        log_df %<>% arrange(timestamp)
        log_df$delta_t <- log_df$timestamp - lag(log_df$timestamp)
        log_df %<>% filter(!is.na(img_id))

        log_df$delta_t %<>% lubridate::seconds() %>% as.integer()
        log_df$img_id %<>% as.factor() %>% forcats::fct_inorder()
        # Shiny screws up tabular representation of POSIXct, so coerce to char date
        log_df$timepoint <- 1:nrow(log_df)  # Used to order points on timeseries plot
        log_df$timestamp %<>% as.character() %>% str_replace("\\s.*", "")  # Used for subtitle
        log_df
    })

    subtitleTxt <- reactive({
        glue::glue("Data from {userName()}'s {nrow(logDf())} interpretations on {compose(stringFunc, unique)(logDf()$timestamp)}")
    })

    # ---- Outputs ----
    output$nAnnotatedTxt <- renderText({
        req(dxDf())
        N_cases <- nrow(dxDf())
        glue::glue("You've annotated {N_cases} cases.  In these cases the frequency of each finding was:")
    })

    output$dxFreqTbl <- renderTable({
        req(dxDf())
        summarise_all(dxDf()[, 2:ncol(dxDf())], mean) %>%
            t2idf(name = "finding", value = "prevalence") %>%
            arrange(desc(prevalence))
    })

    output$speedTxt <- renderText({
        req(logDf())
        glue::glue_data(.x = ggplot2::mean_se(logDf()$delta_t),
                  "You took an average of {round(y, 1)} seconds to interpret each image (std err {round(ymin, 1)} - {round(ymax, 1)}).")
    })

    output$speedHist <- renderPlot({
        req(logDf())
        p <- ggplot2::ggplot(logDf(), ggplot2::aes(x=delta_t)) +
            ggplot2::geom_histogram(bins = nrow(logDf()) / 2) +
            ggplot2::labs(
                title = "Distribution of Interpretation Durations",
                x = "Interpretation Time (seconds)",
                y = "Number of cases",
                subtitle = subtitleTxt()
            )
        p
    })

    output$speedTimeseries <- renderPlot({
        req(logDf())
        ggplot2::ggplot(logDf(), ggplot2::aes(x = img_id, y=delta_t)) +
            ggplot2::geom_point() +
            ggplot2::geom_line(group = 1) +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust=0.5)) +
            ggplot2::labs(
                title = "Time Spent Annotating Cases",
                x = "Case Number",
                y = "Interpretation Time (seconds)",
                subtitle = subtitleTxt()
            )
    })

    # ---- Observers ----
    shinyjs::onclick("toggleStats",
                     shinyjs::toggle("annStats", anim=TRUE))
}
