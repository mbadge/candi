#' Shiny module for PC based similar historical image lookup
#'
#' @param id chr(1) namespace for the module, decided by the caller at the time the module is used
#' @return \code{\link[shiny]{tagList}} of similar image search ui components
#'
#' @family shiny_module
#' @seealso \code{\link{similarImg}}
#' @export
#' @examples
#' if (interactive()) {
#'
#' library(shiny)
#'
#' kN_HIST_IMGS <- 100
#' test_imgs <- candi::radiographs %>%
#'     filter(img_id %in% candi::test_imgs) %>%
#'     with_sep(left_join, cases, by="case")
#' hist_imgs <- radiographs %>%
#'     filter(img_id %ni% candi::test_imgs) %>%
#'     sample_n(kN_HIST_IMGS) %>%
#'     with_sep(left_join, candi::cases, by="case")
#'
#' shinyApp(
#'     ui = fluidPage(
#'         selectInput("imgIdIn", "Image Id:", choices = test_imgs$img_id),
#'         similarImgUi("similarImg")
#'     ),
#'     server = function(input, output, session) {
#'         callModule(similarImg, "similarImg",
#'                    testImgId = reactive(input$imgIdIn),
#'                    test_imgs_df = test_imgs,
#'                    hist_imgs_df = hist_imgs)
#'     }
#' )
#' }
similarImgUi <- function(id) {
    ns <- NS(id)

    tagList(
        a(id=ns("toggleBrowseSimilar"), "Show/hide Similar Image Records"),
        div(id=ns("browseSimilarUi"), align = "center",
            h3("Search for Similar Historical Images"),
            fluidRow(
                # User Input Panel -------------------
                column(2,
                       wellPanel(
                           selectInput(ns("x"), label="x variable:", choices=str_c("PC", 1:10), selected="PC1"),
                           selectInput(ns("y"), label="y variable:", choices=str_c("PC", 1:10), selected="PC2"),
                           uiOutput(ns("colorUi")),
                           uiOutput(ns("facetRowUi")),
                           uiOutput(ns("facetColUi"))
                       )),
                # PC scatter -------------------------
                column(3,
                       plotOutput(ns('pcaPlot'), width="100%", click="plot_click",
                                  hover = hoverOpts(id=ns("plot_hover"), delayType="throttle"),
                                  brush = brushOpts(id=ns("plot_brush")))
                ),
                column(4,
                       div(id=ns("similarImageUi"), align="center",
                           imageOutput(ns("hoverImage"), width = 299, height=299))
                ),
                column(3,
                       tableOutput(ns("hoverYTbl")),  # Diagnosis Tbl
                       textOutput(ns("hoverNoteText"))  # Free Text Note
                )
            )
        )
    )
}


#' CANDI PC-based Similar Image Search Shiny Module Server
#'
#' @param testImgId reactive expression that returns chr(1) img_id of current test image
#' @param img_dir chr(1)
#' @param input shiny session input
#' @param output shiny session output
#' @param session shiny session id
#' @param test_imgs_df radiograph-major data.frame with at least img_id and PCs
#' @param hist_imgs_df radiograph-major data.frame with at least img_id and PCs,
#'  and also other cols to map to scatter plot aesthetics
#'
#' @return data.frame similarImgsDf
#'
#' @family shiny_module
#' @seealso \code{\link{similarImgUi}}
#' @export
similarImg <- function(input, output, session,
                       testImgId,
                       test_imgs_df,
                       hist_imgs_df,
                       dxs_chr = candi::candiOpt(dxs_chr),
                       img_dir = candi::candiOpt(large_img_dir))
{
    ggplot2::theme_set(theme_dark())

    testImgPcDf <- reactive({
        test_img_pc_df <- test_imgs_df %>%
            filter(img_id == testImgId()) %>%
            select(starts_with("PC"))
        test_img_pc_df
    })

    similarImgsDf <- reactive({
        dist_df <- hist_imgs_df %>%
            select(img_id, starts_with("PC")) %>%
            nest(-img_id, .key = "PCs") %>%
            mutate(test_hist_dist = map_dbl(PCs,
                                            ~dist(rbind(.x, testImgPcDf()))
                                            )) %>% select(-PCs)

        dist_df$relative_difference <- dist_df$test_hist_dist / max(dist_df$test_hist_dist)
        dist_df %<>% select(img_id, relative_difference)

        similar_images_df <- inner_join(dist_df, hist_imgs_df, by="img_id") %>%
            arrange(relative_difference)
        similar_images_df
    })

    # Similar Image Search ---------------
    output$pcaPlot <- renderPlot({
        req(input$x, input$y, input$colorIn, input$facetRowIn, input$facetColIn)

        hist_imgs_df %<>% map_if(is.factor, fct_explicit_na) %>% as.data.frame()
        p <- ggplot(hist_imgs_df, aes_string(x=input$x, y=input$y))

        if (input$colorIn != 'None') {
            p <- p +
                aes_string(color=input$colorIn)
        }
        # Overlay current test image
        p <- p + geom_point(data=testImgPcDf(), aes_string(x=input$x, y=input$y),
                            inherit.aes=FALSE, color="blue", fill="blue",
                            alpha=0.5, size=8)
        # Plot historical points
        p <- p + geom_point(alpha=0.5)

        facets <- paste(input$facetRowIn, '~', input$facetColIn)
        if (facets != '. ~ .')
            p <- p + facet_grid(facets)
        p + theme(legend.position = "bottom",
                  axis.text=element_blank(), axis.title = element_blank())
    })

    # Hovered Image Record -------------
    # show image use is hovering over, or default to the most similar img
    hoverImgId <- reactive({
        hover_row <- shiny::nearPoints(hist_imgs_df, input$plot_hover, input$x, input$y, maxpoints = 1)
        if(nrow(hover_row) == 0) hover_row <- similarImgsDf()[1, ]
        hover_row$img_id
    })
    output$hoverImage <- renderPlot({
        img_fp <- file.path(img_dir, str_c(hoverImgId(), ".jpg"))
        img <- EBImage::readImage(img_fp) %>% Viz.Image()
    })
    output$hoverYTbl <- renderTable({
        similarImgsDf() %>%
            filter(img_id == hoverImgId()) %>%
            select(one_of(dxs_chr)) %>%
            gather(key=diagnosis, value=appreciated) %>%
            arrange(desc(appreciated))
    })
    output$hoverNoteText <- renderText({
        hist_imgs_df %>%
            filter(img_id == hoverImgId()) %>%
            use_series(findings) %||% "radiology note missing"
    })

    # Reactive Ui Elements ---------------
    output$colorUi <- renderUI({
        ns <- session$ns
        colorable_cols <- hist_imgs_df %>%
            `[`(setdiff(names(.), str_c("PC", 1:10))) %>%
            keep(.p=Not(is.character)) %>%
            names() %>%
            set_names(., str_case_title(.))
        selectInput(ns("colorIn"), "Color By:", choices=c("None", colorable_cols), selected = "None")
    })
    output$facetRowUi <- renderUI({
        ns <- session$ns
        facetable_cols <- hist_imgs_df %>%
            keep(.p=Or(is.factor, is.logical)) %>%
            names() %>%
            set_names(., str_case_title(.))
        selectInput(ns("facetRowIn"), "Split rows by:", c(None='.', facetable_cols))
    })
    output$facetColUi <- renderUI({
        ns <- session$ns
        facetable_cols <- hist_imgs_df %>%
            keep(.p=Or(is.factor, is.logical)) %>%
            names() %>%
            set_names(., str_case_title(.))
        selectInput(ns("facetColIn"), "Split columns by:", c(None='.', facetable_cols))
    })

    return(similarImgsDf)
}
