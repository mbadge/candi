similarImgUi <- function(id) {
    ns <- NS(id)

    tagList(
        a(id=ns("toggleBrowseSimilar"), "Show/hide Similar Image Records"),
        shinyjs::hidden(div(id=ns("browseSimilarUi"),
            fluidRow(
                # User Input Panel -------------------
                column(4,
                       wellPanel(
                           selectInput(ns("x"), label="x variable:", choices=str_c("PC", 1:10), selected="PC1"),
                           selectInput(ns("y"), label="y variable:", choices=str_c("PC", 1:10), selected="PC2"),
                           uiOutput(ns("colorUi")),
                           uiOutput(ns("facetRowUi")),
                           uiOutput(ns("facetColUi"))
                       )),
                # PC scatter -------------------------
                column(4,
                       plotOutput(ns('pcaPlot'), width="100%", click="plot_click",
                                  hover = hoverOpts(id=ns("plot_hover"), delayType="throttle"),
                                  brush = brushOpts(id=ns("plot_brush")))
                ),
                # Historical Documentation Panel -----------------
                column(4,
                       h3("Historical Documentation:"),
                       div(id=ns("similarImageUi"), align="center",
                           imageOutput(ns("hoverImage"), width = 299, height=299)),
                       tableOutput(ns("hoverYTbl")),  # Diagnosis Tbl
                       textOutput(ns("hoverNoteText"))  # Free Text Note
                )
            ),
            fluidRow(
                dataTableOutput(ns("brushedPointsTable"))
            )
        ))
    )
}

similarImg <- function(input, output, session, testImgId, test_pc_df, hist_imgs_df, hist_img_dir, dx_chr) {
    testImgPcDf <- reactive({
        test_img_pc_df <- test_pc_df %>%
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

    # Table of pca selected point region; or most similar points
    output$brushedPointsTable <- renderDataTable({
        brush_rows <- brushedPoints(hist_imgs_df, input$plot_brush, input$x, input$y)
        brush_imgs <- brush_rows$img_id
        df <- similarImgsDf()
        if (nrow(brush_rows) > 0) {df <- filter(df, img_id %in% brush_imgs)}
        df
    })

    # Hovered Image Record -------------
    # show image use is hovering over, or default to the most similar img
    hoverImgId <- reactive({
        hover_row <- shiny::nearPoints(hist_imgs_df, input$plot_hover, input$x, input$y, maxpoints = 1)
        if(nrow(hover_row) == 0) hover_row <- similarImgsDf()[1, ]
        hover_row$img_id
    })
    output$hoverImage <- renderPlot({
        img_fp <- file.path(hist_img_dir, str_c(hoverImgId(), ".jpg"))
        img <- EBImage::readImage(img_fp) %>% Viz.Image()
    })
    output$hoverYTbl <- renderTable({
        similarImgsDf() %>%
            filter(img_id == hoverImgId()) %>%
            select(one_of(str_case_title(dx_chr))) %>%
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

    # Event handler ----------------------
    # Toggle similar image Ui
    shinyjs::onclick("toggleBrowseSimilar",
                     shinyjs::toggle("browseSimilarUi", anim=TRUE))

    return(similarImgsDf)
}
