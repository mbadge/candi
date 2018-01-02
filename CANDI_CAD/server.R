function(input, output, session) {
    # Reactive elements bound to lower camel case names
    # Reactive Conductors -----------------
    idDf <- reactive({
        x <- map(kID_FIELDS, function(x) x=input[[x]])  # collect form id fields
        x %<>% as.data.frame() %>% purrr::set_names(c(kID_FIELDS))
        #!TODO: add session id to ids
    })
    usrInptDf <- reactive({
        data.frame(
            pathologies = toString(input$dxChkbxIn),
            clinical_note = input$noteTxtIn)
    })

    # Compute similarities of historical images for the current test image
    testImgPcDf <- reactive({test_pc_df %>%
        filter(img_id == input$mainImageId) %>%
        select(starts_with("PC"))
    })
    similarImgsDf <- reactive({
        dist_df <- hist_pc_df %>%
            nest(-img_id, .key = "PCs") %>%
            mutate(test_hist_dist = map_dbl(PCs,
                ~dist(rbind(.x, testImgPcDf())))) %>%
            select(-PCs)
        dist_df$relative_difference <- dist_df$test_hist_dist / max(dist_df$test_hist_dist)
        dist_df %<>% select(img_id, relative_difference)

        similar_images_df <- inner_join(dist_df, hist_imgs_df, by="img_id") %>%
            arrange(relative_difference)
    })

    cdata <- session$clientData
    usage_data <- reactive({
        cnames <- names(cdata)
        cvals <- lapply(cnames, function(name) {cdata[[name]]})
        cvals %>% set_names(cnames)
    })

    # Serve outputs ------------------------------------------------------------
    # Test Image Outputs ---------------
    # Radiograph
    output$mainImage <- renderImage({
        filename <- stringr::str_interp("${kTEST_IMG_IN_DIR}/${input$mainImageId}.jpg")
        return(list(src = filename, filetype="image/jpeg", alt="Main Radiograph"))
    }, deleteFile = FALSE)

    # CNN Test Image Predictions
    output$cnnPyTbl <- renderTable({
        test_py_df %>%
            filter(img_id == input$mainImageId) %>%
            select(-img_id) %>%
            gather(key=diagnosis, value=probability) %>%
            arrange(desc(probability))
    })

    # Similar Image Search ---------------
    pcaPlot <- reactive({
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
    output$pcaPlot <- renderPlot({pcaPlot()})

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
        img_fp <- file.path(kHIST_IMG_IN_DIR, str_c(hoverImgId(), ".jpg"))
        img <- EBImage::readImage(img_fp) %>% Viz.Image()
    })
    output$hoverYTbl <- renderTable({
        similarImgsDf() %>%
            filter(img_id == hoverImgId()) %>%
            select(one_of(str_case_title(kDXS_CHR))) %>%
            gather(key=diagnosis, value=appreciated) %>%
            arrange(desc(appreciated))
    })
    output$hoverNoteText <- renderText({
        hist_imgs_df %>%
            filter(img_id == hoverImgId()) %>%
            use_series(findings) %||% "radiology note missing"
    })


    # Reactive Ui Elements --------------------------------------------------
    output$colorUi <- renderUI({
        colorable_cols <- hist_imgs_df %>%
            `[`(setdiff(names(.), str_c("PC", 1:10))) %>%
            keep(.p=Not(is.character)) %>%
            names() %>%
            set_names(., str_case_title(.))
        selectInput("colorIn", "Color By:", choices=c("None", colorable_cols), selected = "None")
    })
    output$facetRowUi <- renderUI({
        facetable_cols <- hist_imgs_df %>%
            keep(.p=Or(is.factor, is.logical)) %>%
            names() %>%
            set_names(., str_case_title(.))
        selectInput("facetRowIn", "Split rows by:", c(None='.', facetable_cols))
    })
    output$facetColUi <- renderUI({
        facetable_cols <- hist_imgs_df %>%
            keep(.p=Or(is.factor, is.logical)) %>%
            names() %>%
            set_names(., str_case_title(.))
        selectInput("facetColIn", "Split columns by:", c(None='.', facetable_cols))
    })

    # Reactive Event Handlers --------------------------------------------------
    observeEvent(input$submit_impression, {
        usr_input <- bind_cols(idDf(), usrInptDf()) %>%
            add_column(timestamp = date_time_stamp(), .before=1)
        log <- bind_cols(idDf(), usage_data()) %>%
            add_column(timestamp = date_time_stamp(), .before=1)
        save_usr_input(usr_input)
        save_usr_usage(log)
    })
    observe(
        updateSelectInput(session, "similarImageId",
                          choices=similarImgsDf()$img_id,
                          selected = similarImgsDf()[1, "img_id"]))

    # UI toggle panel events
    shinyjs::onclick("toggleCnnPy",
        shinyjs::toggle("cnnPyUi", anim=TRUE))
    shinyjs::onclick("toggleBrowseSimilar",
        shinyjs::toggle("browseSimilarUi", anim=TRUE))



    # Trace ---------------------------------
    # Supplementary Info panel for diagnostics
    shinyjs::onclick("toggleAdvanced",
                     shinyjs::toggle("advanced"))

    output$mouseHoverTable <- renderPrint(input$radHover %>% str())
    # Values from cdata returned as text
    output$clientdataText <- renderText({
        cnames <- names(cdata)
        allvalues <- lapply(cnames, function(name) {
            paste(name, cdata[[name]], sep = " = ")
        })
        paste(allvalues, collapse = "\n")
    })

    # DIAGNOSTIC SERVER CODE GENERATED BY `cg.R`

    # Reactive Inputs
    output$colorInPrint <- renderPrint(input$colorIn %>% str())
    output$facetRowInPrint <- renderPrint(input$facetRowIn %>% str())
    output$facetColInPrint <- renderPrint(input$facetColIn %>% str())
    output$radiologistPrint <- renderPrint(input$radiologist %>% str())
    output$mainImageIdPrint <- renderPrint(input$mainImageId %>% str())
    output$dxChkbxInPrint <- renderPrint(input$dxChkbxIn %>% str())
    output$noteTxtInPrint <- renderPrint(input$noteTxtIn %>% str())
    output$xPrint <- renderPrint(input$x %>% str())
    output$yPrint <- renderPrint(input$y %>% str())

    # Reactive Conductors
    output$idDfPrint <- renderPrint(idDf() %>% str())
    output$usrInptDfPrint <- renderPrint(usrInptDf() %>% str())
    output$testImgPcDfPrint <- renderPrint(testImgPcDf() %>% str())
    output$similarImgsDfPrint <- renderPrint(similarImgsDf() %>% str())
    output$usage_dataPrint <- renderPrint(usage_data() %>% str())
    output$pcaPlotPrint <- renderPrint(pcaPlot() %>% str())
    output$hoverImgIdPrint <- renderPrint(hoverImgId() %>% str())
}

