function(input, output, session) {
    # Reactive elements bound to lower camel case names

    # Invoke modules ----------------------
    usrInptDf <- callModule(impression, "impression")
    similarImgDf <- callModule(similarImg, "similarImg",
                               testImgId = reactive(input$testImgId),
                               test_pc_df = test_pc_df,
                               hist_imgs_df = hist_imgs_df,
                               hist_img_dir = kHIST_IMG_IN_DIR,
                               dx_chr = kDXS_CHR)

    # Reactive Conductors -----------------
    idDf <- reactive({
        x <- map(kID_FIELDS, function(x) x=input[[x]])  # collect form id fields
        x %<>% as.data.frame() %>% purrr::set_names(c(kID_FIELDS))
        #!TODO: add session id to ids
    })

    cdata <- session$clientData
    usageLst <- reactive({
        cnames <- names(cdata)
        cvals <- lapply(cnames, function(name) {cdata[[name]]})
        cvals %>% set_names(cnames)
    })

    # Serve outputs -----------------------
    # Radiographs
    output$mainImage <- renderImage({
        filename <- stringr::str_interp("${kTEST_IMG_IN_DIR}/${input$testImgId}.jpg")
        return(list(src = filename, filetype="image/jpeg", alt="Main Radiograph"))
    }, deleteFile = FALSE)

    # CNN Test Image Predictions
    output$cnnPyTbl <- renderTable({
        test_py_df %>%
            filter(img_id == input$testImgId) %>%
            select(-img_id) %>%
            gather(key=diagnosis, value=probability) %>%
            arrange(desc(probability))
    })


    # Reactive Event Handlers --------------------------------------------------
    observeEvent(input$submit_impression, {
        usr_input <- bind_cols(idDf(), usrInptDf()) %>%
            add_column(timestamp = date_time_stamp(), .before=1)
        log <- bind_cols(idDf(), usageLst()) %>%
            add_column(timestamp = date_time_stamp(), .before=1)
        save_usr_input(usr_input)
        save_usr_usage(log)
    })

    # UI toggle panel events
    shinyjs::onclick("toggleCnnPy",
        shinyjs::toggle("cnnPyUi", anim=TRUE))

    callModule(trace, "trace",
               radHover = reactive(input$radHover),
               radiologistIn = reactive(input$radiologist),
               testImgIdIn = reactive(input$testImgId),
               idDf = reactive(idDf()),
               usrInptDf = reactive(usrInptDf()),
               similarImgDf = reactive(similarImgDf()),
               usageLst = reactive(usageLst()))
}
