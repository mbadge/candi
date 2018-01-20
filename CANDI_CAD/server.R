function(input, output, session) {
    # Reactive elements bound to lower camel case names

    # Invoke modules ----------------------
    usrInptDf <- callModule(impression, "impression",
                            idDf = reactive(idDf()),
                            usage_data = reactive(usage_data()))
    similarImgDf <- callModule(similarImg, "similarImg",
                               testImgId = reactive(input$mainImageId),
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
    usage_data <- reactive({
        cnames <- names(cdata)
        cvals <- lapply(cnames, function(name) {cdata[[name]]})
        cvals %>% set_names(cnames)
    })

    # Serve outputs -----------------------
    # Radiographs
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

    # Reactive Event Handlers --------------------------------------------------

   # UI toggle panel events
    shinyjs::onclick("toggleCnnPy",
        shinyjs::toggle("cnnPyUi", anim=TRUE))

    callModule(trace, "trace",
               radHover = reactive(input$radHover),
               radiologistIn = reactive(input$radiologist),
               mainImageIdIn = reactive(input$mainImageId),
               idDf = reactive(idDf()),
               usrInptDf = reactive(usrInptDf()),
               similarImgDf = reactive(similarImgDf()),
               usage_data = reactive(usage_data()))
}
