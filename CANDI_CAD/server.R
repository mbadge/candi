function(input, output, session) {
    # Reactive elements bound to lower camel case names

    # Invoke modules -----------------------------
    usrImpressionDf <- callModule(impression, "impression")
    # callModule(similarImg, "similarImg",
    #     testImgId = reactive(SD()[["test_img_id"]]),
    #     test_pc_df = test_pc_df,
    #     hist_imgs_df = hist_imgs_df,
    #     hist_img_dir = kHIST_IMG_IN_DIR,
    #     dx_chr = kDXS_CHR)

    # Serve outputs --------------------------------
    # Test Radiograph
    output$mainImage <- renderImage({
        req(SD())
        filename <- stringr::str_interp("${kTEST_IMG_IN_DIR}/${SD()[['test_img_id']]}.jpg")
        return(list(src = filename, filetype="image/jpeg", alt="Main Radiograph"))
    }, deleteFile = FALSE)

    # CNN Test Image Predictions
    output$cnnPyTbl <- renderTable({
        req(SD())
        test_py_df %>%
            filter(img_id == SD()[['test_img_id']]) %>%
            select(-img_id) %>%
            gather(key=diagnosis, value=probability) %>%
            arrange(desc(probability))
    })

    output$readerModeTxt <- renderText({
        req(SD())
        SD()[["reader_mode"]]
    })

    # Reactive Event Handlers --------------------------------------------------
    SD <- eventReactive(input$submitBtn, {


        session_state_df <-
            data.frame(
                username = input$radiologist,
                # Select new test case
                #! TODO (random sample from kIMGS_AVAIL_CHR %ni% imgs_done)
                test_img_id = sample(x = stem(test_img_fns), size = 1),
                reader_mode = sample(x = c("concurrent", "second"), size = 1)
            ) %>%
            mutate(is_cad_available = map_lgl(reader_mode, ~ .x == "concurrent"))

        # Variably show CAD
        #shinyjs::toggle("cnnPyUi", anim=TRUE, condition = session_state_df$is_cad_available)

        # F -> show CAD; update is_cad_available to TRUE
        session_state_df
    })

    observe(
        shinyjs::toggle("cnnCadUi", anim=TRUE, condition = SD()[["is_cad_available"]])
    )



    # Save impression form everytime user clicks submit
    observeEvent(input$submitBtn, {
        # save annotated user input
        usr_input <- usrImpressionDf() %>%
            add_column(username = input$radiologist,
                       timestamp = date_time_stamp(), .before=1)
        save_usr_input(usr_input)
    })

    # UI toggle panel events
    shinyjs::onclick("toggleCnnPy",
        shinyjs::toggle("cnnPyUi", anim=TRUE))


    # Trace --------------------------------------------------------------------
    callModule(trace, "trace",
               radiologistIn = reactive(input$radiologist),
               usrImpressionDf = reactive(usrImpressionDf()),
               SD = reactive(SD()),
               usageLst = reactive({
                   cdata <- session$clientData
                   cnames <- names(cdata)
                   cvals <- lapply(cnames, function(name) {cdata[[name]]})
                   cvals %>% set_names(cnames)
               })
    )
}
