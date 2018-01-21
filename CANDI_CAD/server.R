function(input, output, session) {
    # Reactive elements bound to lower camel case names

    # Invoke modules -----------------------------
    usrImpressionDf <- callModule(impression, "impression")
    callModule(similarImg, "similarImg",
        testImgId = reactive(SD()[["test_img_id"]]),
        test_pc_df = test_pc_df,
        hist_imgs_df = hist_imgs_df,
        hist_img_dir = kHIST_IMG_IN_DIR,
        dx_chr = kDXS_CHR)


    # Reactive Event Handlers --------------------------------------------------
    SD <- eventReactive(input$submitBtn, {
        if (!session$clientData[["output_cnnPyTbl_hidden"]] || input$submitBtn == 1) {  # User had cad available
            cat("\nnext case")
            # if CAD was availabe, go to next case

            df <- getNewCaseDf()
            df$is_cad_available <- (df$reader_mode[1] == "concurrent")

            if (df$is_cad_available) {shinyjs::show("cnnCadUi")
            } else {shinyjs::hide("cnnCadUi")}

            return(df)
        } else {
            cat("\nadd cad to current case")

            df <- data.frame(
                test_img_id = getLastCaseChr(input$radiologist),
                reader_mode = "second",
                is_cad_available = TRUE
            )

            shinyjs::show("cnnCadUi", anim=TRUE)
            return(df)
        }
    })


    # Save impression form everytime user clicks submit
    observeEvent(input$submitBtn, {
        if (input$submitBtn == 1) {shinyjs::show("impressionPanel"); invisible(return(NULL))}

        # save annotated user input
        submit_data_df <- usrImpressionDf() %>%
            add_column(username = input$radiologist,
                       timestamp = date_time_stamp(), .before=1) %>%
            bind_cols(SD())
        save_usr_input(submit_data_df)
    })

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
        switch(SD()[["reader_mode"]],
               "second" = "Second Reader Mode: first, submit your unaided impression.  CNN utilities will then be provided and you can optionally modify answers.",
               "concurrent" = "Concurrent Reader Mode: feel free to use the CNN utilities below.")
    })


    # Trace -----------------------------------------
    # callModule(trace, "trace",
    #            radiologistIn = reactive(input$radiologist),
    #            usrImpressionDf = reactive(usrImpressionDf()),
    #            SD = reactive(SD()),
    #            usageLst = reactive({
    #                cdata <- session$clientData
    #                cnames <- names(cdata)
    #                cvals <- lapply(cnames, function(name) {cdata[[name]]})
    #                cvals %>% set_names(cnames)
    #            })
    #)
}
