function(input, output, session) {
    # Reactive elements bound to lower camel case names

    # Invoke modules -----------------------------
    usrImpressionDf <- callModule(impression, "impression")
    callModule(similarImg, "similarImg",
        testImgId = reactive(SD()[["test_img_id"]]),
        img_dir = kDIR_SMALL_IMGS,
        dx_chr = kDXS_CHR)


    # Reactive Event Handlers --------------------------------------------------
    SD <- eventReactive(input$submitBtn, {
        if (!session$clientData[["output_cnnPyTbl_hidden"]] || input$submitBtn == 1) {  # User had cad available
            cat("\nnext case")
            # if CAD was availabe, go to next case

            df <- getNewCaseDf(test_imgs_df$img_id)
            df$is_cad_available <- (df$reader_mode[1] == "concurrent")

            if (df$is_cad_available) {shinyjs::show("cnnCadUi")
            } else {shinyjs::hide("cnnCadUi")}

            return(df)
        } else {
            cat("\nadd cad to current case")

            df <- data.frame(
                test_img_id = getLastCaseChr(input$user_name),
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
            add_column(username = input$user_name,
                       timestamp = date_time_stamp(), .before=1) %>%
            bind_cols(SD())
        save_usr_input(submit_data_df)
    })

    # Serve outputs --------------------------------

    # Test Radiograph
    output$mainImage <- renderImage({
        req(SD())
        filename <- stringr::str_interp("${kDIR_SMALL_IMGS}/${SD()[['test_img_id']]}.jpg")
        return(list(src = filename, filetype="image/jpeg", alt="Main Radiograph"))
    }, deleteFile = FALSE)

    # Test Radiograph with CNN BBox Localization
    output$bboxImage <- renderImage({
        req(SD())
        filename <- stringr::str_interp("${kDIR_BBOX_IMGS}/${SD()[['test_img_id']]}.jpg")
        return(list(src = filename, filetype="image/jpeg", alt="Bbox Radiograph"))
    }, deleteFile = FALSE)

    # CNN Classification pY Tbl
    output$cnnPyTbl <- renderTable({
        req(SD())
        test_py_df %>%
            df_filter_trans(img_id = SD()[['test_img_id']]) %>%
            set_colnames(c("diagnosis", "probability")) %>%
            arrange(desc(probability))
    })

    # Randomized Reader Mode Banner
    output$readerModeTxt <- renderText({
        req(SD())
        switch(SD()[["reader_mode"]],
               "second" = "Second Reader Mode: first, submit your unaided impression.  CNN utilities will then be provided and you can optionally modify answers.",
               "concurrent" = "Concurrent Reader Mode: feel free to use the CNN utilities below.")
    })


    # Trace -----------------------------------------
    # callModule(trace, "trace",
    #            user_nameIn = reactive(input$user_name),
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
