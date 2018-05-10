function(input, output, session) {
    # Invoke modules ----
    usrImpressionDf <- callModule(impression, "impression")
    callModule(similarImg, "similarImg",
        testImgId = reactive(input$imgIdIn),
        img_dir = kDIR_SMALL_IMGS,
        dx_chr = kDXS_CHR)

    # Image Progression ----
    # Get user-specific ordered queue
    usrQueue <- reactive(candi::randomize_user_queue(input$user_name, kAVAIL_IMG_IDS))

    # UI
    output$imgIdUi <- renderUI({
        selectInput("imgIdIn", "Image Id:", choices = usrQueue())
    })
    outputOptions(output, "imgIdUi", suspendWhenHidden=FALSE)

    # Reactive Event Handlers --------------------------------------------------
    readerMode <- eventReactive(input$submitBtn, {
        # Either initialize or save answer
        if (input$submitBtn == 1) {
            # Since we're timing user responses, gate the initiation of the app
            # Initially show nothing but name and begin button, then reveal impression and submit
            updateActionButton(session, "submitBtn", label = "Submit Impression")
            shinyjs::disable("user_name")
            shinyjs::show("mainImageUi")
            shinyjs::show("impressionPanel")

            is_cad_available <- TRUE  # Coerce cad flag to TRUE to get a new case loaded
        } else {
            # Infer CAD state from clientData
            is_cad_available <- !session$clientData[["output_cnnPyTbl_hidden"]]

            # save annotated user input
            submit_data_df <- usrImpressionDf() %>%
                add_column(is_cad_available = is_cad_available, .before=1) %>%
                add_column(img_id = input$imgIdIn, .before=1) %>%
                add_column(user_name = input$user_name, .before=1)
            save_usr_input(submit_data_df, dir=kDIR_USR_INPT)
        }

        # Load next interpretation (either new case or add cad)
        if (is_cad_available) {
            cat("\nnext case")
            # Clear entry forms
            updateCheckboxGroupInput(session=session, inputId = NS("impression", id="dxChkbxIn"), selected = character(0))
            updateTextAreaInput(session=session, inputId = NS("impression", id="noteTxtIn"), value="Clinical Impression")

            # Find next case in queue AFTER saving prior impression
            complete_input_ids <- candi::load_usr_input(input$user_name, kDIR_USR_INPT) %>%
                filter(is_cad_available == TRUE) %>%
                magrittr::use_series("img_id")

            # Get remaining portion of queue
            todo_input_ids <- usrQueue()[usrQueue() %ni% complete_input_ids]
            updateSelectInput(session, "imgIdIn", selected = todo_input_ids[1])

            # Randomize reader mode
            reader_mode <- sample(x = c("concurrent", "second"), size = 1)
            cat(glue::glue("reader_mode: {reader_mode}"))

            if (reader_mode == "concurrent") {
                shinyjs::show("cnnCadUi", anim=TRUE)
            } else {
                shinyjs::hide("cnnCadUi", anim = TRUE)
            }
            return(reader_mode)

        } else {  # if user did NOT have CAD available, they must have been doing second reader mode unaided
            shinyjs::show("cnnCadUi", anim=TRUE)
            return("second")
        }
    })

    # Auxillary State Info for user ----
    # Progress Message
    output$progressTxt <- renderText({
        readerMode()
        complete_input_ids <- candi::load_usr_input(input$user_name, kDIR_USR_INPT) %>%
            filter(is_cad_available == TRUE) %>%
            magrittr::use_series("img_id")
        glue::glue("Completed {length(complete_input_ids)} of {length(kAVAIL_IMG_IDS)} radiographs")
    })

    # Randomized Reader Mode Banner
    output$readerModeTxt <- renderText({
        switch(readerMode(),
               "second" = "Second Reader Mode: first, submit your unaided impression.  CNN utilities will then be provided and you can optionally modify answers.",
               "concurrent" = "Concurrent Reader Mode: feel free to use the CNN utilities below.")
    })


    # Serve outputs --------------------------------
    # Test Radiograph
    output$mainImage <- renderImage({
        req(input$imgIdIn)
        src_fp <- file.path(kDIR_SMALL_IMGS, str_c(input$imgIdIn, ".jpg"))
        return(list(src = src_fp, filetype="image/jpeg", alt="Main Radiograph"))
    }, deleteFile = FALSE)

    # CNN Toolkit ----
    # Test Radiograph with CNN BBox Localization
    output$bboxImage <- renderImage({
        src_fp <- file.path(kDIR_BBOX_IMGS, str_c(input$imgIdIn, ".jpg"))
        return(list(src = src_fp, filetype="image/jpeg", alt="Bbox Radiograph"))
    }, deleteFile = FALSE)

    # CNN Classification pY Tbl
    output$cnnPyTbl <- renderTable({
        test_py_df %>%
            df_filter_trans(img_id = input$imgIdIn) %>%
            set_colnames(c("diagnosis", "probability")) %>%
            arrange(desc(probability))
    })


    #Trace -----------------------------------------
    callModule(trace, "trace",
               user_nameIn = reactive(input$user_name),
               usrImpressionDf = reactive(usrImpressionDf()),
               usageLst = reactive({
                   cdata <- session$clientData
                   cnames <- names(cdata)
                   cvals <- lapply(cnames, function(name) {cdata[[name]]})
                   cvals %>% purrr::set_names(cnames)
               })
    )
}
