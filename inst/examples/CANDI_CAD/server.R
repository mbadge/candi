function(input, output, session) {
    # ---- Modules ----
    # Input
    usrImpressionDf <- callModule(impression, "user_impression")

    # Output
    callModule(radiograph, "main_image", imgIdIn = reactive(input$imgIdIn))
    callModule(similarImg, "similarImg",
        testImgId = reactive(input$imgIdIn),
        test_imgs_df = test_img_df,
        hist_imgs_df = hist_img_df)


    # ---- Conductors ----
    # Manage image progression and reader mode state used for multiple outputs

    # Queue management
    usrQueue <- eventReactive(
        eventExpr = input$submit_btn,
        label = "User Queue Randomization",
        valueExpr = {
            candi::randomize_user_queue(input$user_name, kAVAIL_TEST_IDS)
        }
    )

    # Remaining test image ids
    remainingQueue <- eventReactive(
        eventExpr = input$submit_btn,
        label = "Remaining Queue eventRxtive",
        valueExpr = {
            # Scape user input to see what images user has already reviewed
            usr_input_df <- candi::load_usr_input(input$user_name, kDIR_USR_INPT)

            if (is.null(usr_input_df)) {
              complete_input_ids <- character(0)
            } else {
                complete_input_ids <- usr_input_df %>%
                    filter(is_cad_available == TRUE) %>%
                    magrittr::use_series("img_id")
            }

            # Get remaining portion of queue
            todo_input_ids <- usrQueue()[usrQueue() %ni% complete_input_ids]
            todo_input_ids
        }
    )

    # CAD state progression and randomization
    readerMode <- eventReactive(input$submit_btn, {
        # Either initialize or save answer
        if (input$submit_btn == 1) {
            # Since we're timing user responses, gate the initiation of the app
            # Initially show nothing but name and begin button, then reveal impression and submit
            updateActionButton(session, "submit_btn", label = "Submit Impression")
            shinyjs::disable("user_name")
            shinyjs::show("main_img_panel")
            shinyjs::show("user_impression_panel")
            log_usr_event(input$user_name, "start btn", dir = kDIR_LOG)

            is_cad_available <- TRUE  # Coerce cad flag to TRUE to get a new case loaded
        } else {
            # Infer CAD state from clientData
            is_cad_available <- !session$clientData[["output_cnnPyTbl_hidden"]]
        }

        # Load next interpretation (either new case or add cad)
        if (is_cad_available) {
            cat("\nnext case\n")
            # Clear entry forms
            updateCheckboxGroupInput(session=session, inputId = NS("user_impression", id="dxChkbxIn"), selected = character(0))
            updateTextAreaInput(session=session, inputId = NS("user_impression", id="noteTxtIn"), value="Clinical Impression")

            # Randomize reader mode
            set.seed(as.integer(Sys.time()))  # https://stackoverflow.com/questions/32133816/r-shiny-not-randomizing
            reader_mode <- sample(x = c("concurrent", "second"), size = 1)
            cat(glue::glue("reader_mode: {reader_mode}"))

            if (reader_mode == "concurrent") {
                shinyjs::show("cnnCadUi", anim=TRUE)
            } else {
                shinyjs::hide("cnnCadUi", anim = TRUE)
            }
        } else {  # if user did NOT have CAD available, they must have been doing second reader mode unaided
            reader_mode = "second"
            shinyjs::show("cnnCadUi", anim=TRUE)
        }
        reader_mode
    })


    # ---- Observers ----
    observeEvent(
        eventExpr = input$submit_btn,
        label = "Save User Data",
        handlerExpr = {
            req(input$imgIdIn)
            is_cad_available <- !session$clientData[["output_cnnPyTbl_hidden"]]  # Infer CAD state from clientData

            submit_data_df <- usrImpressionDf() %>%
                tibble::add_column(is_cad_available = is_cad_available, .before=1) %>%
                tibble::add_column(img_id = input$imgIdIn, .before=1) %>%
                tibble::add_column(user_name = input$user_name, .before=1)
            save_usr_input(submit_data_df, dir=kDIR_USR_INPT)
            log_usr_event(input$user_name, "submit btn", dir = kDIR_LOG, img_id = input$imgIdIn)

            cat("data saved")
        },
        priority = 10  # Execute before remainingQueue and all others so they act on the next img
    )


    # ---- Outputs ----
    # image selection UI
    output$imgIdUi <- renderUI({
        todo_input_ids <- remainingQueue()
        cat("\n", todo_input_ids, "\n")

        selectInput("imgIdIn", "Image Id:", choices = todo_input_ids, selected = todo_input_ids[1])
    })
    outputOptions(output, "imgIdUi", suspendWhenHidden=FALSE)

    # State Info
    # Progress Message
    output$progressText <- renderText({
        n_total <- length(kAVAIL_TEST_IDS)

        if (is.null(input$imgIdIn)) {
            return(glue::glue("{n_total} radiographs available"))
        } else {
            n_complete <- n_total - length(remainingQueue())
            return(glue::glue("Completed {n_complete} of {n_total} radiographs"))
        }
    })

    # Randomized Reader Mode Banner
    output$readerModeText <- renderText({
        switch(readerMode(),
               "second" = "Second Reader Mode: first, submit your unaided impression.  CNN utilities will then be provided and you can optionally modify answers.",
               "concurrent" = "Concurrent Reader Mode: feel free to use the CNN utilities below.")
    })

    # CNN Toolkit
    # Test Radiograph with CNN BBox Localization
    output$bboxImage <- renderImage({
        req(input$imgIdIn)
        src_fp <- file.path(kDIR_BBOX_IMGS, stringr::str_c(input$imgIdIn, ".jpg"))
        return(list(src = src_fp, filetype="image/jpeg", alt="Bbox Radiograph"))
    }, deleteFile = FALSE)

    # CNN Classification pY Tbl
    output$cnnPyTbl <- renderTable({
        req(input$imgIdIn)
        pY_df <- test_img_df %>%
            select(img_id, starts_with("pY"))
        names(pY_df) <- str_replace(names(pY_df), "^pY_", "")
        pY_df %>%
            df_filter_trans(img_id = input$imgIdIn) %>%
            set_colnames(c("diagnosis", "probability")) %>%
            arrange(desc(probability))
    }, hover = TRUE, spacing = "xs")


    # ---- Trace ----
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
