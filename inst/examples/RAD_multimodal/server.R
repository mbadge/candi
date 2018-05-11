# Reactive elements bound to lower camel case names

function(input, output, session) {
    # Invoke Modules ----
    usrImpressionDf <- callModule(impression, "usrImpression",
               include_demographics = kINCLUDE_DEMOGRAPHICS,
               include_technical = kINCLUDE_TECHNICAL)

    callModule(patientMedicalRecord, "displayEMR",
               idIn = reactive(input$imgIdIn))

    callModule(radiograph, "main_image", imgIdIn = reactive(input$imgIdIn))

    # Progress Message
    output$progressTxt <- renderText({
        input$submitBtn
        complete_input_ids <- candi::load_usr_input(input$userNameIn, kDIR_USR_INPT) %>%
            magrittr::use_series("img_id")
        # This runs before a file is saved, so I hacked a +1 to the count (still starts at 0, but doesn't lag)
        glue::glue("Completed {length(complete_input_ids)} of {length(kAVAIL_IMG_IDS)} radiographs")
    })

    # UI
    output$imgIdUi <- renderUI({
        input$submitBtn
        # Tee up next radiograph ---
        # Scape user input to see what images user has already reviewed
        complete_input_ids <- candi::load_usr_input(input$userNameIn, kDIR_USR_INPT) %>%
            magrittr::use_series("img_id")
        # Get user-specific ordered queue
        usr_queue <- candi::randomize_user_queue(input$userNameIn, kAVAIL_IMG_IDS)
        # Get remaining portion of queue
        todo_input_ids <- usr_queue[usr_queue %ni% complete_input_ids]

        selectInput("imgIdIn", "Image Id:", choices = todo_input_ids)
    })
    outputOptions(output, "imgIdUi", suspendWhenHidden = FALSE)


    # Save impression form everytime user clicks submit
    observeEvent(input$submitBtn, {
        cat("saving data...")
        # Record user data
        submit_data_df <- usrImpressionDf() %>%
            tibble::add_column(img_id = input$imgIdIn, .before=1) %>%
            tibble::add_column(user_name = input$userNameIn, .before=1)
        save_usr_input(submit_data_df, dir = kDIR_USR_INPT)
        cat("data saved")

        # Tee up next radiograph
        complete_input_ids <- candi::load_usr_input(input$userNameIn, kDIR_USR_INPT) %>%
            magrittr::use_series("img_id")
        todo_input_ids <- setdiff(kAVAIL_IMG_IDS, complete_input_ids)

        next_imgId <- todo_input_ids[[1]]

        cat('prefilling values')
        updateSelectInput(session = session, inputId = "imgIdIn", selected = next_imgId)

        # Prefill values
        dx_chr <- cases[c("case", kDXS_CHR)] %>%
            df_filter_trans(., case = imgIds2Cases(next_imgId)) %>%
            dplyr::filter(value) %>%
            magrittr::use_series("column")

        pt_df <- cases[c("case", kEMR_DEMOGRAPHICS)] %>%
            dplyr::filter(case == imgIds2Cases(next_imgId))

        # Clear radiobuttons if the input is missing
        if_na_clear <- function(x) ifelse(is.na(x), character(0), x)

        updateCheckboxGroupInput(session, NS("usrImpression", "dxChkbxIn"),
        	selected = dx_chr)
        updateSliderInput(session, NS("usrImpression", "ageIn"), value = pt_df$age)
        updateRadioButtons(session, NS("usrImpression", "sexIn"), selected = pt_df$sex)
        updateTextAreaInput(session, NS("usrImpression", "noteTxtIn"), value=character(0))
    })
}
