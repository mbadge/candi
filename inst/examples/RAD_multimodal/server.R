function(input, output, session) {
    # Invoke Modules ----
    callModule(patientMedicalRecord, "displayEMR",
               idIn = reactive(input$imgIdIn))

    # User Input / Conductors ----
    # Reactive elements bound to lower camel case names
    usrInptDf <- reactive({
        data.frame(
            user_name = input$userNameIn,
            img_id = input$imgIdIn,
            dxs = input$dxChkbxIn %>% paste(collapse = ","),
            age = input$ageIn,
            sex = input$sexIn,
            view = input$viewIn,
            cassette = input$cassetteIn,
            note = input$txtIn
        )
    })

    # Main image
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
        candi::save_usr_input(usrInptDf(), dir = kDIR_USR_INPT)

        # Tee up next radiograph
        complete_input_ids <- candi::load_usr_input(input$userNameIn, kDIR_USR_INPT) %>%
            magrittr::use_series("img_id")
        todo_input_ids <- setdiff(kAVAIL_IMG_IDS, complete_input_ids)

        next_imgId <- todo_input_ids[[1]]

        updateSelectInput(session = session, inputId = "imgIdIn", selected = next_imgId)

        # Prefill values
        dx_chr <- test_df[c("img_id", kDXS_CHR)] %>%
            df_filter_trans(., img_id = next_imgId) %>%
            dplyr::filter(value) %>%
            magrittr::use_series("column")

        pt_df <- test_df[c("img_id", kEMR_DEMOGRAPHICS)] %>%
            dplyr::filter(img_id == next_imgId)

        # Clear radiobuttons if the input is missing
        if_na_clear <- function(x) ifelse(is.na(x), character(0), x)

        updateCheckboxGroupInput(session, "dxChkbxIn", selected = dx_chr)
        updateSliderInput(session, "ageIn", value = pt_df$age)
        updateRadioButtons(session, "sexIn", selected = pt_df$sex)
        updateRadioButtons(session, "viewIn", selected = if_na_clear(pt_df$view))
        updateRadioButtons(session, "cassetteIn", selected = if_na_clear(pt_df$cassette_orientation))
        # Reset
        updateTextInput(session, "txtIn", value=character(0))
    })
}
