function(input, output, session) {
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
    #observeEvent(input$submitBtn, {
    n_complete <- eventReactive(input$submitBtn, {
        candi::save_usr_input(usrInptDf(), dir = kDIR_USR_INPT)
        cxrTargetDiff::log_usr_event(input$userNameIn, "submitBtn", dir = kDIR_LOG, img_id = input$imgIdIn)

        # Tee up next radiograph
        complete_input_ids <- candi::load_usr_input(input$userNameIn, kDIR_USR_INPT) %>%
            magrittr::use_series("img_id")
        todo_input_ids <- setdiff(kAVAIL_IMG_IDS, complete_input_ids)

        next_imgId <- todo_input_ids[[1]]

        updateSelectInput(session = session, inputId = "imgIdIn", selected = next_imgId)

        # Clear user entry forms
        updateCheckboxGroupInput(session, "dxChkbxIn", selected = character(0))
        updateSliderInput(session, "ageIn", value = 50)
        updateRadioButtons(session, "sexIn", selected = character(0))
        updateRadioButtons(session, "viewIn", selected = character(0))
        updateRadioButtons(session, "cassetteIn", selected = character(0))
        updateTextInput(session, "txtIn", value=character(0))

        # Return n complete images
        complete_input_ids <- candi::load_usr_input(input$userNameIn, kDIR_USR_INPT) %>%
            magrittr::use_series("img_id")
        # This runs before a file is saved, so I hacked a +1 to the count (still starts at 0, but doesn't lag)
        glue::glue("Completed {length(complete_input_ids)} of {length(kAVAIL_IMG_IDS)} radiographs")
    })

    output$progressTxt <- renderText({
        input$userNameIn
        n_complete()
    })
}
