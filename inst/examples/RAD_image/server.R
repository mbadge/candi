function(input, output, session) {
    # Invoke Modules ----
    usrImpressionDf <- callModule(impression, "user_impression",
                                  include_demographics = kINCLUDE_DEMOGRAPHICS,
                                  include_technical = kINCLUDE_TECHNICAL)

    # Main image
    callModule(radiograph, "main_image", imgIdIn = reactive(input$imgIdIn))

    # UI
    output$imgIdUi <- renderUI({
        input$submit_btn
        # Tee up next radiograph ---
        # Scape user input to see what images user has already reviewed
        complete_input_ids <- candi::load_usr_input(input$user_name, kDIR_USR_INPT) %>%
            magrittr::use_series("img_id")
        # Get user-specific ordered queue
        usr_queue <- candi::randomize_user_queue(input$user_name, kAVAIL_IMG_IDS)
        # Get remaining portion of queue
        todo_input_ids <- usr_queue[usr_queue %ni% complete_input_ids]

        selectInput("imgIdIn", "Image Id:", choices = todo_input_ids)
    })
    outputOptions(output, "imgIdUi", suspendWhenHidden = FALSE)

    # Save impression form everytime user clicks submit
    #observeEvent(input$submit_btn, {
    n_complete <- eventReactive(input$submit_btn, {
        # Record user data
        submit_data_df <- usrImpressionDf() %>%
            tibble::add_column(img_id = input$imgIdIn, .before=1) %>%
            tibble::add_column(user_name = input$user_name, .before=1)
        save_usr_input(submit_data_df, dir = kDIR_USR_INPT)
        log_usr_event(input$user_name, "submit_btn", dir = kDIR_LOG, img_id = input$imgIdIn)

        # Tee up next radiograph
        complete_input_ids <- candi::load_usr_input(input$user_name, kDIR_USR_INPT) %>%
            magrittr::use_series("img_id")
        todo_input_ids <- setdiff(kAVAIL_IMG_IDS, complete_input_ids)

        next_imgId <- todo_input_ids[[1]]

        updateSelectInput(session = session, inputId = "imgIdIn", selected = next_imgId)

        # Clear user entry forms
        updateCheckboxGroupInput(session, inputId = NS(namespace = "user_impression", id = "dxChkbxIn"), selected = character(0))
        updateSliderInput(session, inputId = NS(namespace = "user_impression", id = "ageIn"), value = 50)
        updateRadioButtons(session, NS("user_impression", "sexIn"), selected = character(0))
        updateRadioButtons(session, NS("user_impression", "viewIn"), selected = character(0))
        updateRadioButtons(session, NS("user_impression", "cassetteIn"), selected = character(0))
        updateTextAreaInput(session, NS("user_impression", "noteTxtIn"), value=character(0))

        # Return n complete images
        complete_input_ids <- candi::load_usr_input(input$user_name, kDIR_USR_INPT) %>%
            magrittr::use_series("img_id")
        # This runs before a file is saved, so I hacked a +1 to the count (still starts at 0, but doesn't lag)
        glue::glue("Completed {length(complete_input_ids)} of {length(kAVAIL_IMG_IDS)} radiographs")
    })

    output$progressTxt <- renderText({
        input$user_name
        n_complete()
    })
}
