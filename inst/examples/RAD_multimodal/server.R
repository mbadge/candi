function(input, output, session) {
    # ---- Modules ----
    # Input
    usrImpressionDf <- callModule(impression, "user_impression",
                                  include_demographics = kINCLUDE_DEMOGRAPHICS,
                                  include_technical = kINCLUDE_TECHNICAL)

    # Output
    callModule(patientMedicalRecord, "display_emr",
               idIn = reactive(input$imgIdIn))
    callModule(radiograph, "main_image", imgIdIn = reactive(input$imgIdIn))


    # ---- Conductors ----
    # Create x_chr of remaining test image ids
    remainingQueue <- eventReactive(
        eventExpr = input$submit_btn,
        label = "remainingQueue_eventRxtive",
        valueExpr = {
            # Scape user input to see what images user has already reviewed
            complete_input_ids <- candi::load_usr_input(input$user_name, kDIR_USR_INPT) %>%
                magrittr::use_series("img_id")
            # Get user-specific ordered queue
            usr_queue <- candi::randomize_user_queue(input$user_name, kAVAIL_IMG_IDS)
            # Get remaining portion of queue
            todo_input_ids <- usr_queue[usr_queue %ni% complete_input_ids]
        }
    )


    # ---- Observers ----
    # Inialize App
    observeEvent(
        eventExpr = input$submit_btn,
        label = "startup_obsEvnt",
        handlerExpr = {
            updateActionButton(session, "submit_btn", label = "Submit Annotation")
            shinyjs::disable("user_name")
            shinyjs::show("user_impression_panel")
        },
        priority = -1,
        once = TRUE  # Destroy this component after one intended execution
    )

    # Save impression form
    observeEvent(
        eventExpr = input$submit_btn,
        label = "Save User Data",
        handlerExpr = {
            req(input$imgIdIn)

            # Record user data
            submit_data_df <- usrImpressionDf() %>%
                tibble::add_column(img_id = input$imgIdIn, .before=1) %>%
                tibble::add_column(user_name = input$user_name, .before=1)
            save_usr_input(submit_data_df, dir = kDIR_USR_INPT)
            log_usr_event(input$user_name, "submit_btn", dir = kDIR_LOG, img_id = input$imgIdIn)
            cat("data saved")
        },
        priority = 10  # Execute before remainingQueue and all others so they act on the next img
    )

    # Prefill impression values for next case for use convenience
    observeEvent(
        eventExpr = input$submit_btn,
        label = "Prefill values on record for next case",
        handlerExpr = {
            next_imgId <- remainingQueue()[1L]

            dx_chr <- cases[c("case", kDXS_CHR)] %>%
                df_filter_trans(., case = imgIds2Cases(next_imgId)) %>%
                dplyr::filter(value) %>%
                magrittr::use_series("column")

            pt_df <- cases[c("case", kEMR_DEMOGRAPHICS)] %>%
                dplyr::filter(case == imgIds2Cases(next_imgId))

            # Clear radiobuttons if the input is missing
            if_na_clear <- function(x) ifelse(is.na(x), character(0), x)

            updateCheckboxGroupInput(session, NS("user_impression", "dxChkbxIn"),
            	selected = dx_chr)
            updateSliderInput(session, NS("user_impression", "ageIn"), value = pt_df$age)
            updateRadioButtons(session, NS("user_impression", "sexIn"), selected = pt_df$sex)
            updateTextAreaInput(session, NS("user_impression", "noteTxtIn"), value=character(0))
        }
    )


    # ---- Outputs ----
    # Image UI based on completed image records.
    output$imgIdUi <- renderUI({
        todo <- remainingQueue()
        selectInput("imgIdIn", "Image Id:", choices = todo, selected = todo[1])
    })
    outputOptions(output, "imgIdUi", suspendWhenHidden = FALSE)

    # Progress Message
    output$progressText <- renderText({
        n_total <- length(kAVAIL_IMG_IDS)
        n_complete <- n_total - length(remainingQueue())
        glue::glue("Completed {n_complete} of {n_total} radiographs")
    })
}
