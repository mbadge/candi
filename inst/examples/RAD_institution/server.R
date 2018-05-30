function(input, output, session) {
    # ---- Conductors ----
    # Create x_chr of remaining test image ids
    remainingQueue <- eventReactive(
        eventExpr = input$submit_btn,
        label = "remainingQueue_eventRxtive",
        valueExpr = {
            # Scape user input to see what images user has already reviewed
            complete_input_ids <- candi::load_usr_input(input$user_name,
                                                        file.path(kDIR_USR_INPT, "classification")) %>%
                magrittr::use_series("img_id")
            # Get user-specific ordered queue
            usr_queue <- candi::randomize_user_queue(input$user_name, kAVAIL_TEST_IDS)
            # Get remaining portion of queue
            todo_input_ids <- usr_queue[usr_queue %ni% complete_input_ids]
            todo_input_ids
        }
    )

    ids <- reactive({
        id_fields <- c("user_name", "imgIdIn")
        x <- map(id_fields, function(x) x=input[[x]])
        x$timestamp <- lubridate::now()
        x %<>% as.data.frame() %>% purrr::set_names(c("user_name", "img_id", "timestamp"))
    })

    bboxCoordinates <- reactive({
        data.frame(input$bbox_brush[c("xmin", "xmax", "ymin", "ymax")]) %||% data.frame()
    })

    classificationDF <- reactive({
        df <- ids()
        df$Pathologies <- toString(input$pathologies)
        df
    })

    segmentationDF <- reactive({
        df <- ids()
        coords <- bboxCoordinates()
        bind_cols(df, coords)
    })

    clinicalNoteDF <- reactive({
        df <- ids()
        df$ClinicalNote <- input$note
        df
    })


    # ---- Observers ----
    observeEvent(
        eventExpr = input$submit_btn,
        label = "Initialize App",
        handlerExpr = {
            shinyjs::disable("user_name")
            updateSelectInput(session, "user_name", label = character(0))
            updateActionButton(session, "submit_btn", label = "Next Image")
            shinyjs::hide("init_help_panel")
            shinyjs::show("user_impression_panel")
            shinyjs::show("image_panel")
            log_usr_event(input$user_name, "start_btn", dir = kDIR_LOG)
        },
        priority = -1,
        once = TRUE  # Destroy this component after one intended execution
    )

    observeEvent(
        eventExpr = input$submit_classification,
        label = "Save Classificaiton Annotation",
        handlerExpr = {
          save_usr_input(classificationDF(), dir = kDIR_USR_INPT, subdir = "classification")
          log_usr_event(input$user_name, "submit_classification", dir = kDIR_LOG, img_id = input$imgIdIn)
          shinyjs::disable(id = "submit_classification")
          shinyjs::enable(id = "submit_btn")
        }
    )

    observeEvent(
        eventExpr = input$submit_cardiomegaly,
        label = "Save Cardiomegaly Segmentation",
        handlerExpr = {
          save_segmentation(segmentationDF(), "cardiomegaly")
          log_usr_event(input$user_name, "submit_segmentation", dir = kDIR_LOG, img_id = input$imgIdIn)
          shinyjs::disable(id = "submit_cardiomegaly")
        }
    )

    observeEvent(
        input$submit_emphysema,
        label = "Save Emphysema Segmentation",
        handlerExpr = {
          save_segmentation(segmentationDF(), "emphysema")
          log_usr_event(input$user_name, "submit_segmentation", dir = kDIR_LOG, img_id = input$imgIdIn)
          shinyjs::disable(id = "submit_emphysema")
        }
    )

    observeEvent(
        input$submit_effusion,
        label = "Save Effusion Segmentation",
        handlerExpr = {
          save_segmentation(segmentationDF(), "effusion")
          log_usr_event(input$user_name, "submit_segmentation", dir = kDIR_LOG, img_id = input$imgIdIn)
          shinyjs::disable(id = "submit_effusion")
        }
    )

    observeEvent(
        input$submit_note,
        label = "Save Note",
        handlerExpr = {
          save_usr_input(clinicalNoteDF(), dir = kDIR_USR_INPT, subdir = "clinical_note")
          log_usr_event(input$user_name, "submit_note", dir = kDIR_LOG, img_id = input$imgIdIn)
          shinyjs::disable(id = "submit_note")
        }
    )

    observeEvent(
        eventExpr = input$submit_btn,
        label = "Reset Entry Form",
        handlerExpr = {
            # Clear user entry forms
            updateCheckboxGroupInput(session, inputId = "pathologies", selected = character(0))
            updateTextAreaInput(session, "note", value=character(0))
            # enable non-segmentation submit buttons EXCEPT this "next image" button
            # (segmentation buttons deactive on click and reactivate on new image area draw)
            shinyjs::enable(id = "submit_classification")
            shinyjs::enable(id = "submit_note")
            shinyjs::disable(id = "submit_btn")
        }
    )

    # Conditionally hide/disable segmentation submission
    observe(shinyjs::toggle("segmentation", anim=TRUE, condition=!is.null(input$pathologies)))
    observe(shinyjs::toggleState("submit_cardiomegaly",
                                 condition = nrow(bboxCoordinates()) > 0 && "cardiomegaly" %in% input$pathologies))
    observe(shinyjs::toggleState("submit_emphysema",
                                 condition = nrow(bboxCoordinates()) > 0 && "emphysema" %in% input$pathologies))
    observe(shinyjs::toggleState("submit_effusion",
                                 condition = nrow(bboxCoordinates()) > 0 && "effusion" %in% input$pathologies))


    # ---- Outputs ----
    # Image UI based on completed image records.
    output$imgIdUi <- renderUI({
        todo <- remainingQueue()
        selectInput("imgIdIn", "Case Id:", choices = todo, selected = todo[1])
    })
    outputOptions(output, "imgIdUi", suspendWhenHidden = FALSE)

    # Radiograph
    output$radiographImage <- renderImage({
        filename <- stringr::str_interp("${kDIR_SMALL_IMGS}/${input$imgIdIn}.jpg")
        return(list(src = filename, filetype="image/jpeg", alt="Radiograph"))
    }, deleteFile = FALSE)

    # Brush Coordinates
    output$coordinatesTable <- renderTable({
        bboxCoordinates() %>%
            t()
    }, striped = TRUE, hover = TRUE, spacing = "xs",
    rownames = TRUE, colnames = FALSE, digits=0)

    # Image/Annotation Sources
    output$imgFp <- renderPrint(kDIR_SMALL_IMGS %>% cat())
    output$annFp <- renderPrint(kDIR_USR_INPT %>% cat())

    # Downloads
    output$downloadClassification <- handle_annotation_download("classification", f_load = load_annotation)
    output$downloadSegmentation <- handle_annotation_download("segmentation", f_load = load_annotation)
    output$downloadClinicalNote <- handle_annotation_download("clinical_note", f_load = load_annotation)

    # Progress Message
    output$progressText <- renderText({
        n_total <- length(kAVAIL_TEST_IDS)

        if (is.null(input$imgIdIn)) {  # Startup edge case
            return(glue::glue("{n_total} radiographs available"))
        } else {
            n_complete <- n_total - length(remainingQueue())
            return(glue::glue("Completed {n_complete} of {n_total} radiographs"))
        }
    })

    # Initial Usage Help/Instructions
    output$helpPrint <- renderPrint({cat(kHELP_TXT, sep="\n")})
}
