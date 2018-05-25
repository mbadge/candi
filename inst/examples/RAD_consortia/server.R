function(input, output, session) {
    # Image Upload and Display -------------------------------------------------
    filesInDf <- reactive({
        df <- input$filesInputDf %||% data.frame()
    })

    # Predicate reactive expression for conditional UI
    output$notUploaded <- reactive({
        is_empty(filesInDf())
    })
    output$isUploaded <- reactive({
        !is_empty(filesInDf())
    })
    outputOptions(output, "notUploaded", suspendWhenHidden = FALSE)
    outputOptions(output, "isUploaded", suspendWhenHidden = FALSE)

    img_id2fp_chr <- reactive({
        req(!is_empty(filesInDf()))
        df <- filesInDf()
        # 'deframe' user uploaded file df into LUT
        img_id2fp_lut <- df$datapath %>% purrr::set_names(df$name)
        names(img_id2fp_lut) %<>% stringr::str_sub(end = -5)  # Remove .jpg extension
        img_id2fp_lut
    })

    # Render UI drop-down to select which uploaded file to display
    output$img_select <- renderUI({
        req(!is_empty(filesInDf()))
        selectInput("img_id", "Image:", choices = names(img_id2fp_chr()))
    })

    # Radiograph
    output$radiographImage <- renderImage({
        req(!is_empty(filesInDf()))
        req(input$img_id)
        fp <- img_id2fp_chr()[input$img_id]
        return(list(src = fp, contentType = "image/jpeg", alt = "Please upload radiograph(s)"))
    }, deleteFile = FALSE)

    # Other outputs ------------------------------------------------------------
    # Brush coordinates
    output$coordinatesTable <- renderTable({bboxCoordinates()})

    # Downloads
    output$downloadClassification <- handle_annotation_download(ann_type = "classification", f_load = load_annotation)
    output$downloadSegmentation <- handle_annotation_download(ann_type = "segmentation", f_load = load_annotation)
    output$downloadClinicalNote <- handle_annotation_download(ann_type = "clinical_note", f_load = load_annotation)

    output$downloadSampleImages <- downloadHandler(
        filename = "sample_images.tar.gz",
        content = function(file) {
            file.copy(kFP_SAMPLE_IMGS, to=file)
        }
    )

    # Image/Annotation sources
    output$imgTmpFp <- renderPrint({
        req(!is_empty(filesInDf()))
        req(input$img_id)
        pth <- filesInDf() %>% filter(stringr::str_sub(name, end = -5) == input$img_id) %$% datapath
        cat(pth)
    })
    output$annotationURL <- renderPrint(cat(gsURL))

    # Reactive Conductors -----------------
    ids <- reactive({
        req(!is_empty(filesInDf()))
        x <- map(kID_FIELDS, function(x) x = input[[x]])
        x$timestamp <- Sys.time()
        x %<>% as.data.frame %>% purrr::set_names(c("Radiologist Name", "Image ID", "Timestamp"))
        x
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


    # Reactive Observers -------------------------------------------------------
    # I/O buttons
    observeEvent(
        eventExpr = input$submit_classification,
        handlerExpr = {
            save_annotation(data=classificationDF(), ann_type="classification")
        }
    )

    save_segmentation <- function(path) {
        stopifnot(path %in% kDXS_CHR)
        df <- segmentationDF() %>%
            tibble::add_column(Pathology=path)
        save_annotation(df, "segmentation")
    }
    observeEvent(
        eventExpr = input$submit_cardiomegaly,
        handlerExpr = {
            save_segmentation("cardiomegaly")
            shinyjs::disable(id = "submit_cardiomegaly")
        }
    )
    observeEvent(
        eventExpr = input$submit_emphysema,
        handlerExpr = {
            save_segmentation("emphysema")
            shinyjs::disable(id = "submit_emphysema")
        }
    )
    observeEvent(
        eventExpr = input$submit_effusion,
        handlerExpr = {
            save_segmentation("effusion")
            shinyjs::disable(id = "submit_effusion")
        }
    )

    observeEvent(input$submit_note,
                 save_annotation(clinicalNoteDF(), "clinical_note"))

    # Conditionally hide/disable segmentation submission
    observe(shinyjs::toggle("segmentation", anim=TRUE, condition=!is.null(input$pathologies)))
    observe(shinyjs::toggleState("submit_cardiomegaly",
                                 condition = nrow(bboxCoordinates()) > 0 && "cardiomegaly" %in% input$pathologies))
    observe(shinyjs::toggleState("submit_emphysema",
                                 condition = nrow(bboxCoordinates()) > 0 && "emphysema" %in% input$pathologies))
    observe(shinyjs::toggleState("submit_effusion",
                                 condition = nrow(bboxCoordinates()) > 0 && "effusion" %in% input$pathologies))
}
