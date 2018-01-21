function(input, output, session) {
    # Reactive elements bound to lower camel case names

    # Invoke modules -----------------------------
    usrImpressionDf <- callModule(impression, "impression")
    # callModule(similarImg, "similarImg",
    #     testImgId = reactive(SD()[["test_img_id"]]),
    #     test_pc_df = test_pc_df,
    #     hist_imgs_df = hist_imgs_df,
    #     hist_img_dir = kHIST_IMG_IN_DIR,
    #     dx_chr = kDXS_CHR)


    # Reactive Event Handlers --------------------------------------------------
    SD <- eventReactive(input$submitBtn, {
        if (!session$clientData[["output_cnnPyTbl_hidden"]]) {
            cat("\nnext case")
            # if CAD was availabe, go to next case
            df <- getNewCaseDf()
            df$is_cad_available <- (df$reader_mode[1] == "concurrent")
            if (df$is_cad_available) {show("cnnCadUi")
            } else {hide("cnnCadUi")}
#            toggle("cnnCadUi", anim = TRUE, condition = df$is_cad_available)
            return(df)
        } else {
            cat("\nadd cad to current case")
            df <- getLastCaseDf()
            df$is_cad_available <- TRUE
            show("cnnCadUi", anim=TRUE)
            df$reader_mode <- "second"
            return(df)
        }
    })

    getNewCaseDf <- function() {
        data.frame(
            test_img_id = sample(x = stem(test_img_fns), size = 1),
            reader_mode = sample(x = c("concurrent", "second"), size = 1)
        )
    }
    getLastCaseDf <- function() {
        data.frame(
            test_img_id = "iu_25_1"
        )
    }

    # observe({
    #     shinyjs::toggle("cnnCadUi", anim=TRUE, condition = SD()[["is_cad_available"]])
    # })


    # Save impression form everytime user clicks submit
    observeEvent(input$submitBtn, {
        # save annotated user input
        usr_input <- usrImpressionDf() %>%
            add_column(username = input$radiologist,
                       timestamp = date_time_stamp(), .before=1)
        save_usr_input(usr_input)
    })

    # Serve outputs --------------------------------
    # Test Radiograph
    output$mainImage <- renderImage({
        req(SD())
        filename <- stringr::str_interp("${kTEST_IMG_IN_DIR}/${SD()[['test_img_id']]}.jpg")
        return(list(src = filename, filetype="image/jpeg", alt="Main Radiograph"))
    }, deleteFile = FALSE)

    # CNN Test Image Predictions
    output$cnnPyTbl <- renderTable({
        req(SD())
        test_py_df %>%
            filter(img_id == SD()[['test_img_id']]) %>%
            select(-img_id) %>%
            gather(key=diagnosis, value=probability) %>%
            arrange(desc(probability))
    })

    output$readerModeTxt <- renderText({
        req(SD())
        SD()[["reader_mode"]]
    })


    # Trace -----------------------------------------
    callModule(trace, "trace",
               radiologistIn = reactive(input$radiologist),
               usrImpressionDf = reactive(usrImpressionDf()),
               SD = reactive(SD()),
               usageLst = reactive({
                   cdata <- session$clientData
                   cnames <- names(cdata)
                   cvals <- lapply(cnames, function(name) {cdata[[name]]})
                   cvals %>% set_names(cnames)
               })
    )
}


# is_cad_available <- function() {
#     !session$clientData[["output_cnnPyTbl_hidden"]]
# }
