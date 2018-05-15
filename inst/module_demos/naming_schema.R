# Reactives ----
# Reactive sources: snake_case
# Reactive expressions: lowerCamelCase
# Reactive observers: lowerCamelCase

library(shiny)
shinyApp(
    ui = fluidPage(
        textInput("user_msg", "Enter Message:"),
        textOutput("msgText")
    ),
    server = function(input, output, session) {
        output$msgText <- renderText(input$user_msg)
    }
)


# Modules ----
# ui fxn: lowerCamelCase ending w/ Input/Output/UI
# server fxn: lowerCamelCase ending w/ Module
# namespace id variable: snake case
#  * ui.R: <ui_module_fxn>("namespace_id")
#  * server.R: callModule(<server_module_fxn, "namespace_id")
usrMsgOutput <- function(id) {
    ns <- NS(id)
    textOutput(ns("msgText"))
}
usrMsgModule <- function(input, output, session, usrMsg) {
    output$msgText <- renderText(expr = usrMsg())
}
shinyApp(
    ui = fluidPage(
        textInput("user_msg", "Enter Message:"),
        usrMsgOutput("user_msg")
    ),
    server = function(input, output, session) {
        callModule(usrMsgModule, "user_msg", usrMsg = reactive(input$user_msg))
    }
)




generateCode_module <- function(module_id) {
    # module id should equate to the key arg passed into the module invocations
    stopifnot(is.character(module_id))

    ui_fxn_nm <- str_c(str_case_camel(module_id), "Output")
    server_fxn_nm <- str_c(str_case_camel(module_id), "Module")

    ui <- bquote(
        .(ui_fxn_nm) <- function(id) {
            ns <- NS(id)
            textOutput(ns( .(module_id) ))
        }
    )

    server <- bquote(
        .(server_fxn_nm) <- function(input, output, session) {
            output[[ .(module_id) ]] <- renderText(expr = .(str_case_camel(module_id)) () )
        }
    )

    list(ui = ui, server = server)
}
generateCode_module("user_msg")


"userMsgOutput" <- function(id) {
    ns <- NS(id)
    textOutput(ns(msgText))
}
