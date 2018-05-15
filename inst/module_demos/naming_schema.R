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
usrMsgOutput <- function(id, msg) {
    ns <- NS(id)
    textOutput(ns("msgText"))
}
usrMsgModule <- function(input, output, session, userMsg) {
    output$msgText <- renderText(expr = userMsg())
}
shinyApp(
    ui = fluidPage(
        textInput("user_msg", "Enter Message:"),
        usrMsgOutput("user_msg")
    ),
    server = function(input, output, session) {
        callModule(usrMsgModule, "user_msg", userMsg = reactive(input$user_msg))
    }
)
