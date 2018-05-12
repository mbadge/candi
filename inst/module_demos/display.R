# if (interactive()) {
# library(shiny)
# 
# imgs_avail = candiOpt(large_img_dir) %>% list.files() %>% fp_stem()
# if (length(imgs_avail) == 0L) stop("no images found in ", candiOpt(large_img_dir), call. = FALSE)
# 
# shinyApp(
# ui = fluidPage(
#     selectInput("imgIdIn", "Image Id:", choices = imgs_avail),
#     verbatimTextOutput("selectedImgTxt"),
#     radiographOutput("show_rad")
# ),
# server = function(input, output, session) {
#     output$selectedImgTxt <- renderPrint(input$imgIdIn)
#     callModule(radiograph, "show_rad", imgIdIn = reactive(input$imgIdIn))
# })
# }
# 
# if (interactive()) {
# library(shiny)
# 
# imgs_avail = candiOpt(large_img_dir) %>% list.files() %>% fp_stem()
# if (length(imgs_avail) == 0L) stop("no images found in ", candiOpt(large_img_dir), call. = FALSE)
# 
# shinyApp(
# ui = fluidPage(
#     selectInput("imgIdIn", "Image Id:", choices = imgs_avail),
#     verbatimTextOutput("selectedImgTxt"),
#     radiographOutput("show_rad")
# ),
# server = function(input, output, session) {
#     output$selectedImgTxt <- renderPrint(input$imgIdIn)
#     callModule(radiograph, "show_rad", imgIdIn = reactive(input$imgIdIn))
# })
# }



library(EBImage)
x = readImage(system.file("images", "sample-color.png", package="EBImage"))[257:768,,]
EBImage:::validImage(x)
EBImage:::interactiveMode()

display(x)
# display(x, method = "browser")
# display(x, method = "raster")
