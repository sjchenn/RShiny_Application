#' @title Run Shiny App
#
#' @description This function run UI part and server part for our shiny app
#'
#' @import shiny
#'
#' @export
run_shiny = function(){
  shiny::shinyApp(
    ui = UI_function(),
    server = Server
  )
}
