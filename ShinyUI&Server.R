#' @title Shiny UI
#
#' @description This function provides UI part of our Shiny App
#'
#' @import shiny
#' @import ShinyRatingInput
#' @importFrom shinyjs useShinyjs extendShinyjs
#' @import ECharts2Shiny
UI_function = function(){
  #https://stackoverflow.com/questions/39573258/reset-ratinginput-in-shiny-app
  jsCode <-"shinyjs.reset_1 = function(params){$('.rating-symbol-foreground').css('width', params);}"

  fluidPage(
    setBackgroundColor(color = "f3f3f3"),
    titlePanel(h3("Calculate Your Rate For Entering UCLA!",
                  style = "color:orange;font-size:30px")),

    sidebarLayout(
      sidebarPanel(
        h4("Standard Scores:"),
        sliderInput("GRE","Your Current GRE Score is:",min=260,max=340,value=320),
        sliderInput("TOEFL","Your  Current TOEFL Score is:",min=1,max=120,value=100),
        sliderInput("GPA","Your Current GPA is:",min=0.0,max=10.0,value=8.0,step=0.1),
        h4("Background:"),
        bootstrapPage(
          useShinyjs(),
          extendShinyjs(text = jsCode, functions = c("winprint")),
          ratingInput("CollegeR", label="Rate Your Undergraduate University", dataStop=5),
          htmlOutput("CollegeRout"),
          ratingInput("SOP",label="Rate Your Strength of Statement of Purpose", dataStop=5),
          htmlOutput("SOPout"),
          ratingInput("LOR",label="Rate Your Strength of Letter of Recommendation", dataStop=5),
          htmlOutput("LORout"),
          checkboxInput("research","Do you have reserach experience?",FALSE),
          actionButton("resultbutton","Let's See!")),
      ),

      mainPanel(htmlOutput("text"),style="color:orange;font-size:25px",
                uiOutput("image"),
                loadEChartsLibrary(),
                tags$div(id="comparison", style="width:80%;height:500px;"),
                deliverChart(div_id = "comparison"))
    )
  )
}


#' @title Shiny Server
#
#' @description This function provides server part of our Shiny App
#'
#' @param input shiny inputs
#'
#' @param output shiny output
#'
#' @param session shiny session
#'
#' @import shiny
Server = function(input, output, session) {
    inputdf = reactive({
      req(input$GRE, input$TOEFL, input$CollegeR, input$SOP, input$LOR, input$GPA)
      personaldata = as.data.frame(t(c(as.integer(input$GRE),as.integer(input$TOEFL),
                                       as.integer(input$CollegeR),
                                       as.integer(input$SOP),
                                       as.integer(input$LOR),as.integer(input$GPA),as.integer(input$research))))
      colnames(personaldata) = c('GRE.Score','TOEFL.Score','University.Rating','SOP',
                                 'LOR','CGPA','Research')
      return(personaldata)
    })


    rate = eventReactive(input$resultbutton,{
      result = round(as.numeric(
        predict_skr(model_comparison()$best_model,inputdf(),
                    model_comparison()$best_method)),digits=2)
      return(result)
    })

    #output RadarChart: how is this specific person doing compared to people whose prob of admission is above 70%
    observeEvent(input$resultbutton, {
      range.gre = quantile(gradApplication[gradApplication[,"Chance.of.Admit"] > 0.7,"GRE.Score"], c(0,1,0.5))
      range.tof = quantile(gradApplication[gradApplication[,"Chance.of.Admit"] > 0.7,"TOEFL.Score"], c(0,1,0.5))
      range.urt = quantile(gradApplication[gradApplication[,"Chance.of.Admit"] > 0.7,"University.Rating"], c(0,1,0.5))
      range.sop = quantile(gradApplication[gradApplication[,"Chance.of.Admit"] > 0.7,"SOP"], c(0,1,0.5))
      range.lor = quantile(gradApplication[gradApplication[,"Chance.of.Admit"] > 0.7,"LOR"], c(0,1,0.5))
      range.gpa = quantile(gradApplication[gradApplication[,"Chance.of.Admit"] > 0.7,"CGPA"], c(0,1,0.5))
      range.res = quantile(gradApplication[gradApplication[,"Chance.of.Admit"] > 0.7,"Research"], c(0,1,0.5))
      data = data.frame(
        `Your score` = c(inputdf()[,"University.Rating"], inputdf()[,"TOEFL.Score"],
                         inputdf()[,"GRE.Score"], inputdf()[,"SOP"], inputdf()[,"LOR"],
                         inputdf()[,"CGPA"], inputdf()[,"Research"]),
        Median = c(range.urt[3], range.tof[3], range.gre[3], range.sop[3], range.lor[3], range.gpa[3], range.res[3]),
        Max = c(5, 120, 340, 5, 5, 10, 1)
      )
      row.names(data) = c("universityRate", "TOEFL", "GRE", "SOP", "LOR", "GPA", "Research")

      renderRadarChart(div_id = "comparison",
                       data = data,
                       theme = "shine")
    })


    output$text = renderUI({
      str1 = paste("Your Probability for Getting into UCLA is: ","<br>","<center><font color=\" #2774AE\";font size=10;><b>", rate(), "</b></font></center>")
      if (rate()>0.9){
        str2 = paste0("Congratulations! You are ready for the admission!")}
      else if(rate()>0.5&rate()<0.9){
        str2 = "Congratulations! Work harder and you can make it!"
      }
      else{
        str2 = "Keep trying! You can refer to the radar plot to see what you need to improve!"
      }
      HTML(paste(str1,str2,sep='<br/>'))
    })

    output$image = renderUI({
      tags$img(src = "https://www.keirsuccess.com/images/keirsuccess/logos/ucla_logo.png",
               style = "height:80px")})

}


