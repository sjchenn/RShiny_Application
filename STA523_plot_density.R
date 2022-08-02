#' @title A function that extract theme
#'
#' @description An internal function that extract theme, only for plot arrangement purpose
#'
#' @param a.gplot A ggplot object
#'
#' @return return a legend
g_legend<-function(a.gplot){
  tmp <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

#' @title plot the gradApplication dataset density distribution
#'
#' @description plot the gradApplication dataset for EDA purpose
#'
#' @return return Nothing
#'
#' @export
plot_density = function() {
  # Quick EDA
  ## check if na numbers and good to go
  data=as.data.frame(gradApplication)
  data_level = data

  for (i in 1:nrow(data)){
    if (data$Chance.of.Admit[i] <= 0.5){
      data_level$Chance.of.Admit[i] = "low Possibility"
    }
    else if(data$Chance.of.Admit[i] > 0.5 & data_level$Chance.of.Admit[i] <= 0.7){
      data_level$Chance.of.Admit[i] = "middle Possibility"
    }
    else if(data$Chance.of.Admit[i] > 0.7){
      data_level$Chance.of.Admit[i] = "high Possibility"
    }
  }

  ## Distributions
  p1 = ggplot2::ggplot(data_level, ggplot2::aes(x = GRE.Score,
                                                fill = Chance.of.Admit)) +
    ggplot2::geom_histogram(stat = "bin", bins = 30,
                            position = "identity",
                            alpha = 0.5, show.legend = FALSE)
  p2 = ggplot2::ggplot(data_level, ggplot2::aes(x = TOEFL.Score,
                                                fill = Chance.of.Admit)) +
    ggplot2::geom_histogram(stat = "bin", bins = 30,
                            position = "identity",
                            alpha = 0.5, show.legend = FALSE)
  p3 = ggplot2::ggplot(data_level, ggplot2::aes(x = University.Rating,
                                                fill = Chance.of.Admit)) +
    ggplot2::geom_histogram(stat = "bin", bins = 30,
                            position = "identity",
                            alpha = 0.5, show.legend = FALSE)
  p4 = ggplot2::ggplot(data_level, ggplot2::aes(x = SOP,
                                                fill = Chance.of.Admit)) +
    ggplot2::geom_histogram(stat = "bin", bins = 30,
                            position = "identity",
                            alpha = 0.5, show.legend = FALSE)
  p5 = ggplot2::ggplot(data_level, ggplot2::aes(x = LOR,
                                                fill = Chance.of.Admit)) +
    ggplot2::geom_histogram(stat = "bin", bins = 30,
                            position = "identity",
                            alpha = 0.5, show.legend = FALSE)
  plegend = ggplot2::ggplot(data_level, ggplot2::aes(x = CGPA,
                                                fill = Chance.of.Admit)) +
    ggplot2::geom_histogram(stat = "bin", bins = 30,
                            position = "identity",
                            alpha = 0.5)
  legend = g_legend(plegend)
  p6 = ggplot2::ggplot(data_level, ggplot2::aes(x = CGPA,
                                                     fill = Chance.of.Admit)) +
    ggplot2::geom_histogram(stat = "bin", bins = 30,
                            position = "identity",
                            alpha = 0.5, show.legend = FALSE)
  p7 = ggplot2::ggplot(data_level, ggplot2::aes(x = as.factor(Research),
                                                fill = Chance.of.Admit)) +
    ggplot2::geom_histogram(stat = "count",
                            alpha = 0.5, show.legend = FALSE) +
    xlab("Research")
  p8 = ggplot2::ggplot(data, ggplot2::aes(x = Chance.of.Admit)) +
  ggplot2::geom_histogram()
  gridExtra::grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,legend)
}
