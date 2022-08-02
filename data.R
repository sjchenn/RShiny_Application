#' @title Graduate Application Dataset
#'
#' @description  The dataset on kaggle is called "Graduate Admission 2" which was created for prediction of Graduate Admissions from an Indian student perspective. It contains some parameters on students' academic performance.
#'
#' @format A data frame with 500 observation on 9 variables.
#' \describe{
#' \item{Serial.No.}{A unique Serial Number}
#' \item{GRE.Score}{Graduate Record Examinations score}
#' \item{TOEFL.Score}{Test of English as a Foreign Languag score}
#' \item{University.Rating}{A rating of the goodness of applicant's university. 0 = very bad, 5 = very good}
#' \item{SOP}{Statement of Purpose strength. 0 = very weak, 5 = very strong}
#' \item{LOR}{Letter of Recommendation strength. 0 = very weak, 5 = very strong}
#' \item{CGPA}{Cumulative Grade Point Average}
#' \item{Research}{Whether applicant has research experience or not. 0 = no, 1 = yes}
#' \item{Chance.of.Admit}{The chance of being admitted to UCLA}
#' }
#'
#' @note This is imported directly from the kaggle dataset.
#'
#' @source https://www.kaggle.com/mohansacharya/graduate-admissions
#'
#' @examples
#' summary(gradApplication)
#'
"gradApplication"
