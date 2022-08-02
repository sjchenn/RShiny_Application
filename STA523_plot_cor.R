#' @title plot the gradApplication dataset pairwise scatter plot
#'
#' @description plot the correlation of gradApplication dataset for EDA purpose
#'
#' @return return Nothing
#'
#' @importFrom GGally ggpairs
#' @import ggplot2
#'
#' @export
plot_cor = function() {
    # Quick EDA
    ## check if na numbers and good to go
    data=gradApplication
    colSums(is.na(data))
    ## Pairwise Correlation
    ggpairs(data, columns = 2:ncol(data),
            title = "Pairwise Correlation Between Variables")+ theme_bw()
}

