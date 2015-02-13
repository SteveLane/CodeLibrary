################################################################################
################################################################################
## Title: summOutliers
## Author: Steve Lane
## Date: Tue 06/01/2015
## Synopsis: Calculate 6 number summaries, and Tukey outliers.
################################################################################
################################################################################
summOutliers <- function(x, ID){
    summ <- summary(x)[1:6]
    box <- boxplot(x)$out
    outs <- ID[x %in% box]
    matrix(c(formatC(summ, digits = 2, format = "f"),
             paste(outs, collapse = ", ")), nrow = 1)
}
