################################################################################
################################################################################
## Title: Difference in medians confidence interval
## Author: Steve Lane
## Date: Wed 05/11/2014
## Synopsis: Calculate the confidence interval for the difference in medians
## between two groups, by (stratified) bootstrapping.
################################################################################
################################################################################
require(boot)
medDiff <- function(data, ind, formula){
    localData <- model.frame(formula, data)[ind,]
    medDiff <- diff(tapply(localData[,1], localData[,2], median))
    medDiff
}
## Prototype code:
## out <- boot(data, medDiff, R = 9999, strata = data$strata, formula =
##             as.formula("outcome ~ strata"))
