################################################################################
################################################################################
## Title: Confidence intervals table
## Author: Steve Lane
## Date: Thu 14/02/2013
## Synopsis: Create a `nice' confidence interval table
################################################################################
################################################################################
## Input is a matrix with 1st column containing estimate, 2nd column lower CI,
## and 3rd column upper CI.
conf.int.table <- function(tab, dig = 2){
    r <- nrow(tab)
    tab <- formatC(tab, digits = dig, format = "f")
    nice.tab <- matrix(NA, nrow = r, ncol = 1)
    for(i in 1:r){
        nice.tab[i,] <- paste(tab[i, 1], " (", tab[i, 2], ", ", tab[i, 3], ")",
                              sep = "")
    }
    rownames(nice.tab) <- rownames(tab)
    nice.tab
}
