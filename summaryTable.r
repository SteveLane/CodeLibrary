################################################################################
################################################################################
## Title: summaryTable
## Author: Steve Lane
## Date: Mon 24/11/2014
## Synopsis: Create summary tables for grouped data (e.g. male vs. female)
################################################################################
################################################################################
## Calculate mean and sd
summaryCont1 <- function(vals, dig = 2){
    estMean <- formatC(mean(vals, na.rm = TRUE), digits = dig, format = "f")
    estSd <- formatC(sd(vals, na.rm = TRUE), digits = dig - 1, format = "f")
    paste(estMean, " (", estSd, ")", sep = "")
}
## Calculate median and iqr
summaryCont2 <- function(vals, dig = 1){
    med <- formatC(median(vals, na.rm = TRUE), digits = dig, format = "f")
    iqr <- formatC(quantile(vals, probs = c(0.25, 0.75), na.rm = TRUE), digits =
                   dig, format = "f")
    paste(med, " [", iqr[1], ", ", iqr[2], "]", sep = "")
}
## Calculate percentage and number
summaryBin <- function(vals){
    n <- sum(vals, na.rm = TRUE)
    prop <- formatC(mean(vals, na.rm = TRUE)*100, digits = 1, format = "f")
    paste(prop, " (", n, ")", sep = "")
}
## Create a single row of the table
## isCont is an indicator for if the data is continuous, then use median (iqr)
createRow <- function(data, formula, varType, dig = 1){
    if(varType == 0){
        mf <- model.frame(formula, data)
        output <- c(summaryCont1(mf[,1], dig),
                    tapply(mf[,1], mf[,2], summaryCont1, dig = dig))
    } else if(varType == 3){
        mf <- model.frame(formula, data)
        output <- c(summaryCont2(mf[,1], dig),
                    tapply(mf[,1], mf[,2], summaryCont2, dig = dig))
    } else if (varType == 1){
        mf <- model.frame(formula, data)
        output <- c(summaryBin(mf[,1]), tapply(mf[,1], mf[,2], summaryBin))
    } else {
        mf <- model.matrix(formula, data)
        mf2 <- model.frame(formula, data)
        mf <- cbind(mf2[, 1], mf)
        output <- array(NA, dim = c(ncol(mf) - 1, 4))
        for(j in 2:ncol(mf)){
            nms <- paste("&emsp;", levels(factor(mf2[,2]))[j - 1], sep = "")
            output[j - 1,] <- c(nms,
                                summaryBin(mf[,j]),
                                tapply(mf[,j], mf[,1], summaryBin))
        }
    }
    ## browser()
    if(varType == 2){
        return(output)
    } else {
        return(c(names(mf)[1], output))
    }
}
## Put together summary table. Input requires the data, the list of variables,
## the group for comparison and a list of the type of variable (0 - continuous,
## 1 - binary, 2, categorical (entered as a character))
summaryTable <- function(data, variables, strata, varType, colHead, dig = 1){
    tab <- matrix(NA, ncol = 4)
    colnames(tab) <- colHead
    for(i in 1:length(variables)){
        if(varType[i] == 2){
            form <- as.formula(paste(strata, "~", variables[i], "- 1"))
            tab <- rbind(tab, c(variables[i], rep("", 3)))
        } else {
            form <- as.formula(paste(variables[i], "~", strata))
        }
        tab <- rbind(tab, createRow(data, form, varType[i], dig[i]))
    }
    tab[-1,]
}
################################################################################
################################################################################
