##' Get initial Food Value
##' 
##' This function computes the first food value to be used in the Food module for the
##' commodities classified as a "Food Estimate". The values are computed using
##' the average of the old food data (old methodology) and using the average of the 
##' imports. 
##' The function goes forward and backward and chooses the figure to be used based on
##' the following criteria:
##' 1. the figure that contains the year closer to the reference year
##' 2. in case the year is the same, use the figure from the source "food".
##' 
##'   
##' @param country A character vector with the country code.
##' @param commodity A character vector with the commodity code.
##' @param referenceYear A numeric with the reference year.
##' @param data The dataset to be used to compute food.
##'   
##' @return A dataset. 
##' @export
##' 


getInitialFoodValue <- function(country, commodity, referenceYear, data) {
    
    countryCommodity <- data.table(geographicAreaM49 = country, measuredItemCPC = commodity)
    keys = c("geographicAreaM49", "measuredItemCPC")
    dataMerge <- merge(countryCommodity, data, all.x = T, by = keys)
    
    # food
    firstFoodEstimateMin <- dataMerge[food > 0 & timePointYears > referenceYear,
                                      list(food = mean(food), 
                                           timePointYears = as.numeric(min(timePointYears)), 
                                           nrows = .N,
                                           source = "food"), 
                                      by = list(geographicAreaM49, measuredItemCPC)]
    
    firstFoodEstimateMax <- dataMerge[food > 0 & timePointYears < referenceYear,
                                      list(food = mean(food), 
                                           timePointYears = as.numeric(max(timePointYears)), 
                                           nrows = .N,
                                           source = "food"),
                                      by = list(geographicAreaM49, measuredItemCPC)]
    
    sourceFood <- rbind(firstFoodEstimateMax, firstFoodEstimateMin)
    
    # imports
    firstFoodImportedMin <- dataMerge[imports > 0 & timePointYears > referenceYear,
                                      list(food = mean(imports), 
                                           timePointYears = as.numeric(min(timePointYears)), 
                                           nrows = .N,
                                           source = "imports"), 
                                      by = list(geographicAreaM49, measuredItemCPC)]
    
    firstFoodImportedMax <- dataMerge[imports > 0 & timePointYears < referenceYear,
                                      list(food = mean(imports), 
                                           timePointYears = as.numeric(max(timePointYears)), 
                                           nrows = .N, 
                                           source = "imports"),
                                      by = list(geographicAreaM49, measuredItemCPC)]
    
    sourceImports <- rbind(firstFoodImportedMax, firstFoodImportedMin)
    
    # Combine different sources
    dataResult <- rbind(sourceFood, sourceImports)
    
    dataResult$source <- factor(dataResult$source, levels = c("food", "imports"), ordered = TRUE)
    
    setorder(dataResult, geographicAreaM49, measuredItemCPC, timePointYears, source)
    
    dataResultMax <- dataResult[timePointYears > referenceYear, 
                                .SD[1],
                                by = c("geographicAreaM49", "measuredItemCPC")]
    
    setorder(dataResult, geographicAreaM49, measuredItemCPC, -timePointYears, source)
    dataResultMin <- dataResult[timePointYears < referenceYear, 
                                .SD[1],
                                by = c("geographicAreaM49", "measuredItemCPC")]
    
    finalBind <- rbind(dataResultMax, dataResultMin)
    setnames(finalBind, "food", "initialFood")
    return(finalBind)
} 
