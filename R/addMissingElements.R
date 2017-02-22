##' Add Missing Elements
##' 
##' This function takes a data.table and adds rows to it so that the data 
##' contains an observation for all elements for each commodity in the dataset.
##' 
##' @param data The data.table containing the full dataset for standardization.
##' @param standParams The parameters for standardization.  These parameters 
##'   provide information about the columns of data and tree, specifying (for 
##'   example) which columns should be standardized, which columns represent 
##'   parents/children, etc.
##'   
##' @return The same data.table as what was passed ("data") but possibly with
##'   additional rows.
##'   

addMissingElements = function(data, standParams){
  
  ## Data Quality Checks
  if(nrow(data[, .N, by = c(standParams$yearVar, standParams$geoVar)]) > 1)
    stop("This function is designed to work with only one country/year at ",
         "a time!")
  
  elements = standParams[c("productionCode", "importCode", "exportCode",
                           "stockCode", "foodCode", "foodProcCode",
                           "feedCode", "wasteCode", "seedCode",
                           "industrialCode", "touristCode", "residualCode")]
  elements = as.character(elements)
  fullTable = expand.grid(unique(data[[standParams$itemVar]]), elements)
  colnames(fullTable) = c(standParams$itemVar, standParams$elementVar)
  fullTable[[standParams$yearVar]] = data[[standParams$yearVar]][1]
  fullTable[[standParams$geoVar]] = data[[standParams$geoVar]][1]
  fullTable = data.table(fullTable)
  fullTable[, c(standParams$itemVar) := as.character(get(standParams$itemVar))]
  fullTable[, c(standParams$elementVar) := as.character(get(standParams$elementVar))]
  
  data = merge(data, fullTable, by = c(standParams$itemVar, standParams$elementVar,
                                       standParams$geoVar, standParams$yearVar),
               all = TRUE)
  return(data)
}
