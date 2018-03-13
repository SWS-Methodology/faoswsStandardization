
##' Final Standardization to Primary Equivalent
##' 
##' After the full SUA has been balanced, all the commodities need to be rolled 
##' up to their primary equivalents.  This function does this, aggregating up 
##' trade and food.
##' 
##' @param data The data.table containing the full dataset for standardization.
##'
##' @param standParams The parameters for standardization.  These parameters 
##'   provide information about the columns of data and tree, specifying (for 
##'   example) which columns should be standardized, which columns represent 
##'   parents/children, etc.
##' @param loop just an index of loops

##'   
##' @return A data.table with the aggregated primary commodities.
##' @export

computeSupplyComponents= function(data, standParams, loop){


  
## Pull data about population
  
  # start and end year for standardization come from user parameters
  startYear = as.numeric(swsContext.computationParams$startYear)
  endYear = as.numeric(swsContext.computationParams$endYear)
  stopifnot(startYear <= endYear)

  yearVals = as.character(startYear:endYear)
  
  
  
  
  areaKeys = GetCodeList(domain = "suafbs", dataset = "sua", "geographicAreaM49")
  areaKeys = areaKeys[type == "country", code]
  
  
  # elemKeys="21"
  # key = DatasetKey(domain = "population", dataset = "population", dimensions = list(
  #   geographicAreaM49 = Dimension(name = "geographicAreaM49", keys = areaKeys),
  #   measuredElementSuaFbs = Dimension(name = "measuredElementPopulation", keys = elemKeys),
  #   timePointYears = Dimension(name = "timePointYears", keys = yearVals)
  # ))
  
  # areaKeys=standData[,unique(geographicAreaM49)]
  elemKeys="511"
  key = DatasetKey(domain = "population", dataset = "population_unpd", dimensions = list(
    geographicAreaM49 = Dimension(name = "geographicAreaM49", keys = areaKeys),
    measuredElementSuaFbs = Dimension(name = "measuredElement", keys = elemKeys),
    timePointYears = Dimension(name = "timePointYears", keys = as.character(yearVals))
  ))
  
  pop=GetData(key)

  pop[geographicAreaM49=="156",geographicAreaM49:="1248"]
  
  setnames(pop, "Value", "population")
  pop=pop[,.(geographicAreaM49,timePointYears, population)]
  
## Merge pop data with data
  
  
  data=merge(data, pop, by=c("geographicAreaM49","timePointYears"), all.x=TRUE,allow.cartesian = TRUE)
  
  
##   
  
  data[measuredElementSuaFbs=="Calories",DESfoodSupply_kCd:=(Value/365)/(population*1000)]
  data[measuredElementSuaFbs=="Proteins",proteinSupplyQt_gCd:=(Value/365)/(population*1000)]
  data[measuredElementSuaFbs=="Fats",fatSupplyQt_gCd:=(Value/365)/(population*1000)]
  
  
  elementsToCreate=c("DESfoodSupply_kCd","proteinSupplyQt_gCd","fatSupplyQt_gCd", "population")

  
  nutrients = lapply(elementsToCreate, function(x){
    
    temp=data[,mget(c("geographicAreaM49","timePointYears",x,paste0("fbsID",4-loop+1)))]
    temp[, Value := get(x)]
    temp[, c(standParams$elementVar) := x]
    temp[, c(x) := NULL]
    temp
  })

  nutrients= rbindlist(nutrients)
  
  nutrients=nutrients[!is.na(Value),]
  
  data[, c("DESfoodSupply_kCd","proteinSupplyQt_gCd","fatSupplyQt_gCd", "population"):=NULL]
  
  nutrients=nutrients[,mget(c("geographicAreaM49","timePointYears","measuredElementSuaFbs",paste0("fbsID", 4-loop+1),"Value"))]
  
  out = rbind(data,nutrients)
  out = unique(out)

  
  return(out)
   
}
  