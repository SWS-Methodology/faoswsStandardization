##' calculateFoodAggregates
##' 
##' The function creates all the Food aggregates to be displayed in the FBS.
##' This function calls the popoulation dataset in the SWS and displays population
##' in the resulting datatable
##' 
##' @param standData The data.frame resulting from the standardization process
##' containing all the elements needed for the calculation of DES
##' @param params The parameters for standardization.  These parameters 
##'   provide information about the columns of data and tree, specifying (for 
##'   example) which columns should be standardized, which columns represent 
##'   parents/children, etc.
##' @return A data.table containing all the food aggregates, by country-commodity-year
##' and also containing the population by year
##' 
##' @export
##' 


calculateFoodAggregates=function(standData=standData, params=c(),yearVals=yearVals)
{
  p=params

  # first the population dataset has to be downloaded

  # #SWS POPULATION NEW
  areaKeys=standData[,unique(geographicAreaM49)]
  elemKeys="511"
  key = DatasetKey(domain = "population", dataset = "population_unpd", dimensions = list(
    geographicAreaM49 = Dimension(name = "geographicAreaM49", keys = areaKeys),
    measuredElementSuaFbs = Dimension(name = "measuredElement", keys = elemKeys),
    timePointYears = Dimension(name = "timePointYears", keys = as.character(yearVals))
  ))
  # #SWS POPULATION OLD
  
  # areaKeys=standData[,unique(geographicAreaM49)]
  # elemKeys="21"
  # key = DatasetKey(domain = "population", dataset = "population", dimensions = list(
  #   geographicAreaM49 = Dimension(name = "geographicAreaM49", keys = areaKeys),
  #   measuredElementSuaFbs = Dimension(name = "measuredElementPopulation", keys = elemKeys),
  #   timePointYears = Dimension(name = "timePointYears", keys = as.character(yearVals))
  # ))
  
  popSWS=GetData(key)
  popSWS[geographicAreaM49=="156",geographicAreaM49:="1248"]
  
  # popSWS=popSWS[,mget(c("geographicAreaM49","measuredElement","timePointYears","Value"))]
  popSWS=popSWS[,mget(c("geographicAreaM49","measuredElement","timePointYears","Value"))]
  setnames(popSWS,"Value","population")
  
  standData= merge(standData,popSWS,by=c("geographicAreaM49","timePointYears"),all=TRUE)
  
  
  
  standData[,DESfoodSupply_kCd:=(Calories/365)/(population*1000)]
  standData[,proteinSupplyQt_gCd:=(Proteins/365)/(population*1000)]
  standData[,fatSupplyQt_gCd:=(Fats/365)/(population*1000)]
  standData[,Calories:=(Calories/1000000)]
  standData[,Proteins:=(Proteins/1000000)]
  standData[,Fats:=(Fats/1000000)]
  
  
  return(standData)
}
