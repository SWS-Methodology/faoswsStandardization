##' calculateFoodProc
##' 
##' 
##' 
##' @param data The data.table containing the full dataset for standardization. 
##'   It should have columns corresponding to year, country, element, commodity,
##'   and value.  The specific names of these columns should be in standParams.
##' @param tree The commodity tree which details how elements can be processed 
##'   into other elements.  It does not, however, specify which elements get 
##'   aggregated into others.  This data.table should have columns parent, 
##'   child, extraction rate, share, and target (target specifying if an element
##'   is processed forward or not).  Names of these columns should be provided
##'   in standParams.
##'
##' @param params The parameters for standardization.  These parameters 
##'   provide information about the columns of data and tree, specifying (for 
##'   example) which columns should be standardized, which columns represent 
##'   parents/children, etc.
##' @param availability is availability used for calculation of food processing 
##' @param zeroWeight is vector of co-products
##' @return A data.table containing the final balanced and standardized SUA 
##'   data.  Additionally, this table will have new elements in it if 
##'   nutrientData was provided.

##' 
##' @export
##' 


calculateFoodProc=function(data=data, params=c(), tree=tree, availability=availability,zeroWeight= zeroWeight)
  
  
  
{
  p=params

  # data[measuredElementSuaFbs==p$productionCode & availability<0 & is.na(Value), Value:=-availability]
  
  
  mergeToTree = data[get(params$elementVar)== params$productionCode,list(measuredItemSuaFbs=get(params$itemVar),Value=Value)]
  
  setnames(mergeToTree, params$itemVar, params$childVar)
  tree = merge(tree, mergeToTree, by = params$childVar, all.x = TRUE)
  
    
    
  tree[, foodProcElement:= ((Value/extractionRate)*share)*weight]
  mergeToData=tree[,.(measuredItemSuaFbs=get(params$parentVar), foodProcElement)]

  if(nrow(mergeToData[!is.na(foodProcElement),])>0){
  mergeToData=aggregate(foodProcElement~measuredItemSuaFbs, data=mergeToData, FUN= sum)
  }
  mergeToData=data.table(mergeToData)
  
  mergeToData=mergeToData[!is.na(foodProcElement),]
  
  
  return(mergeToData)
}
