##' Get Commodity Tree
##' 
##' This function pulls the commodity trees in the new CPC system.
##' 
##' @param geographicAreaM49 A character vector of area codes.  The trees 
##'   returned are specific to country and year; thus, providing this parameter 
##'   limits which trees are pulled. If NULL, all are used.
##' @param timePointYears A character vector of years.  See geographicAreaM49.
##'   
##' @return A data.table object containing the commodity tree.  The dimension 
##'   columns correspond to the country, year, parent, and child commodity.  Two
##'   value columns are available: extraction rate and share.  The logic of the
##'   NEW system is that 0 ExtractionRates are NA, meaning that that connection
##'   is not valid for that cpuntry/commodity/year combination
##' 
##' @export
##' 

getCommodityTreeNewMethod = function(geographicAreaM49 = NULL, timePointYears = NULL){
  ## Data Quality Checks
  if(!exists("swsContext.datasets")){
    stop("No swsContext.datasets object defined.  Thus, you probably ",
         "won't be able to read from the SWS and so this function won't ",
         "work.")
  }
  stopifnot(is(geographicAreaM49, "character"))
  stopifnot(is(timePointYears, "character"))
  
  ## Define constants
  treeelemKeys = c("5423", "5431")
  # 5423 = Extraction Rate [hg/t]
  # 5431 = Share of utilization [%]
  
  ## Define the dimensions and check for input errors
  allAreaCodes = GetCodeList(domain = "suafbs", dataset = "ess_fbs_commodity_tree2",
                             dimension = "geographicAreaM49")
  allAreaCodes = allAreaCodes[type == "country", code]
  allYears = GetCodeList(domain = "suafbs", dataset = "ess_fbs_commodity_tree2",
                         dimension = "timePointYears")[, code]
  if(!is.null(geographicAreaM49)){
    stopifnot(geographicAreaM49 %in% allAreaCodes)
  }else{
    geographicAreaM49=allAreaCodes
  }
  if(!is.null(timePointYears)){
    stopifnot(timePointYears %in% allYears)
  }else{
    timePointYears=allYears
  }
  
  treeitemPKeys = GetCodeList(domain = "suafbs", dataset = "ess_fbs_commodity_tree2", "measuredItemParentCPC_tree")
  treeitemPKeys = treeitemPKeys[, code]
  
  treeitemCKeys = GetCodeList(domain = "suafbs", dataset = "ess_fbs_commodity_tree2", "measuredItemChildCPC_tree")
  treeitemCKeys = treeitemCKeys[, code]
  
  treekey = faosws::DatasetKey(domain = "suafbs", dataset = "ess_fbs_commodity_tree2", dimensions = list(
    geographicAreaM49 = Dimension(name = "geographicAreaM49", keys = geographicAreaM49),
    measuredElementSuaFbs = Dimension(name = "measuredElementSuaFbs", keys = treeelemKeys),
    measuredItemParentCPC = Dimension(name = "measuredItemParentCPC_tree", keys = treeitemPKeys),
    measuredItemChildCPC = Dimension(name = "measuredItemChildCPC_tree", keys = treeitemCKeys),
    timePointYears = Dimension(name = "timePointYears", keys = timePointYears)
  ))
  
  ## Extract the specific tree
  
  tree = faosws::GetData(treekey,omitna = FALSE)
  if("flag_obs_status_v2"%in%colnames(tree)){
    setnames(tree,"flag_obs_status_v2","flagObservationStatus")
  }
  
  
  if(nrow(tree[measuredElementSuaFbs=="5423"&is.na(Value)])>0){
    tree[measuredElementSuaFbs=="5423"&is.na(Value),flagObservationStatus:="T"]
    tree[measuredElementSuaFbs=="5423"&is.na(Value),flagMethod:="-"]
    tree[measuredElementSuaFbs=="5423"&is.na(Value),Value:=0]
  }
  if(nrow(tree[measuredElementSuaFbs=="5431"&is.na(Value)])>0){
    tree[measuredElementSuaFbs=="5431"&is.na(Value),flagObservationStatus:="E"]
    tree[measuredElementSuaFbs=="5431"&is.na(Value),flagMethod:="-"]
    tree[measuredElementSuaFbs=="5431"&is.na(Value),Value:=0]
  }    
  
  
  tree[measuredElementSuaFbs=="5423",measuredElementSuaFbs:="extractionRate"]
  tree[measuredElementSuaFbs=="5431",measuredElementSuaFbs:="share"]
  
  message("Commodity Tree correctly downloaded")
  
  setnames(tree,c("measuredItemParentCPC_tree","measuredItemChildCPC_tree"),
           c("measuredItemParentCPC","measuredItemChildCPC"))
  
  return(tree)    
}