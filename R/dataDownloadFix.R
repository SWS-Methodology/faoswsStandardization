##' Data Download and Fix of sugar Values
##' 
##' 
##' This function download the data from sua_unbalanced dataset, then harmonize sugar codes. 
##' Raw cane and beet sugar are considered as separate codes in some domain, like trade, because sugar raw can be traded
##' as beet raw, cane raw or just raw (23511.01, 23512 or 2351f), 
##' but when one has to go from the processed product to the primary product, 
##' is not possible to know if a code 2351f has to be standardized to cane or beet, therefore 
##' in the standardization process cane and beet have to be considered as a unique code (2351f)
##' This function makes checks and harmonize sugar codes 
##'  
##' 
##' @param key Key with dimensions for data download.
##' @param p The parameters for standardization.  These parameters 
##'   provide information about the columns of data and tree, specifying (for 
##'   example) which columns should be standardized, which columns represent 
##'   parents/children, etc.
##' @return A data.table with the data fixed for sugar
##' @export
##'  


dataDownloadFix = function(key=c(), p = c() ){

data = elementCodesToNames(data = GetData(key), itemCol = "measuredItemFbsSua",
                           elementCol = "measuredElementSuaFbs")
setnames(data, "measuredItemFbsSua", "measuredItemSuaFbs")
data[measuredElementSuaFbs=="stock_change",measuredElementSuaFbs:="stockChange"]
data[measuredElementSuaFbs=="stock",measuredElementSuaFbs:="stockChange"]

data=data[timePointYears%in%yearVals]
data=data[!is.na(measuredElementSuaFbs)]

##############################################################
######### SUGAR RAW CODES TO BE CONVERTED IN 2351F ###########
##############################################################
datas=data[measuredItemSuaFbs %in% c("23511.01","23512","2351f")]
datas[measuredElementSuaFbs=="tourist",measuredItemSuaFbs:="2351f"]
datas[measuredElementSuaFbs=="stockChange"&geographicAreaM49=="705",measuredItemSuaFbs:="2351f"]
datas[measuredElementSuaFbs=="food"&geographicAreaM49%in%c("422","28","308"),measuredItemSuaFbs:="2351f"]
datas[,s2351f:=sum(Value*
                     ifelse(measuredElementSuaFbs == "production"&measuredItemSuaFbs=="2351f",1,0),na.rm = TRUE),
      by=c("geographicAreaM49","timePointYears")]
dataTorBind = unique(datas[measuredElementSuaFbs=="production",list(geographicAreaM49,timePointYears,measuredElementSuaFbs,s2351f,flagObservationStatus,flagMethod)])
datas[,s2351f:=NULL]
datas=datas[!(measuredItemSuaFbs%in%c("23511.01","23512"))]
dataTorBind = dataTorBind[,measuredItemSuaFbs:="2351f"]

setnames(dataTorBind,"s2351f","Value")
dataTorBind=dataTorBind[,c(7,3,1,2,4:6),with=FALSE]

datas=rbind(datas[!measuredElementSuaFbs=="production"],dataTorBind)
data=data[!(measuredItemSuaFbs %in% c("23511.01","23512","2351f"))]

data=rbind(data,datas)





data=data[, list(Value = sum(Value, na.rm = TRUE)),
          by = c("measuredItemSuaFbs","measuredElementSuaFbs", "geographicAreaM49", "timePointYears","flagObservationStatus","flagMethod")]
data=left_join(data,flagValidTable,by=c("flagObservationStatus","flagMethod"))%>%
  data.table

data[flagObservationStatus%in%c("","T"),Official:=TRUE]
data[is.na(Official),Official:=FALSE]


return(data)
}