##' Converts sugar codes 23511.01 and 23512 to 2351f
##' 
##' 
##' This function harmonize sugar codes. 
##' Raw cane and beet sugar are considered as separate codes in some domain, like trade, because sugar raw can be traded
##' as beet raw, cane raw or just raw (23511.01, 23512 or 2351f), 
##' but when one has to go from the processed product to the primary product, 
##' is not possible to know if a code 2351f has to be standardized to cane or beet, therefore 
##' in the standardization process cane and beet have to be considered as a unique code (2351f)
##' This function makes checks and harmonize sugar codes 
##'  
##' 
##' @param data the downloaded data from sua
##' @param p The parameters for standardization.  These parameters 
##'   provide information about the columns of data and tree, specifying (for 
##'   example) which columns should be standardized, which columns represent 
##'   parents/children, etc.
##' @return A data.table with the data fixed for sugar
##' @export
##' 

convertSugarCodes = function(data){
  
dataSugar=data.table(data.frame(data[measuredItemSuaFbs %in% c("23511.01","23512","2351f")]))

dataSugar[,ValueSugar:=sum(Value*ifelse(measuredItemSuaFbs=="2351f",0,1)),by=c("geographicAreaM49","timePointYears","measuredElementSuaFbs")]
sugarComb = dataSugar[, .N, by = c("geographicAreaM49", "timePointYears","measuredElementSuaFbs")]


##### dataSugarA #####
# Filter all the rows for which only one row exists

filterSugarA=sugarComb[N==1]
filterSugarA=filterSugarA[,N:=NULL]
dataSugarA=dataSugar[filterSugarA, ,on=c("geographicAreaM49", "timePointYears","measuredElementSuaFbs")]
# check if some of this rows are not 2351f and change the flag, in that case
dataSugarA[measuredItemSuaFbs!="2351f",flagObservationStatus:=ifelse(flagObservationStatus!="I","I",flagObservationStatus)]
dataSugarA[measuredItemSuaFbs!="2351f",flagMethod:=ifelse(!(flagMethod%in%c("e","s")),"s",flagMethod)]
dataSugarA[,measuredItemSuaFbs:="2351f"]
dataSugarA[,ValueSugar:=NULL]


##### dataSugarB #####

filterSugarB=sugarComb[N>1]
filterSugarB=filterSugarB[,N:=NULL]
dataSugarC=dataSugar[filterSugarB, ,on=c("geographicAreaM49", "timePointYears","measuredElementSuaFbs")]
dataSugarC[,s2351f:=max(ValueSugar,Value),by=c("geographicAreaM49","timePointYears","measuredElementSuaFbs")]

dataSugarC[,c("Value","ValueSugar"):=NULL]
dataSugarC[,Value:=s2351f*ifelse(measuredItemSuaFbs=="2351f",1,0),by=c("geographicAreaM49","timePointYears","measuredElementSuaFbs")]
dataSugarB=dataSugarC[Value!=0]

##### dataSugarD #####

sugarComb2=dataSugarB[,.N,by=c("geographicAreaM49","timePointYears","measuredElementSuaFbs")][,N:=NULL]
dataSugarD=dataSugarC[sugarComb2,,on=c("geographicAreaM49","timePointYears","measuredElementSuaFbs")]

dataSugarD=data.table(setdiff(dataSugarC,dataSugarD))

if(nrow(dataSugarD)>0){
dataSugarD[,Value:=sum(s2351f*ifelse(measuredItemSuaFbs=="2351f",0,1)),by=c("geographicAreaM49","timePointYears","measuredElementSuaFbs")]
dataSugarD=unique(dataSugarD,by=c("geographicAreaM49","timePointYears","measuredElementSuaFbs"))
dataSugarD[,measuredItemSuaFbs:="2351f"]
dataSugarD=dataSugarD[,colnames(dataSugarA),with=FALSE]
dataSugarB=dataSugarB[,colnames(dataSugarA),with=FALSE]
dataTorBind=rbind(dataSugarA,dataSugarB,dataSugarD)
}else{
  dataSugarB=dataSugarB[,colnames(dataSugarA),with=FALSE]
  dataTorBind=rbind(dataSugarA,dataSugarB)
}


data=data[!(measuredItemSuaFbs %in% c("23511.01","23512","2351f"))]
data=rbind(data,dataTorBind)

return(data)

}