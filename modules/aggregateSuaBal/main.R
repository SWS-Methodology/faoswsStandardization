##'
##' **Author: Carlo Del Bello**
##'
##' **Description:**
##'
##' This module is designed to aggregate des figures at SUA balanced level
##' 
##' 
##' **Inputs:**
##'
##' * sua balanced
##' 
##'' **Output:**
##'
##' * csv file attached in mail





## load the library
library(faosws)
library(data.table)
library(faoswsUtil)
library(sendmailR)
library(dplyr)
library(faoswsUtil)
library(faoswsStandardization)
library(faoswsFlag)


## set up for the test environment and parameters
R_SWS_SHARE_PATH = Sys.getenv("R_SWS_SHARE_PATH")

if(CheckDebug()){
  message("Not on server, so setting up environment...")
  
  library(faoswsModules)
  SETT <- ReadSettings("modules/aggregateSuaBal/sws.yml")
  
  R_SWS_SHARE_PATH <- SETT[["share"]]  
  ## Get SWS Parameters
  SetClientFiles(dir = SETT[["certdir"]])
  GetTestEnvironment(
    baseUrl = SETT[["server"]],
    token = SETT[["token"]]
  )
}

#startYear = as.numeric(swsContext.computationParams$startYear)
startYear = as.numeric(2010)

#endYear = as.numeric(swsContext.computationParams$endYear)
endYear = as.numeric(2016)
#geoM49 = swsContext.computationParams$geom49
stopifnot(startYear <= endYear)
yearVals = startYear:endYear

##' Get data configuration and session
sessionKey = swsContext.datasets[[1]]

sessionCountries =
  getQueryKey("geographicAreaM49", sessionKey)

geoKeys = GetCodeList(domain = "agriculture", dataset = "aproduction",
                      dimension = "geographicAreaM49")[type == "country", code]

top48FBSCountries = c(4,24,50,68,104,120,140,144,148,1248,170,178,218,320,
                      324,332,356,360,368,384,404,116,408,450,454,484,508,
                      524,562,566,586,604,608,716,646,686,762,834,764,800,
                      854,704,231,887,894,760,862,860)

# top48FBSCountries<-as.character(top48FBSCountries)
# 
# selectedCountries = setdiff(geoKeys,top48FBSCountries) #229
# 


# ##Select the countries based on the user input parameter
selectedGEOCode =sessionCountries





#########################################
##### Pull from SUA balanced data #####
#########################################

message("Pulling SUA balanced Data")

#take geo keys
geoDim = Dimension(name = "geographicAreaM49", keys = selectedGEOCode)

#Define element dimension. These elements are needed to calculate net supply (production + net trade)

eleKeys = GetCodeList(domain = "suafbs", dataset = "sua_balanced", "measuredElementSuaFbs")
eleKeys <-eleKeys[, code]

eleDim <- Dimension(name = "measuredElementSuaFbs", keys = eleKeys)


#Define item dimension

itemKeys = GetCodeList(domain = "suafbs", dataset = "sua_balanced", "measuredItemFbsSua")
itemKeys = itemKeys[, code]

itemDim <- Dimension(name = "measuredItemFbsSua", keys = itemKeys)


# Define time dimension

timeDim <- Dimension(name = "timePointYears", keys = as.character(yearVals))

#Define the key to pull SUA data
key = DatasetKey(domain = "suafbs", dataset = "sua_balanced", dimensions = list(
  geographicAreaM49 = geoDim,
  measuredElementSuaFbs = eleDim,
  measuredItemFbsSua = itemDim,
  timePointYears = timeDim
))



sua_balanced_data = GetData(key)
# 
 kcalData <- subset(sua_balanced_data, measuredElementSuaFbs %in% c("664"))
 
 kcalData[,6:7]<-NULL
 
 kcalData<-kcalData %>% group_by(geographicAreaM49,timePointYears) %>%  mutate(totDES=sum(Value,na.rm = T))

 kcalData2 = melt.data.table(setDT(kcalData),id.vars = c("geographicAreaM49","measuredItemFbsSua","timePointYears"),
                             measure.vars = c("Value","totDES"))
 setnames(kcalData2, "variable", "Aggregation")
 setnames(kcalData2, "value", "Value")
 
 kcalData2[,Aggregation:=ifelse(Aggregation=="Value","Item","GRAND TOTAL")]
 
 grandTotal=subset(kcalData2,Aggregation=="GRAND TOTAL")
 grandTotal[,measuredItemFbsSua:="S2901"]
 grandTotal=unique(grandTotal)
 grandTotal=nameData("suafbs","fbs_balanced_",grandTotal)
items=nameData("suafbs","sua_balanced",subset(kcalData2,Aggregation=="Item"))
 
 kcalDataWide = dcast.data.table(setDT(items),geographicAreaM49+geographicAreaM49_description+
                                   measuredItemFbsSua+measuredItemFbsSua_description~timePointYears , 
                                 fun.aggregate = NULL,value.var = "Value")
 GTWide = dcast.data.table(setDT(grandTotal),geographicAreaM49+geographicAreaM49_description+
                             measuredItemFbsSua+measuredItemFbsSua_description~timePointYears , fun.aggregate = NULL,value.var = "Value")
 
 toSend=rbind(GTWide,kcalDataWide)
 



bodySuaBALAggregation= paste("The Email contains the aggregated caloric intake at sua balanced level",
                          sep='\n')

sendMailAttachment(toSend,"SuaBalAggregation",bodySuaBALAggregation)




############ calculate imbalance

commDef=ReadDatatable("fbs_commodity_definitions")
# primaryProxyPrimary=commDef$cpc[commDef[,proxy_primary=="X" | primary_commodity=="X"]]
# primary=commDef$cpc[commDef[,primary_commodity=="X"]]
# 
# 
# proxyPrimary=commDef$cpc[commDef[,proxy_primary=="X" | derived=="X"]]
food=commDef$cpc[commDef[,food_item=="X"]]



sua_balanced_data[, imbalance := sum(ifelse(is.na(Value), 0, as.numeric(Value)) *
                               ifelse(measuredElementSuaFbs == "5510", 1,
                                      ifelse(measuredElementSuaFbs == "5610", 1,
                                             ifelse(measuredElementSuaFbs == "5910", -1,
                                                    ifelse(measuredElementSuaFbs == "5071", -1,
                                                           ifelse(measuredElementSuaFbs == "5141", -1,
                                                                  ifelse(measuredElementSuaFbs == "5023", -1,
                                                                         ifelse(measuredElementSuaFbs == "5520", -1,
                                                                                ifelse(measuredElementSuaFbs == "5016", -1,
                                                                                       ifelse(measuredElementSuaFbs == "5525", -1,
                                                                                              ifelse(measuredElementSuaFbs == "5165", -1,
                                                                                                     ifelse(measuredElementSuaFbs == "5164", -1,
                                                                                                            ifelse(measuredElementSuaFbs == "5166", -1,0))))))))))))),
          by =c("geographicAreaM49","measuredItemFbsSua", "timePointYears") ]

sua_balanced_data[, perc.imbalance := 100*abs(imbalance)/sum(ifelse(is.na(Value), 0, as.numeric(Value)) *
                                                      ifelse(measuredElementSuaFbs == 5510, 1,
                                                             ifelse(measuredElementSuaFbs == 5610, 1,
                                                                    ifelse(measuredElementSuaFbs == 5910, -1,
                                                                           ifelse(measuredElementSuaFbs == 5071, -1,
                                                                                  ifelse(measuredElementSuaFbs == 5141, 0,
                                                                                         ifelse(measuredElementSuaFbs == 5023, 0,
                                                                                                ifelse(measuredElementSuaFbs == 5520, 0,
                                                                                                       ifelse(measuredElementSuaFbs == 5016, 0,
                                                                                                              ifelse(measuredElementSuaFbs == 5525, 0,
                                                                                                                     ifelse(measuredElementSuaFbs == 5165, 0,
                                                                                                                            ifelse(measuredElementSuaFbs == 5164, 0,
                                                                                                                                   ifelse(measuredElementSuaFbs == 5166, 0,0))))))))))))),
                  by =c("geographicAreaM49","measuredItemFbsSua", "timePointYears") ]


imbToSend=subset(sua_balanced_data,measuredItemFbsSua %in% food & timePointYears>=2014 & abs(imbalance)>100 & abs(perc.imbalance)>1)
imbToSend=unique(imbToSend, incomparables=FALSE, fromLast=FALSE, by=c("geographicAreaM49","measuredItemFbsSua","timePointYears","imbalance"))
imbToSend$measuredElementSuaFbs<-NULL
imbToSend$Value<-NULL
imbToSend$flagObservationStatus<-NULL
imbToSend$flagMethod<-NULL
data.table::setorder(imbToSend,-perc.imbalance)

imbToSend=nameData("suafbs","sua_balanced",imbToSend)

bodyImbalances= paste("The Email contains the list of imbalanced food items at SUA bal level. Please check them.",
                             sep='\n')

sendMailAttachment(setDT(imbToSend),"ImbalanceList",bodyImbalances)


#### FBS AGGREGATES AT SUA LEVEL (NO STANDARDIZATION INVOLVED HERE)


fbsTree<-ReadDatatable("fbs_tree")
fbsCodes<-GetCodeList(domain = "suafbs", dataset = "fbs_balanced_", "measuredItemFbsSua")
fbsCodes=subset(fbsCodes,grepl("^S",code))
fbsCodes=subset(fbsCodes,grepl("[[:upper:]]\\s",description))
fbsCodes$code<-gsub("^S","",fbsCodes$code)
# 
fbsTree <- merge(fbsTree,fbsCodes,by.x = "id1",by.y = "code")



#########
kcalData <- subset(sua_balanced_data, measuredElementSuaFbs %in% c("664"))

kcalData[,6:7]<-NULL


# 
# 
kcalData <- merge(kcalData,fbsTree,by.x = "measuredItemFbsSua",by.y = "item_sua_fbs")
# 
# totalDes=kcalData_id[,totDES:=sum(Value, na.rm = T), by=c("geographicAreaM49,timePointYears,id1")]
# 
# #kcalData<-kcalData[,totDES:=sum(Value, na.rm = T), by=c("geographicAreaM49,timePointYears,id1")]
agg4=kcalData[,desAgg4:=sum(Value, na.rm = T), by=c("geographicAreaM49","timePointYears","id4")]
agg3=kcalData[,desAgg3:=sum(Value, na.rm = T), by=c("geographicAreaM49","timePointYears","id3")]
agg2=kcalData[,desAgg2:=sum(Value, na.rm = T), by=c("geographicAreaM49","timePointYears","id2")]
agg1=kcalData[,desAgg1:=sum(Value, na.rm = T), by=c("geographicAreaM49","timePointYears","id1")]


agg4=agg4[,c("geographicAreaM49","measuredItemFbsSua","measuredElementSuaFbs","timePointYears","id4","desAgg4"),with=F]
agg3=agg3[,c("geographicAreaM49","measuredItemFbsSua","measuredElementSuaFbs","timePointYears","id3","desAgg3"),with=F]
agg2=agg2[,c("geographicAreaM49","measuredItemFbsSua","measuredElementSuaFbs","timePointYears","id2","desAgg2"),with=F]
agg1=agg1[,c("geographicAreaM49","measuredItemFbsSua","measuredElementSuaFbs","timePointYears","id1","desAgg1"),with=F]

agg4[,measuredItemFbsSua:=id4]
agg4[,id4:=NULL]
setnames(agg4,"desAgg4","Value")
agg4=unique(agg4)

agg3[,measuredItemFbsSua:=id3]
agg3[,id3:=NULL]
setnames(agg3,"desAgg3","Value")
agg3=unique(agg3)

agg2[,measuredItemFbsSua:=id2]
agg2[,id2:=NULL]
setnames(agg2,"desAgg2","Value")
agg2=unique(agg2)


agg1[,measuredItemFbsSua:=id1]
agg1[,id1:=NULL]
setnames(agg1,"desAgg1","Value")

agg1=unique(agg1)

aggData=rbind(agg1,agg2,agg3,agg4)
aggData[,Value:=round(Value,0)]

aggDataWide = dcast.data.table(setDT(aggData),geographicAreaM49+
                                 measuredItemFbsSua ~timePointYears , fun.aggregate = NULL,value.var = "Value")
aggDataWide$measuredItemFbsSua<-gsub("^2","S2",aggDataWide$measuredItemFbsSua)
aggDataWide<-nameData("suafbs","fbs_balanced_",aggDataWide)


bodyImbalances= paste("The Email contains the FBS aggregates by DES calculated at SUA BALANCED LEVEL.",
                      sep='\n')

sendMailAttachment(setDT(aggDataWide),"FbsAggAtSUA",bodyImbalances)

