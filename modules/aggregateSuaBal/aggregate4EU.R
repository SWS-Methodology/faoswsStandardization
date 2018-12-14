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
library(readr)

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

EU28=c(40,
       56,
       100,
       191,
       196,
       203,
       208,
       233,
       246,
       250,
       276,
       300,
       348,
       372,
       380,
       428,
       440,
       442,
       470,
       528,
       616,
       620,
       642,
       703,
       705,
       724,
       752,
       826)

# top48FBSCountries<-as.character(top48FBSCountries)
# 
# selectedCountries = setdiff(geoKeys,top48FBSCountries) #229
# 


# ##Select the countries based on the user input parameter
selectedCountries = EU28


selectedGEOCode = as.character(EU28)



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

 
 ############## GET FISH DATA AND CLEAN
 fish<-read.csv("C:\\Users\\Delbello.FAODOMAIN\\Documents\\fisheryEU29.csv")
 setDT(fish)
 fish[,geographicAreaM49:=fs2m49(as.character(fish$Country))]
 
setnames(fish,old=c("X2014","X2015","X2016"),new=c("2014","2015","2016"))
fish[,c("Country","country_name"):=NULL]
 
fishM=melt.data.table(fish,measure.vars = patterns("^2"), value.name = "Value", variable.name="timePointYears")

fishM[,measuredElementSuaFbs:="664"]
fishM[,measuredItemFbsSua:="9999"]
setcolorder(fishM,c("geographicAreaM49", "measuredItemSuaFbs", "measuredElementFbsSua", "timePointYears", "Value1"))
setnames(fishM,"Value1","Value")


 ################### 
 
 
 fbsTree<-ReadDatatable("fbs_tree")
fbsCodes<-GetCodeList(domain = "suafbs", dataset = "fbs_balanced_", "measuredItemFbsSua")
fbsCodes=subset(fbsCodes,grepl("^S",code))
fbsCodes=subset(fbsCodes,grepl("[[:upper:]]\\s",description))
fbsCodes$code<-gsub("^S","",fbsCodes$code)
# 
 fbsTree <- merge(fbsTree,fbsCodes,by.x = "id1",by.y = "code")
 
# 
# 
 kcalData <- merge(kcalData,fbsTree,by.x = "measuredItemFbsSua",by.y = "item_sua_fbs")
# 
# totalDes=kcalData_id[,totDES:=sum(Value, na.rm = T), by=c("geographicAreaM49,timePointYears,id1")]
# 
# #kcalData<-kcalData[,totDES:=sum(Value, na.rm = T), by=c("geographicAreaM49,timePointYears,id1")]
agg2<-kcalData[,desAgg2:=sum(Value, na.rm = T), by=c("geographicAreaM49","timePointYears","id2")]
agg1<-kcalData[,desAgg1:=sum(Value, na.rm = T), by=c("geographicAreaM49","timePointYears","id1")]



agg2=agg2[,c("geographicAreaM49","measuredItemFbsSua","measuredElementSuaFbs","timePointYears","id2","desAgg2"),with=F]
agg1=agg1[,c("geographicAreaM49","measuredItemFbsSua","measuredElementSuaFbs","timePointYears","id1","desAgg1"),with=F]

agg2[,measuredItemFbsSua:=id2]
agg2[,id2:=NULL]
setnames(agg2,"desAgg2","Value")

agg2=unique(agg2)
#prova=subset(prova,id2=="2903" | id2=="2941" )
#agg2<-nameData("suafbs","fbs_balanced_",agg2)

agg1[,measuredItemFbsSua:=id1]
agg1[,id1:=NULL]
setnames(agg1,"desAgg1","Value")

agg1=unique(agg1)

aggData=rbind(agg1,agg2,fishM)
aggData[,Value:=round(Value,0)]

aggDataWide = dcast.data.table(setDT(aggData),geographicAreaM49+
measuredItemFbsSua ~timePointYears , fun.aggregate = NULL,value.var = "Value")
aggDataWide$measuredItemFbsSua<-gsub("^2","S2",aggDataWide$measuredItemFbsSua)
aggDataWide<-nameData("suafbs","fbs_balanced_",aggDataWide)

aggDataWide[,measuredItemFbsSua_description:=ifelse(measuredItemFbsSua=="9999","FISHERY",measuredItemFbsSua_description)]
aggDataWide[,measuredItemFbsSua_description:=ifelse(measuredItemFbsSua=="S2901","GRAND TOTAL EXCL. FISH",measuredItemFbsSua_description)]
aggDataWide[,measuredItemFbsSua_description:=ifelse(measuredItemFbsSua=="S2941","ANIMAL PRODUCTS EXCL. FISH",measuredItemFbsSua_description)]

write.csv(aggDataWide,"EU28_toSend.csv",row.names = F)



#prova=subset(prova,id2=="2903" | id2=="2941" )
#agg1<-nameData("suafbs","fbs_balanced_",agg1)
# 
# aniVeg[,measuredItemFbsSua:=ifelse(measuredItemFbsSua=="2903","S2903","S2941")]
# aniVegWide = dcast.data.table(setDT(aniVeg),geographicAreaM49+
#                             measuredItemFbsSua ~timePointYears , fun.aggregate = NULL,value.var = "Value")
# 
# toSend=rbind(GTWide,kcalDataWide)
# 
# # 
 # kcalData2 = melt.data.table(totalDES,id.vars = c("geographicAreaM49","measuredElementSuaFbs","measuredItemFbsSua","timePointYears","Value","totDES"),
 #                           measure.vars = "totDES")
# kcalData2 = dcast.data.table(setDT(kcalData2),geographicAreaM49+measuredItemFbsSua~timePointYears + totDES, fun.aggregate = NULL,value.var = "Value")
# 
# #kcalData<-nameData("suafbs","fbs_balanced",sua_balanced_data)
# 
# 
# #sua_balanced_data$measuredItemFbsSua <- paste0("'",sua_balanced_data$measuredItemFbsSua,sep = "")
# 

