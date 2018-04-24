##' # Pull data from different domains to sua 
##'
##' **Author: Cristina Muschitiello**
##'
##' **Description:**
##'
##' This module is designed to harvest the data from other tables and pull all
##' relevant FBS data into the SUA/FBS domain.  It pulls from the following
##' 
##' **Inputs:**
##'
##' * Agriculture Production (production, stock, seed, industrial)
##' * Food (food)
##' * Loss (loss)
##' * feed (feed) 
##' * stock (stock) 
##' * Trade:
##' in november 2017, for urgent purposes, as it was not possible to validate all the new Trade data
##' it has been decided to use:
##'    . Old Trade data up to 2013
##'    . New Trade data from 2014 (Trade domain)
##' * Tourist (tourist)

##'
##' **Flag assignment:**
##'
##' | Observation Status Flag | Method Flag|
##' | --- | --- | --- |


## load the library
library(faosws)
library(data.table)
library(faoswsUtil)
library(sendmailR)

oldProductionCode = "51"
foodCode = "5141"
importCode = "5610"
exportCode = "5910"
oldFeedCode = "101"
oldSeedCode = "111"

#oldLossCode = "121"
lossCode = "5016"
industrialCode = "5165"
touristCode = "100"
suaTouristCode = "5164"
# Convert tourism units to tonnes
# touristConversionFactor = -1/1000
touristConversionFactor = 1
# warning("Stocks is change in stocks, not absolute! This needs to be changed")
stocksCode = "5071"

## set up for the test environment and parameters
R_SWS_SHARE_PATH = Sys.getenv("R_SWS_SHARE_PATH")

if(CheckDebug()){
  message("Not on server, so setting up environment...")
  
  library(faoswsModules)
  SETT <- ReadSettings("modules/pullDataToSUA/sws.yml")
  
  R_SWS_SHARE_PATH <- SETT[["share"]]  
  ## Get SWS Parameters
  SetClientFiles(dir = SETT[["certdir"]])
  GetTestEnvironment(
    baseUrl = SETT[["server"]],
    token = SETT[["token"]]
  )
}

startYear = as.numeric(swsContext.computationParams$startYear)
endYear = as.numeric(swsContext.computationParams$endYear)
geoM49 = swsContext.computationParams$geom49
stopifnot(startYear <= endYear)
yearVals = startYear:endYear

##' Get data configuration and session
sessionKey = swsContext.datasets[[1]]

sessionCountries =
  getQueryKey("geographicAreaM49", sessionKey)

geoKeys = GetCodeList(domain = "agriculture", dataset = "aproduction",
                      dimension = "geographicAreaM49")[type == "country", code]
geo=c("4", "24", "50", "68", "854", "116", "120", "140", "148", "1248", 
      "170", "178", "384", "408", "218", "231", "320", "324", "332", 
      "356", "360", "368", "404", "450", "454", "484", "508", "104", 
      "524", "562", "566", "586", "604", "608", "646", "686", "144", 
      "760", "762", "764", "800", "834", "860", "862", "704", "887", 
      "894", "716")
##' Select the countries based on the user input parameter
selectedGEOCode =
  switch(geoM49,
         "session" = sessionCountries,
         "all" = geoKeys)

################################################
##### Harvest from Agricultural Production #####
################################################

message("Pulling data from Agriculture Production")

## if the 

geoDim = Dimension(name = "geographicAreaM49", keys = geo)


eleKeys = GetCodeTree(domain = "agriculture", dataset = "aproduction",
                      dimension = "measuredElement")
## Get all children of old codes
prodKeys = strsplit(eleKeys[parent %in% c(oldProductionCode), children],
                   split = ", ")
eleKeys = "5510"
## Combine with single codes
eleDim = Dimension(name = "measuredElement", keys = eleKeys)

itemKeys = GetCodeList(domain = "agriculture", dataset = "aproduction",
                       dimension = "measuredItemCPC")[, code]
itemDim = Dimension(name = "measuredItemCPC", keys = itemKeys)
timeDim = Dimension(name = "timePointYears", keys = as.character(yearVals))
agKey = DatasetKey(domain = "agriculture", dataset = "aproduction",
                   dimensions = list(
                     geographicAreaM49 = geoDim,
                     measuredElement = eleDim,
                     measuredItemCPC = itemDim,
                     timePointYears = timeDim)
)
agData = GetData(agKey)
setnames(agData, c("measuredElement", "measuredItemCPC"),
         c("measuredElementSuaFbs", "measuredItemSuaFbs"))

################################################
#####       Harvest from Food Domain       #####
################################################
message("Pulling data from Food")
eleFoodKey=Dimension(name = "measuredElement",
                     keys = foodCode)
foodKey = DatasetKey(domain = "food", dataset = "fooddata",
                     dimensions = list(
                       geographicAreaM49 = geoDim,
                       measuredElement = eleFoodKey,
                       measuredItemCPC = itemDim,
                       timePointYears = timeDim)
)
foodData = GetData(foodKey)
setnames(foodData, c("measuredElement", "measuredItemCPC"),
         c("measuredElementSuaFbs", "measuredItemSuaFbs"))


################################################
#####       Merging data files together    #####
################################################

message("Merging data files together and saving")
newData = do.call("rbind", list(agData,foodData))
#protected data
#### CRISTINA: after havig discovered that for crops , official food values are Wrong and have to be deleted. 
# now we have to delete all the wrong values:
# THE FOLLOWING STEPS HAVE BEEN COMMENTED BECAUSE THEY SHOULD NOT BE NEEDED
# the data might have to be corrected from the questionnaires

cropsOfficialFood = c("0111","0112","0113","0115","0116","0117","01199.02","01801","01802")
newData[!geographicAreaM49%in%c("604")&measuredItemSuaFbs%in%cropsOfficialFood
     &measuredElementSuaFbs=="5141"
     ,Value:=NA]
# only for Japan, delete also Food of Rice Milled.
newData[geographicAreaM49=="392"&measuredElementSuaFbs=="5141"&measuredItemSuaFbs=="23161.02",Value:=0]
# newData <- newData[!is.na(Value),]
setnames(newData,"measuredItemSuaFbs","measuredItemFbsSua")

setnames(newData,c("Value","flagObservationStatus", "flagMethod"),c("ValueNew","flagObservationStatusNew", "flagMethodNew"))

#### The previous step has been inserted here and removed from the standardization in order
# to give to the data team the possibility to eventually add some food value for primary commodities


########################################## 
## Pull Data from sua_unbalanced

areaKeys=selectedGEOCode
elemKeys=c("5510","5141")
itemKeys = GetCodeList(domain = "suafbs", dataset = "sua_unbalanced", "measuredItemFbsSua")
itemKeys = itemKeys[, code]

key = DatasetKey(domain = "suafbs", dataset = "sua_unbalanced", dimensions = list(
  geographicAreaM49 = Dimension(name = "geographicAreaM49", keys = geo),
  measuredElementSuaFbs = Dimension(name = "measuredElementSuaFbs", keys = elemKeys),
  measuredItemFbsSua = Dimension(name = "measuredItemFbsSua", keys = itemKeys),
  timePointYears = Dimension(name = "timePointYears", keys = as.character(yearVals))
))


suaUnb = GetData(key,omitna = FALSE)

setnames(suaUnb,c("Value","flagObservationStatus", "flagMethod"),c("Value2Keep","flagObservationStatus2Keep", "flagMethod2Keep"))


data=data.table(left_join(newData,suaUnb,by=c("geographicAreaM49", "measuredElementSuaFbs", "measuredItemFbsSua", 
                        "timePointYears")))
data[,Value:=ifelse(is.na(Value2Keep),NA,
                    ValueNew)]
# 0))]


data[,Value:=ifelse(is.na(Value2Keep),NA,
                      ifelse(Value2Keep!=ValueNew,Value2Keep,
                             ValueNew))]
                             # 0))]

data[geographicAreaM49=="800"&measuredItemFbsSua=="0141"&timePointYears=="2014",flagObservationStatus2Keep:="E"]
data[geographicAreaM49=="800"&measuredItemFbsSua=="0141"&timePointYears=="2014",flagMethod2Keep:="f"]

#### Delete NEver existing Data

data=data[!(is.na(ValueNew)&is.na(Value2Keep))]


#### Create the data to save in sua_unbalanced

data[,flagObservationStatus:=ifelse(is.na(Value),NA,
                    ifelse(Value==ValueNew,flagObservationStatusNew,
                           flagObservationStatus2Keep))]

data[,flagMethod:=ifelse(is.na(Value),NA,
                         ifelse(Value==ValueNew,flagMethodNew,
                                flagMethod2Keep))]

data[,c("ValueNew","flagObservationStatusNew","flagMethodNew","Value2Keep","flagMethod2Keep","flagObservationStatus2Keep"):=NULL]


##### Wipe Sua_unbalanced
# 
# datatoClean=data.table(data.frame(newData))
# 
# datatoClean[, Value := NA_real_]
# datatoClean[, c("flagObservationStatus","flagMethod") := NA_character_]
# 
# SaveData("suafbs", "sua_unbalanced" , data = datatoClean, waitTimeout = Inf)


##### Save in Sua_unbalanced

newData=newData[!is.na(Value)]

stats = SaveData(domain = "suafbs", dataset = "sua_unbalanced", data = newData, waitTimeout = Inf)

paste0(stats$inserted, " observations written, ",
       stats$ignored, " weren't updated, ",
       stats$discarded, " had problems.")



