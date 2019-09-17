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
library(dtplyr)
library(tidyr)

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
stocksCode = c("5113", "5071") # 5113 = opening, 5071 = variation

## set up for the test environment and parameters
R_SWS_SHARE_PATH = Sys.getenv("R_SWS_SHARE_PATH")

if(CheckDebug()){
  message("Not on server, so setting up environment...")
  
  library(faoswsModules)
  SETT <- ReadSettings("modules/pullDataToSUA/sws.yml")
  
  #R_SWS_SHARE_PATH <- SETT[["share"]]  
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

geoDim = Dimension(name = "geographicAreaM49", keys = selectedGEOCode)


eleKeys = GetCodeTree(domain = "agriculture", dataset = "aproduction",
                      dimension = "measuredElement")
## Get all children of old codes
eleKeys = strsplit(eleKeys[parent %in% c(oldProductionCode, oldFeedCode,
                                         oldSeedCode), children],
                   split = ", ")
## Combine with single codes
eleDim = Dimension(name = "measuredElement", keys = c(do.call("c", eleKeys)
                                                      # ,industrialCode
))
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
#####        Harvest from Industrial       #####
################################################

################################################################################################################################

# temporary solution til codes will be updated


# message("Pulling data from industrial domain")      
# indEleDim = Dimension(name = "measuredElement",
#                       keys = industrialCode)
# 
# indKey = DatasetKey(domain = "industrialUse", dataset = "industrialusedata",
#                     dimensions = list(
#                       geographicAreaM49 = geoDim,
#                       measuredElement = indEleDim,
#                       measuredItemCPC = itemDim,
#                       timePointYears = timeDim)
# )
# indData = GetData(indKey)
# setnames(indData, c("measuredElement", "measuredItemCPC"),
#          c("measuredElementSuaFbs", "measuredItemSuaFbs"))



#We decided to pull industrial data not from Industrial Use domain, instead from  production and agriculture  domain. 

################################################################################################################################


message("Pulling data from industrial domain")
indEleDim = Dimension(name = "measuredElement",
                      keys = industrialCode)

geoDim = Dimension(name = "geographicAreaM49", keys = selectedGEOCode)

indKey = DatasetKey(domain = "agriculture", dataset = "aproduction",
                    dimensions = list(
                      geographicAreaM49 = geoDim,
                      measuredElement = indEleDim,
                      measuredItemCPC = itemDim,
                      timePointYears = timeDim)
)
indData = GetData(indKey)
setnames(indData, c("measuredElement", "measuredItemCPC"),
         c("measuredElementSuaFbs", "measuredItemSuaFbs"))




################################################
#####        Harvest from stockdata        #####
################################################

message("Pulling data from Stock domain")
stockEleDim = Dimension(name = "measuredElement",
                        keys = stocksCode)

stokKey = DatasetKey(domain = "Stock", dataset = "stocksdata",
                     dimensions = list(
                       geographicAreaM49 = geoDim,
                       measuredElement = stockEleDim,
                       measuredItemCPC = itemDim,
                       timePointYears = timeDim)
)
stockData = GetData(stokKey)
setnames(stockData, c("measuredElement", "measuredItemCPC"),
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
#####       Harvest from loss Domain       #####
################################################
message("Pulling data from Loss")
eleLossKey=Dimension(name = "measuredElementSuaFbs",
                     keys = lossCode)
itemLossKey = GetCodeList(domain = "lossWaste", dataset = "loss",
                          dimension = "measuredItemSuaFbs")[, code]
itemLossDim = Dimension(name = "measuredItemSuaFbs", keys = itemLossKey)
lossKey = DatasetKey(domain = "lossWaste", dataset = "loss",
                     dimensions = list(
                       geographicAreaM49 = geoDim,
                       measuredElement = eleLossKey,
                       measuredItemCPC = itemLossDim,
                       timePointYears = timeDim)
)
lossData = GetData(lossKey)

################################################
#####      Harvest from Tourism Domain     #####
################################################
TourGeoKeys = c("28",
                "44",
                "52",
                "84",
                "72",
                "116",
                "132",
                "344",
                "446",
                "174",
                "188",
                "192",
                "196",
                "212",
                "214",
                "818",
                "242",
                "258",
                "308",
                "320",
                "340",
                "352",
                "360",
                "388",
                "404",
                "296",
                "418",
                "462",
                "480",
                "524",
                "540",
                "659",
                "662",
                "882",
                "678",
                "690",
                "90",
                "670",
                "780",
                "548")

TourGeoKeys = TourGeoKeys[TourGeoKeys==selectedGEOCode]

TourGeoDim = Dimension(name = "geographicAreaM49", TourGeoKeys)

message("Pulling data from Tourist")
eleTourDim = Dimension(name = "tourismElement",
                       keys = touristCode)
tourKey = DatasetKey(domain = "tourism", dataset = "tourismprod",
                     dimensions = list(
                       geographicAreaM49 = TourGeoDim,
                       tourismElement = eleTourDim,
                       measuredItemCPC = itemDim,
                       timePointYears = timeDim)
)
tourData = GetData(tourKey)
tourData[, `:=`(tourismElement = suaTouristCode,
                Value = Value * touristConversionFactor)]
setnames(tourData, c("tourismElement", "measuredItemCPC"),
         c("measuredElementSuaFbs", "measuredItemSuaFbs"))


################################################
#####       Harvest from Trade Domain      #####
################################################
# Before old data until 2013 were copied in the total trade dataset
# Data had to be taken from 2 different sources
# These lines are now hided because the total trade data are all on 1 dataset.
# TRADE HAS TO BE PULLED:
# - FROM OLD FAOSTAT UNTIL 2013
# - FROM NEW DATA STARTING FROM 2010
################################################

# message("Pulling data from Trade UNTIL 2013 (old FAOSTAT)")
# 
# eleTradeDim = Dimension(name = "measuredElementTrade",
#                         keys = c(importCode, exportCode))
# tradeItems <- na.omit(sub("^0+", "", cpc2fcl(unique(itemKeys), returnFirst = TRUE, version = "latest")), waitTimeout = 2000000)
# 
# geoKeysTrade=m492fs(selectedGEOCode)
# 
# geokeysTrade=geoKeysTrade[!is.na(geoKeysTrade)]
# 
# if(2013>=endYear){
#   timeTradeDimUp13 = Dimension(name = "timePointYears", keys = as.character(yearVals))
#   
#   ###### Trade UNTIL 2013 (old FAOSTAT)
#   message("Trade UNTIL 2013 (old FAOSTAT)")
#   tradeKeyUp13 = DatasetKey(
#     domain = "faostat_one", dataset = "updated_sua",
#     dimensions = list(
#       #user input except curacao,  saint martin and former germany
#       geographicAreaFS= Dimension(name = "geographicAreaFS", keys = setdiff(geokeysTrade, c("279", "534", "280","274","283"))),
#       measuredItemFS=Dimension(name = "measuredItemFS", keys = tradeItems),
#       measuredElementFS=Dimension(name = "measuredElementFS",
#                                   keys = c( "61", "91")),
#       timePointYears = timeTradeDimUp13 ),
#     sessionId =  slot(swsContext.datasets[[1]], "sessionId")
#   )
#   
#   
#   tradeDataUp13 = GetData(tradeKeyUp13)
#   
#   
#   tradeDataUp13[, `:=`(geographicAreaFS = fs2m49(geographicAreaFS),
#                        measuredItemFS = fcl2cpc(sprintf("%04d", as.numeric(measuredItemFS)),
#                                                 version = "latest"))]
#   
#   
#   setnames(tradeDataUp13, c("geographicAreaFS","measuredItemFS","measuredElementFS","flagFaostat" ),
#            c("geographicAreaM49", "measuredItemSuaFbs","measuredElementSuaFbs","flagObservationStatus"))
#   
#   tradeDataUp13[, flagMethod := "-"]
#   
#   tradeDataUp13[flagObservationStatus %in% c("P", "*", "X"), flagObservationStatus := "T"]
#   tradeDataUp13[flagObservationStatus %in% c("T", "F"), flagObservationStatus := "E"]
#   tradeDataUp13[flagObservationStatus %in% c("B", "C", "E"), flagObservationStatus := "I"]
#   
#   tradeDataUp13[measuredElementSuaFbs=="91",measuredElementSuaFbs:="5910"]
#   tradeDataUp13[measuredElementSuaFbs=="61",measuredElementSuaFbs:="5610"]
#   
#   tradeData=tradeDataUp13 
#   
# }else{
#   ###### Trade FROM 2014 (new Data)
#   message("Trade FROM 2014 (new Data)")
#   
#   timeTradeDimFrom14 = Dimension(name = "timePointYears", keys = as.character(2014:endYear))
#   
#   tradeKeyFrom14 = DatasetKey(
#     domain = "trade", dataset = "total_trade_cpc_m49",
#     dimensions = list(geographicAreaM49 = geoDim,
#                       measuredElementTrade = eleTradeDim,
#                       measuredItemCPC = itemDim,
#                       timePointYears = timeTradeDimFrom14)
#   )
#   tradeDataFrom14 = GetData(tradeKeyFrom14)
#   setnames(tradeDataFrom14, c("measuredElementTrade", "measuredItemCPC"),
#            c("measuredElementSuaFbs", "measuredItemSuaFbs"))
#   
#   ###### Merging Trade Data
#   message("Merging Data")
#   if(2013<startYear){
#     tradeData=tradeDataFrom14
#   }else{
#     timeTradeDimUp13 = Dimension(name = "timePointYears", keys = as.character(startYear:2013))
#     message("Trade UNTIL 2013 (old FAOSTAT)")
#     tradeKeyUp13 = DatasetKey(
#       domain = "faostat_one", dataset = "updated_sua",
#       dimensions = list(
#         #user input except curacao,  saint martin and former germany
#         geographicAreaFS= Dimension(name = "geographicAreaFS", keys = setdiff(geokeysTrade, c("279", "534", "280","274","283"))),
#         measuredItemFS=Dimension(name = "measuredItemFS", keys = tradeItems),
#         measuredElementFS=Dimension(name = "measuredElementFS",
#                                     keys = c( "61", "91")),
#         timePointYears = timeTradeDimUp13 ),
#       sessionId =  slot(swsContext.datasets[[1]], "sessionId")
#     )
#     
#     
#     tradeDataUp13 = GetData(tradeKeyUp13)
#     
#     
#     tradeDataUp13[, `:=`(geographicAreaFS = fs2m49(geographicAreaFS),
#                          measuredItemFS = fcl2cpc(sprintf("%04d", as.numeric(measuredItemFS)),
#                                                   version = "latest"))]
#     
#     
#     setnames(tradeDataUp13, c("geographicAreaFS","measuredItemFS","measuredElementFS","flagFaostat" ),
#              c("geographicAreaM49", "measuredItemSuaFbs","measuredElementSuaFbs","flagObservationStatus"))
#     
#     tradeDataUp13[, flagMethod := "-"]
#     
#     tradeDataUp13[flagObservationStatus %in% c("P", "*", "X"), flagObservationStatus := "T"]
#     tradeDataUp13[flagObservationStatus %in% c("T", "F"), flagObservationStatus := "E"]
#     tradeDataUp13[flagObservationStatus %in% c("B", "C", "E"), flagObservationStatus := "I"]
#     
#     tradeDataUp13[measuredElementSuaFbs=="91",measuredElementSuaFbs:="5910"]
#     tradeDataUp13[measuredElementSuaFbs=="61",measuredElementSuaFbs:="5610"]
#     
#     tradeData=rbind(tradeDataUp13,tradeDataFrom14)  
#     
#   }
#   
# }

### TRADE DATA FROM SINGLE SOURCE

message("Pulling data from Trade")

eleTradeDim = Dimension(name = "measuredElementTrade",
                        keys = c(importCode, exportCode))
#tradeItems <- na.omit(sub("^0+", "", cpc2fcl(unique(itemKeys), returnFirst = TRUE, version = "latest")), waitTimeout = 2000000)

timeTradeDim = Dimension(name = "timePointYears", keys = as.character(yearVals))

tradeKey = DatasetKey(
  domain = "trade", dataset = "total_trade_cpc_m49",
  dimensions = list(geographicAreaM49 = geoDim,
                    measuredElementTrade = eleTradeDim,
                    measuredItemCPC = itemDim,
                    timePointYears = timeTradeDim)
)
tradeData = GetData(tradeKey)
setnames(tradeData, c("measuredElementTrade", "measuredItemCPC"),
         c("measuredElementSuaFbs", "measuredItemSuaFbs"))

################################################
#####       Merging data files together    #####
################################################

message("Merging data files together and saving")
if((nrow(indData)>0)&(nrow(tourData>0))){
Tourism_Industrial = rbind(tourData,indData)
Tourism_Industrial = as.data.frame(Tourism_Industrial)
Tourism_Industrial = dplyr::select(Tourism_Industrial,-flagObservationStatus,-flagMethod)
Tourism_Industrial = tidyr::spread(Tourism_Industrial,measuredElementSuaFbs,Value)
Tourism_Industrial$`5164`[is.na(Tourism_Industrial$`5164`)] = 0
Tourism_Industrial = Tourism_Industrial %>%
  rowwise() %>%
  mutate(`5165` = `5165` - `5164`) %>%
  ungroup()
Tourism_Industrial$`5165`[Tourism_Industrial$`5165`<0&!is.na(Tourism_Industrial$`5165`)] = 0
Tourism_Industrial = tidyr::gather(Tourism_Industrial,measuredElementSuaFbs,Value,-c(geographicAreaM49,
                                                                              measuredItemSuaFbs,
                                                                              timePointYears))
Industrial = dplyr::filter(Tourism_Industrial,measuredElementSuaFbs=="5165")
Industrial = as.data.table(Industrial)
Industrial$flagObservationStatus = "I"
Industrial$flagMethod = "e"
out = rbind(agData, stockData,foodData, lossData, tradeData, tourData,Industrial)
}
if((nrow(indData)==0)|(nrow(tourData==0))){
out = rbind(agData, stockData,foodData, lossData, tradeData, tourData,indData)
}

# NOTE: on 20190911 the removal of items below was commented out after
# discussion with TF about cases where food for important items was
# missing (e.g., Maize in Brazil)
###   #protected data
###   #### CRISTINA: after havig discovered that for crops , official food values are Wrong and have to be deleted. 
###   # now we have to delete all the wrong values:
###   # THE FOLLOWING STEPS HAVE BEEN COMMENTED BECAUSE THEY SHOULD NOT BE NEEDED
###   # the data might have to be corrected from the questionnaires
###   
###   cropsOfficialFood = c("0111","0112","0113","0115","0116","0117","01199.02","01801","01802")
###   out[!geographicAreaM49%in%c("604")&measuredItemSuaFbs%in%cropsOfficialFood
###        &measuredElementSuaFbs=="5141"
###        ,Value:=NA]

# only for Japan, delete also Food of Rice Milled.
out[geographicAreaM49=="392"&measuredElementSuaFbs=="5141"&measuredItemSuaFbs=="23161.02",Value:=0]

#### The previous step has been inserted here and removed from the standardization in order
# to give to the data team the possibility to eventually add some food value for primary commodities

out <- out[!is.na(Value),]
setnames(out,"measuredItemSuaFbs","measuredItemFbsSua")

# Wipe cells stored on SWS that were not pulled (non existing cells)

key_unb <-
  DatasetKey(
    domain = "suafbs",
    dataset = "sua_unbalanced",
    dimensions =
      list(
        geographicAreaM49 =
          Dimension(name = "geographicAreaM49",
                    keys = unique(out$geographicAreaM49)),
        measuredElementSuaFbs =
          Dimension(name = "measuredElementSuaFbs",
                    keys = GetCodeList(domain = "suafbs", dataset = "sua_unbalanced", 'measuredElementSuaFbs')$code),
        measuredItemFbsSua =
          Dimension(name = "measuredItemFbsSua",
                    keys = GetCodeList(domain = "suafbs", dataset = "sua_unbalanced", 'measuredItemFbsSua')$code),
        timePointYears =
          Dimension(name = "timePointYears",
                    keys = unique(out$timePointYears))
      )
  )

data_suaunbal <- GetData(key_unb)

non_existing <-
  data_suaunbal[!out, on = c('geographicAreaM49', 'measuredElementSuaFbs', 'measuredItemFbsSua', 'timePointYears')]

non_existing[, `:=`(Value = NA_real_, flagObservationStatus = NA_character_, flagMethod = NA_character_)]

if (nrow(non_existing) > 0) {
  message(paste("PullData: there were", nrow(non_existing), "non existing observations"))
  out <- rbind(out, non_existing)
}

# / Wipe cells


stats = SaveData(domain = "suafbs", dataset = "sua_unbalanced", data = out, waitTimeout = 2000000)

paste0(stats$inserted, " observations written, ",
       stats$ignored, " weren't updated, ",
       stats$discarded, " had problems.")


################################################################
#####  send Email with notification of correct execution   #####
################################################################

## Initiate email
from = "sws@fao.org"
to = swsContext.userEmail
subject = "PullDataToSua plug-in has correctly run"
body = "The plug-in has saved the SUAs in your session"

sendmailR::sendmail(from = from, to = to, subject = subject, msg = body)
paste0("Email sent to ", swsContext.userEmail)

