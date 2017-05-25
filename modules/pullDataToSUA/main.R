## This module is designed to harvest the data from other tables and pull all
## relevant FBS data into the SUA/FBS domain.  It pulls from the following
## tables:
## 
## - Agriculture Production (production, stock, food, feed, seed, loss, industrial)
## - Trade (trade)
## - Tourist (tourist)
## 

## load the library
library(faosws)
library(data.table)
library(faoswsUtil)

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
touristConversionFactor = -1/1000
warning("Stocks is change in stocks, not absolute! This needs to be changed")
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
    ## baseUrl = "https://hqlprswsas1.hq.un.fao.org:8181/sws",
    ## token = "7b588793-8c9a-4732-b967-b941b396ce4d"
    baseUrl = SETT[["server"]],
    token = SETT[["token"]]
  )
}

startYear = as.numeric(swsContext.computationParams$startYear)
endYear = as.numeric(swsContext.computationParams$endYear)
stopifnot(startYear <= endYear)
yearVals = startYear:endYear





## Harvest from Agricultural Production
message("Pulling data from Agriculture Production")
geoKeys = GetCodeList(domain = "agriculture", dataset = "aproduction",
                      dimension = "geographicAreaM49")[type == "country", code]
geoDim = Dimension(name = "geographicAreaM49", keys = geoKeys)
eleKeys = GetCodeTree(domain = "agriculture", dataset = "aproduction",
                      dimension = "measuredElement")
## Get all children of old codes
eleKeys = strsplit(eleKeys[parent %in% c(oldProductionCode, oldFeedCode,
                                         oldSeedCode), children],
                   split = ", ")
## Combine with single codes

### CRistina deleted the food and loss codes
# eleDim = Dimension(name = "measuredElement", keys = c(do.call("c", eleKeys),
#                                                       foodCode, industrialCode, lossCode, stocksCode))


eleDim = Dimension(name = "measuredElement", keys = c(do.call("c", eleKeys),
                                                      industrialCode, stocksCode))


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





## Harvest from Food Domain
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



## Harvest from Loss Domain
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
# setnames(lossData, c("measuredElement", "measuredItemCPC"),
#          c("measuredElementSuaFbs", "measuredItemSuaFbs"))








## Harvest from Tourist
message("Pulling data from Tourist")
eleTourDim = Dimension(name = "tourismElement",
                       keys = touristCode)
tourKey = DatasetKey(domain = "tourism", dataset = "tourismprod",
                     dimensions = list(
                       geographicAreaM49 = geoDim,
                       tourismElement = eleTourDim,
                       measuredItemCPC = itemDim,
                       timePointYears = timeDim)
)
tourData = GetData(tourKey)
tourData[, `:=`(tourismElement = suaTouristCode,
                Value = Value * touristConversionFactor)]
setnames(tourData, c("tourismElement", "measuredItemCPC"),
         c("measuredElementSuaFbs", "measuredItemSuaFbs"))





## Harvest from Trade: code already present in the repository to pull data fro the current trade domain
## message("Pulling data from Trade")
## eleTradeDim = Dimension(name = "measuredElementTrade",
##                         keys = c(importCode, exportCode))
## tradeKey = DatasetKey(domain = "trade", dataset = "total_trade_cpc_m49",
##                       dimensions = list(
##                         geographicAreaM49 = geoDim,
##                         measuredElementTrade = eleTradeDim,
##                         measuredItemCPC = itemDim,
##                         timePointYears = timeDim)
## )
## tradeData = GetData(tradeKey)
## setnames(tradeData, c("measuredElementTrade", "measuredItemCPC"),
##          c("measuredElementSuaFbs", "measuredItemSuaFbs"))
## tradeData[, flagMethod := NA]
## 
## message("Merging data files together and saving")
## out = do.call("rbind", list(agData, tradeData, tourData))




#### CRISTINA MODIFIED 23/05/2017
## Harvest from Trade: old FAOSTAT data
##message("Pulling data from Trade")

##eleTradeDim = Dimension(name = "measuredElementTrade",
##                        keys = c(importCode, exportCode))

tradeItems <- na.omit(sub("^0+", "", cpc2fcl(unique(itemKeys), returnFirst = TRUE, version = "latest")))

geoKeysTrade=m492fs(geoKeys)

geokeysTrade=geoKeysTrade[!is.na(geoKeysTrade)]

tradeKey = DatasetKey(
 domain = "faostat_one", dataset = "updated_sua",
 dimensions = list(
   #user input except curacao,  saint martin and former germany
   geographicAreaFS= Dimension(name = "geographicAreaFS", keys = setdiff(geokeysTrade, c("279", "534", "280","274","283"))),
   measuredItemFS=Dimension(name = "measuredItemFS", keys = tradeItems),
   measuredElementFS=Dimension(name = "measuredElementFS",
             keys = c( "61", "91")),
   timePointYears = timeDim ),
 sessionId =  slot(swsContext.datasets[[1]], "sessionId")
)


tradeData = GetData(tradeKey)


tradeData[, `:=`(geographicAreaFS = fs2m49(geographicAreaFS),
                measuredItemFS = fcl2cpc(sprintf("%04d", as.numeric(measuredItemFS)),
                                         version = "latest"))]


setnames(tradeData, c("geographicAreaFS","measuredItemFS","measuredElementFS","flagFaostat" ),
        c("geographicAreaM49", "measuredItemSuaFbs","measuredElementSuaFbs","flagObservationStatus"))

tradeData[, flagMethod := "-"]

tradeData[flagObservationStatus %in% c("P", "*", "F"), flagObservationStatus := "T"]




tradeData[measuredElementSuaFbs=="91",measuredElementSuaFbs:="5910"]
tradeData[measuredElementSuaFbs=="61",measuredElementSuaFbs:="5610"]
#### End import FAOSTAT DATA for TRADE (CRISTINA)







GetCodeList2 <- function(dimension = NA) {
  GetCodeList(domain='trade', dataset='total_trade_cpc_m49', dimension = dimension)
}


Vars <- list(reporters = 'geographicAreaM49',
             items     = 'measuredItemCPC',
             elements  = 'measuredElementTrade',
             years     = 'timePointYears')

Keys <- list(reporters = GetCodeList2(dimension = Vars[['reporters']])[type=='country', code],
             items     = GetCodeList2(dimension = Vars[['items']])[, code],
             # Quantity [#], Quantity [head], Quantity [1000 head], Quantity [t], Value [1000 $]
             elements  = c('5607', '5608', '5609', '5610', '5907', '5908', '5909', '5910'),
             years     = as.character(2000:2013))

key <- DatasetKey(domain = 'trade',
                  dataset = 'total_trade_cpc_m49',
                  dimensions = list(
                    Dimension(name = Vars[['reporters']], keys = Keys[['reporters']]),
                    Dimension(name = Vars[['items']],     keys = Keys[['items']]),
                    Dimension(name = Vars[['elements']],  keys = Keys[['elements']]),
                    Dimension(name = Vars[['years']],     keys = Keys[['years']])))


tradeData <- GetData(key = key)

setnames(tradeData, c("measuredElementTrade", "measuredItemCPC"),
         c("measuredElementSuaFbs", "measuredItemSuaFbs"))










message("Merging data files together and saving")
out = do.call("rbind", list(agData,foodData, lossData, tradeData, tourData))






out <- out[!is.na(Value),]
setnames(out,"measuredItemSuaFbs","measuredItemFbsSua")


# save data with TRAde FROM faostat
# data = elementCodesToNames(data = out, itemCol = "measuredItemFbsSua",
#                            elementCol = "measuredElementSuaFbs")
# 
# setnames(data,"measuredItemFbsSua","measuredItemSuaFbs")
# 
# save(data,file="C:/Users/muschitiello/Documents/StandardizationFrancescaCristina/SupportFiles_Standardization/dataTradeFAOSTAT.RData")



stats = SaveData(domain = "suafbs", dataset = "sua_unbalanced", data = out, waitTimeout = 20000)

paste0(stats$inserted, " observations written, ",
       stats$ignored, " weren't updated, ",
       stats$discarded, " had problems.")

