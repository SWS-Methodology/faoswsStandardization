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

oldProductionCode = "51"
importCode = "5610"
exportCode = "5910"
oldFeedCode = "101"
oldSeedCode = "111"
#oldLossCode = "121"
lossCode = "5016"
industrialCode = "5165"
touristCode = "5164"
warning("Stocks is change in stocks, not absolute! This needs to be changed")
stocksCode = "5071"

## set up for the test environment and parameters
R_SWS_SHARE_PATH = Sys.getenv("R_SWS_SHARE_PATH")
DEBUG_MODE = Sys.getenv("R_DEBUG_MODE")

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
eleDim = Dimension(name = "measuredElement", keys = c(do.call("c", eleKeys), 
                                                      industrialCode, touristCode, lossCode, stocksCode))
           
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

## Harvest from Trade
message("Pulling data from Trade")
eleTradeDim = Dimension(name = "measuredElementTrade",
                        keys = c(importCode, exportCode))
tradeKey = DatasetKey(domain = "trade", dataset = "total_trade_cpc_m49",
                      dimensions = list(
                       geographicAreaM49 = geoDim,
                       measuredElementTrade = eleTradeDim,
                       measuredItemCPC = itemDim,
                       timePointYears = timeDim)
)
tradeData = GetData(tradeKey)
setnames(tradeData, c("measuredElementTrade", "measuredItemCPC"),
         c("measuredElementSuaFbs", "measuredItemSuaFbs"))
tradeData[, flagMethod := NA]

message("Merging data files together and saving")
out = do.call("rbind", list(agData, tradeData))

stats = SaveData(domain = "suafbs", dataset = "sua", data = out)

paste0(stats$inserted, " observations written, ",
       stats$ignored, " weren't updated, ",
       stats$discarded, " had problems.")
