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

oldProductionCode = "51"
importCode = "5600"
exportCode = "5900"
oldFeedCode = "101"
oldSeedCode = "111"
oldLossCode = "121"
industrialCode = "???"
touristCode = "100"

## set up for the test environment and parameters
R_SWS_SHARE_PATH = Sys.getenv("R_SWS_SHARE_PATH")
DEBUG_MODE = Sys.getenv("R_DEBUG_MODE")

if(!exists("DEBUG_MODE") || DEBUG_MODE == ""){
    cat("Not on server, so setting up environment...\n")

    ## Get SWS Parameters
    SetClientFiles(dir = "~/R certificate files/QA")
    GetTestEnvironment(
        ## baseUrl = "https://hqlprswsas1.hq.un.fao.org:8181/sws",
        ## token = "7b588793-8c9a-4732-b967-b941b396ce4d"
        baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
        token = "1b6bc897-b002-4c0b-af4f-12bd000f3b2c"
    )
}

startYear = as.numeric(swsContext.computationParams$startYear)
endYear = as.numeric(swsContext.computationParams$endYear)
stopifnot(startYear <= endYear)
yearVals = startYear:endYear

## Harvest from Agricultural Production
cat("Pulling data from Agriculture Production")
geoKeys = GetCodeList(domain = "agriculture", dataset = "agriculture",
                      dimension = "geographicAreaM49")[type == "country", code]
geoDim = Dimension(name = "geographicAreaM49", keys = geoKeys)
eleKeys = GetCodeTree(domain = "agriculture", dataset = "agriculture",
                      dimension = "measuredElement")
eleKeys = strsplit(eleKeys[parent %in% c(oldProductionCode, oldFeedCode,
                                         oldSeedCode, oldLossCode), children],
                   split = ", ")
eleDim = Dimension(name = "measuredElement", keys = do.call("c", eleKeys))
itemKeys = GetCodeList(domain = "agriculture", dataset = "agriculture",
                      dimension = "measuredItemCPC")[, code]
itemDim = Dimension(name = "measuredItemCPC", keys = itemKeys)
timeDim = Dimension(name = "timePointYears", keys = as.character(yearVals))
agKey = DatasetKey(domain = "agriculture", dataset = "agriculture",
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
cat("Pulling data from Trade")
eleTradeDim = Dimension(name = "measuredElementTrade",
                        keys = c(importCode, exportCode))
tradeKey = DatasetKey(domain = "trade", dataset = "total_trade_CPC",
                      dimensions = list(
                       geographicAreaM49 = geoDim,
                       measuredElementTrade = eleTradeDim,
                       measuredItemCPC = itemDim,
                       timePointYears = timeDim)
)
tradeData = GetData(tradeKey)
setnames(tradeData, c("measuredElementTrade", "measuredItemCPC", "flagTrade"),
         c("measuredElementSuaFbs", "measuredItemSuaFbs", "flagObservationStatus"))
tradeData[, flagMethod := NA]

## Harvest from Tourist
cat("Pulling data from Tourist")
eleTourDim = Dimension(name = "tourismElement",
                        keys = touristCode)
itemDim@keys = "0111"
geoDim@keys = geoDim@keys[1]
tourKey = DatasetKey(domain = "tourism", dataset = "tourismprod",
                      dimensions = list(
                       geographicAreaM49 = geoDim,
                       tourismElement = eleTourDim,
                       measuredItemCPC = itemDim,
                       timePointYears = timeDim)
)
tourData = GetData(tourKey)
setnames(tourData, c("tourismElement", "measuredItemCPC"),
         c("measuredElementSuaFbs", "measuredItemSuaFbs"))

cat("Merging data files together and saving")
out = do.call("rbind", list(agData, tradeData, tourData))

stats = SaveData(domain = "suafbs", dataset = "sua", data = out)

paste0(stats$inserted, " observations written, ",
       stats$ignored, " weren't updated, ",
       stats$discarded, " had problems.")