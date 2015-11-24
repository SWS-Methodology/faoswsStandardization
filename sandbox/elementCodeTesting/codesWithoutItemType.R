library(faosws)
library(data.table)
library(ggplot2)

SetClientFiles(dir = "~/R certificate files/QA")
GetTestEnvironment(
    ## baseUrl = "https://hqlprswsas1.hq.un.fao.org:8181/sws",
    ## token = "7b588793-8c9a-4732-b967-b941b396ce4d"
    baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
    token = "3242f11d-28b2-4429-86b0-6fab97cb50bb"
)

itemCodes = GetCodeList("agriculture", "agriculture", "measuredItemCPC")
missingType = itemCodes[is.na(type), code]
key = DatasetKey(domain = "agriculture", dataset = "agriculture", dimensions = list(
    geographicAreaM49 = Dimension(name = "geographicAreaM49",
                                  keys = GetCodeList("agriculture", "agriculture", "geographicAreaM49")[, code]),
    measuredElement = Dimension(name = "measuredElement",
                                keys = GetCodeList("agriculture", "agriculture", "measuredElement")[, code]),
    measuredItemCPC = Dimension(name = "measuredItemCPC", keys = missingType),
    timePointYears = Dimension(name = "timePointYears", keys = as.character(1950:2015))))
data = GetData(key)
data[, .N, c("measuredItemCPC", "measuredElement")]

## Add in trade
key = DatasetKey(domain = "trade", dataset = "total_trade_CPC", dimensions = list(
    geographicAreaM49 = Dimension(name = "geographicAreaM49",
                                  keys = GetCodeList("trade", "total_trade_CPC", "geographicAreaM49")[, code]),
    measuredElementTrade = Dimension(name = "measuredElementTrade",
                                keys = GetCodeList("trade", "total_trade_CPC", "measuredElementTrade")[, code]),
    measuredItemCPC = Dimension(name = "measuredItemCPC", keys = missingType),
    timePointYears = Dimension(name = "timePointYears", keys = as.character(1950:2015))))
tradeData = GetData(key)
setnames(tradeData, "measuredElementTrade", "measuredElement")

out = rbind(data[, .N, c("measuredItemCPC", "measuredElement")],
            tradeData[, .N, c("measuredItemCPC", "measuredElement")])
setnames(itemCodes, "code", "measuredItemCPC")
out = merge(out, itemCodes[, c("measuredItemCPC", "description"), with = FALSE],
            by = "measuredItemCPC")
