## load the library
library(faosws)
library(faoswsUtil)
library(data.table)
library(igraph)
library(faoswsBalancing)

## set up for the test environment and parameters
R_SWS_SHARE_PATH = Sys.getenv("R_SWS_SHARE_PATH")
DEBUG_MODE = Sys.getenv("R_DEBUG_MODE")

if(!exists("DEBUG_MODE") || DEBUG_MODE == ""){
    cat("Not on server, so setting up environment...\n")

    R_SWS_SHARE_PATH = "/media/hqlprsws1_qa/"
    apiDirectory = "~/Documents/Github/faoswsStandardization/R/"
    
    ## Get SWS Parameters
    SetClientFiles(dir = "~/R certificate files/QA")
    GetTestEnvironment(
        ## baseUrl = "https://hqlprswsas1.hq.un.fao.org:8181/sws",
        ## token = "7b588793-8c9a-4732-b967-b941b396ce4d"
        baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
        token = "0297f8b1-62ed-4d6a-a07e-bd1bacc6e615"
    )

    ## Source local scripts for this local test
    for(file in dir(apiDirectory, full.names = T))
        source(file)
}

startYear = as.numeric(swsContext.computationParams$startYear)
endYear = as.numeric(swsContext.computationParams$endYear)
stopifnot(startYear <= endYear)
yearVals = as.character(startYear:endYear)

tree = getCommodityTree(timePointYears = yearVals)

areaKeys = GetCodeList(domain = "suafbs", dataset = "sua", "geographicAreaM49")
areaKeys = areaKeys[type == "country", code]
elemKeys = GetCodeTree(domain = "suafbs", dataset = "sua", "measuredElementSuaFbs")
elemKeys = elemKeys[parent %in% c("51", "61", "71", "91", "101", "111", "121", "131"),
                    paste0(children, collapse = ", ")]
elemKeys = strsplit(elemKeys, ", ")[[1]]
itemKeys = GetCodeList(domain = "suafbs", dataset = "sua", "measuredItemSuaFbs")
itemKeys = itemKeys[, code]
key = DatasetKey(domain = "suafbs", dataset = "sua", dimensions = list(
    geographicAreaM49 = Dimension(name = "geographicAreaM49", keys = areaKeys),
    measuredElementSuaFbs = Dimension(name = "measuredElementSuaFbs", keys = elemKeys),
    measuredItemSuaFbs = Dimension(name = "measuredItemSuaFbs", keys = itemKeys),
    timePointYears = Dimension(name = "timePointYears", keys = yearVals)
))
data = GetData(key)

params = defaultStandardizationParameters()
params$itemVar = "measuredItemSuaFbs"
params$mergeKey[params$mergeKey == "measuredItemCPC"] = "measuredItemSuaFbs"
params$elementVar = "measuredElementSuaFbs"
params$childVar = "measuredItemChildCPC"
params$parentVar = "measuredItemParentCPC"
tree[, target := ifelse(measuredItemParentCPC %in% c("23511.01", "23512",
                                                     "01499.06", "01921.01"),
                        "F", "B")]
tree = adjustCommodityTree(tree, parentColname = "measuredItemParentCPC",
                           childColname = "measuredItemChildCPC", nSigma = 2)
tree[, extractionRate := ifelse(is.na(extractionRate),
                                mean(extractionRate, na.rm = TRUE),
                                extractionRate),
     by = c("measuredItemParentCPC", "measuredItemChildCPC")]
tree[is.na(extractionRate), extractionRate := 1]

itemMap = GetCodeList(domain = "agriculture", dataset = "agriculture", "measuredItemCPC")
itemMap = itemMap[, c("code", "type"), with = FALSE]
setnames(itemMap, "code", "measuredItemSuaFbs")
data = merge(data, itemMap, by = "measuredItemSuaFbs")
setnames(itemMap, "measuredItemSuaFbs", "measuredItemParentCPC")
tree = merge(tree, itemMap, by = "measuredItemParentCPC")

## Split data based on the three factors we need to loop over
data = split(data, f = data[, c("geographicAreaM49",
                                "timePointYears", "type"), with = FALSE])
tree = split(tree, f = tree[, c("geographicAreaM49",
                                "timePointYearsSP", "type"), with = FALSE])
tree = tree[names(data)]
elementGroup = fread(paste0(R_SWS_SHARE_PATH, "/browningj/elementCodes.csv"))
standardizationVectorized = function(data, tree){
    params = 
    printCodes = 
    out = try({
        standardizationWrapper(data = data, tree = tree, standParams = params,
                               printCodes = printCodes)
    })
    if(is(out, "try-error")){
        return(NULL)
    } else {
        return(out)
    }
}
standData = mapply(standardizationVectorized, data = data, tree = tree)

paste0("Successfully built ", successCount, " models out of ",
       failCount + successCount, " commodities.")