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
# data$key = 1:nrow(data)
# data2 = elementCodesToNames(data = data, standParams = params)
# compare = merge(data2, data, by = "key")
# head(compare[, c("measuredItemSuaFbs.x", "measuredElementSuaFbs.x", "measuredElementSuaFbs.y"), with = FALSE], 50)
data = elementCodesToNames(data = data, standParams = params)

## Update params for specific dataset
params = defaultStandardizationParameters()
params$itemVar = "measuredItemSuaFbs"
params$mergeKey[params$mergeKey == "measuredItemCPC"] = "measuredItemSuaFbs"
params$elementVar = "measuredElementSuaFbs"
params$childVar = "measuredItemChildCPC"
params$parentVar = "measuredItemParentCPC"
params$productionCode = "production"
params$importCode = "import"
params$exportCode = "export"
params$stockCode = "stockChange"
params$foodCode = "food"
params$feedCode = "feed"
params$seedCode = "seed"
params$wasteCode = "loss"
params$industrialCode = "industrial"
params$touristCode = "tourist"
params$foodProcCode = "foodManufacturing"

## Update tree by setting some edges to "F", computing average extraction rates
## when missing, and bringing in extreme extraction rates
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
data = split(data, f = data[, c("geographicAreaM49", "timePointYears"), with = FALSE])
tree = split(tree, f = tree[, c("geographicAreaM49", "timePointYearsSP"), with = FALSE])
tree = tree[names(data)]
## Remove country and year from tree (as they're no longer relevant).  Failing
## to do this will cause problems later when merging with data.
lapply(tree, function(x) x[, c("geographicAreaM49", "timePointYearsSP") := NULL])
elementGroup = read.csv(paste0(R_SWS_SHARE_PATH, "/browningj/elementCodes.csv"))
parentNodes = getCommodityLevel(tree[[1]], parentColname = "measuredItemParentCPC",
                                childColname = "measuredItemChildCPC")
parentNodes = parentNodes[level == 0, node]
standardizationVectorized = function(data, tree){
    if(nrow(data) == 0){
        warning("No rows in data, nothing to do")
        return(data)
    }
    samplePool = parentNodes[parentNodes %in% data$measuredItemSuaFbs]
    if(length(samplePool) == 0) samplePool = data$measuredItemSuaFbs
    printCodes = sample(samplePool, size = 1)
    if(!is.null(tree)){
        printCodes = getChildren(commodityTree = tree,
                                 parentColname = params$parentVar,
                                 childColname = params$childVar,
                                 topNodes = printCodes)
    }
    sink(paste0(R_SWS_SHARE_PATH, "/browningj/standardization/",
                data$timePointYears[1], "_",
                data$geographicAreaM49[1], "_sample_test.md"))
    out = try({
        standardizationWrapper(data = data, tree = tree, standParams = params,
                               printCodes = printCodes)
    })
    sink()
    if(is(out, "try-error")){
        return(NULL)
    } else {
        return(out)
    }
}

# ## Debugging
# for(i in 1:length(data)){
#     out = standardizationVectorized(data = data[[i]], tree = tree[[i]])
# }
# 
# ## Testing single case:
# year = "2012"
# country = "1249"
# code = paste(country, year, commodity, sep = ".")
# data[[code]]
# tree[[code]]

standData = mapply(standardizationVectorized, data = data, tree = tree)

paste0("Successfully built ", successCount, " models out of ",
       failCount + successCount, " commodities.")