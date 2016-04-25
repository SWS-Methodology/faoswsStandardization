## load the library
library(faosws)
library(faoswsUtil)
library(data.table)
library(igraph)
library(faoswsBalancing)

## set up for the test environment and parameters
R_SWS_SHARE_PATH = Sys.getenv("R_SWS_SHARE_PATH")

if(CheckDebug()){
  library(faoswsModules)
  message("Not on server, so setting up environment...")
  
  # Read settings_<server> file in working directory. See 
  # settings_<server>.example for more information
  PARAMS <- ReadSettings("sws.yml")
  
  R_SWS_SHARE_PATH = PARAMS["share"]
  apiDirectory = "./R"
  
  ## Get SWS Parameters
  SetClientFiles(dir = PARAMS["certdir"])
  GetTestEnvironment(
    baseUrl = PARAMS["server"],
    token = PARAMS["token"]
  )
  
  ## Source local scripts for this local test
  for(file in dir(apiDirectory, full.names = T))
    source(file)
} else {
  message("Running on server, no need to call GetTestEnvironment...")
  
}

#User name is what's after the slash
SWS_USER = regmatches(swsContext.username, regexpr("(?<=/).+$", swsContext.username, perl=TRUE))

message("Getting parameters/datasets...")

startYear = as.numeric(swsContext.computationParams$startYear)
endYear = as.numeric(swsContext.computationParams$endYear)
stopifnot(startYear <= endYear)
yearVals = as.character(startYear:endYear)

tree = getCommodityTree(timePointYears = yearVals)

areaKeys = GetCodeList(domain = "suafbs", dataset = "sua", "geographicAreaM49")
areaKeys = areaKeys[type == "country", code]
elemKeys = GetCodeTree(domain = "suafbs", dataset = "sua", "measuredElementSuaFbs")

#    code              description
# 1:   51                   Output
# 2:   61              Inflow (Qt)
# 3:   71 Variation Intial Exstenc
# 4:   91             Outflow (Qt)
# 5:  101     Use For Animals (Qt)
# 6:  111     Use For Same Product
# 7:  121                   Losses
# 8:  131 Reemployment Same Sector

fs_elements <- c("51", "61", "71", "91", "101", "111", "121", "131")

elemKeys = elemKeys[parent %in% fs_elements,
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

message("Reading SUA data...")

data = GetData(key)
# data$key = 1:nrow(data)
# data2 = elementCodesToNames(data = data, standParams = params)
# compare = merge(data2, data, by = "key")
# head(compare[, c("measuredItemSuaFbs.x", "measuredElementSuaFbs.x", "measuredElementSuaFbs.y"), with = FALSE], 50)

#!! 3 warnings about things that need to be changed !!#
data = elementCodesToNames(data = data, itemCol = "measuredItemSuaFbs",
                           elementCol = "measuredElementSuaFbs")

## Update params for specific dataset
params = defaultStandardizationParameters()
params$itemVar = "measuredItemSuaFbs"
params$mergeKey[params$mergeKey == "measuredItemCPC"] = "measuredItemSuaFbs"
params$elementVar = "measuredElementSuaFbs"
params$childVar = "measuredItemChildCPC"
params$parentVar = "measuredItemParentCPC"
params$productionCode = "production"
params$importCode = "imports"
params$exportCode = "exports"
params$stockCode = "stockChange"
params$foodCode = "food"
params$feedCode = "feed"
params$seedCode = "seed"
params$wasteCode = "loss"
params$industrialCode = "industrial"
params$touristCode = "tourist"
params$foodProcCode = "foodManufacturing"

message("Applying adjustments to commodity tree...")

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


itemMap = GetCodeList(domain = "agriculture", dataset = "aproduction", "measuredItemCPC")
itemMap = itemMap[, c("code", "type"), with = FALSE]
setnames(itemMap, "code", "measuredItemSuaFbs")
data = merge(data, itemMap, by = "measuredItemSuaFbs")
setnames(itemMap, "measuredItemSuaFbs", "measuredItemParentCPC")
tree = merge(tree, itemMap, by = "measuredItemParentCPC")

## Remove missing elements
data = data[!is.na(measuredElementSuaFbs), ]

message("Loading nutrient data...")

itemKeys = GetCodeList("agriculture", "aupus_ratio", "measuredItemCPC")[, code]
nutrientData = getNutritiveFactors(measuredElement = c("1001", "1003", "1005"),
                                   timePointYears = yearVals)
setnames(nutrientData, c("measuredItemCPC", "timePointYearsSP"),
         c("measuredItemSuaFbs", "timePointYears"))

message("Defining vectorized standardization function...")

standardizationVectorized = function(data, tree, nutrientData){
    if(nrow(data) == 0){
        message("No rows in data, nothing to do")
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
    dir.create(paste0(R_SWS_SHARE_PATH, "/", SWS_USER, "/standardization/"), showWarnings = FALSE)
    sink(paste0(R_SWS_SHARE_PATH, "/", SWS_USER, "/standardization/",
                data$timePointYears[1], "_",
                data$geographicAreaM49[1], "_sample_test.md"))
    # Prevent sink staying open if function is terminated prematurely (such as
    # in debugging of functions in standardizationWrapper)
    sinkOpen <- TRUE
    if(exists(sinkOpen) && sinkOpen){
      on.exit(sink())
    }
    out = try(standardizationWrapper(data = data, tree = tree,
                                 standParams = params, printCodes = printCodes,
                                 nutrientData = nutrientData))
    sink()
    sinkOpen <- FALSE
    
    return(out)
}

## Split data based on the two factors we need to loop over
setnames(tree, "timePointYearsSP", "timePointYears")
uniqueLevels = data[, .N, by = c("geographicAreaM49", "timePointYears")]
uniqueLevels[, N := NULL]
elementGroup = read.csv(paste0(R_SWS_SHARE_PATH, "/browningj/elementCodes.csv"))

parentNodes = getCommodityLevel(tree, parentColname = "measuredItemParentCPC",
                                childColname = "measuredItemChildCPC")
parentNodes = parentNodes[level == 0, node]

message("Beginning actual standardization process...")

aggFun = function(x){
    if(length(x) > 1)
        stop("x should only be one value!")
    return(sum(x))
}

standData = vector(mode = "list", length = nrow(uniqueLevels))

for(i in 1:nrow(uniqueLevels)){
    filter = uniqueLevels[i, ]
    dataSubset = data[filter, , on = c("geographicAreaM49", "timePointYears")]
    treeSubset = tree[filter, , on = c("geographicAreaM49", "timePointYears")]
    # dataSubset[, c("geographicAreaM49", "timePointYears") := NULL]
    treeSubset[, c("geographicAreaM49", "timePointYears") := NULL]
    subNutrientData = nutrientData[filter, , on = c("geographicAreaM49",
                                                    "timePointYears")]
    subNutrientData = dcast(measuredItemSuaFbs ~ measuredElement,
                            data = subNutrientData, value.var = "Value",
                            fun.agg = aggFun)
    setnames(subNutrientData, c("1001", "1003", "1005"),
             c("Calories", "Proteins", "Fats"))
    standData[[i]] = standardizationVectorized(data = dataSubset,
                                               tree = treeSubset,
                                               nutrientData = subNutrientData)
    if(!is(standData[[i]], "try-error")){
        standData[[i]] = standData[[i]][measuredItemSuaFbs %in% parentNodes, ]
    }
}

message("Combining standardized data...")

filter = sapply(standData, function(x){is(x, "try-error")})
errorMessages = sapply(standData[filter], function(x) attr(x, "condition")$message)
cat("Error messages:", errorMessages)
cat(is(standData), "\n")
standData = do.call("rbind", standData[!filter])
cat(is(standData), "\n")
standData[, type := NULL] # Must remove for next function to work ok
cat(is(standData), "\n")
standData = elementNamesToCodes(data = standData,
                                elementCol = "measuredElementSuaFbs",
                                itemCol = "measuredItemSuaFbs")
cat(is(standData), "\n")
standData[, standardDeviation := NULL]
cat(is(standData), "\n")
## Assign flags: I for imputed (as we're estimating/standardizing) and s for
## "sum" (aggregate)
standData[, flagObservationStatus := "I"]
standData[, flagMethod := "s"]

message("Attempting to save standardized data...")

out = SaveData(domain = "suafbs", dataset = "fbs", data = standData)
cat(out$inserted, " observations written and problems with ",
    out$ignored + out$discarded, sep = "")
paste0(out$inserted, " observations written and problems with ",
       out$ignored + out$discarded)
