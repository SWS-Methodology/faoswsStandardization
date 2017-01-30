## load the library
library(faosws)
library(faoswsUtil)
library(data.table)
library(igraph)
library(faoswsBalancing)
library(faoswsStandardization)
library(dplyr)

if(packageVersion("faoswsStandardization") < package_version('0.1.0')){
  stop("faoswsStandardization is out of date")
}

if (CheckDebug()) {
  library(faoswsModules)
  message("Not on server, so setting up environment...")
  
  # Read settings file sws.yml in working directory. See 
  # sws.yml.example for more information
  PARAMS <- ReadSettings("modules/standardizationHHBS/sws.yml")
  message("Connecting to server: ", PARAMS[["current"]])
  
  R_SWS_SHARE_PATH = PARAMS[["share"]]
  apiDirectory = "./R"
  
  ## Get SWS Parameters
  SetClientFiles(dir = PARAMS[["certdir"]])
  GetTestEnvironment(
    baseUrl = PARAMS[["server"]],
    token = PARAMS[["token"]]
  )
  
  ## Source local scripts for this local test
  # for (file in dir(apiDirectory, full.names = T))
  #   source(file)
} else {
  message("Running on server, no need to call GetTestEnvironment...")
  
}
# Get General data for comparisons
# 
# load("~/Github/faoswsStandardization/dataNewTrade.RData")
# data_all <- data

#User name is what's after the slash
SWS_USER = regmatches(swsContext.username, 
                      regexpr("(?<=/).+$", swsContext.username, perl = TRUE))
#SWS_USER = "browningj"


message("Getting parameters/datasets...")


dataAfg <- read.csv("C:/Users/muschitiello/Documents/Validation_HHBS/Afghanistan/dataAfg.csv")
dataAfg <- data.table(dataAfg)
data <- dataAfg %>%
  mutate(measuredElementSuaFbs="food") %>%
  mutate(timePointYears="2008") %>%
  mutate(geographicAreaM49="4") %>%
  rename(measuredItemSuaFbs=cpc_code) %>%
  rename(Value=hs_tot_tons) %>%
  select(c(measuredElementSuaFbs,measuredItemSuaFbs,geographicAreaM49,timePointYears,Value))
data <- data.table(data)
data = data[!is.na(measuredElementSuaFbs), ]

# Get commodity tree with child shares of parent fpr all countries
yearVals=c("2008")
tree = getCommodityTree(timePointYears = yearVals)
FPCommodities <- c("23511.01", "23512",
                   "01499.06", "01921.01")
tree[, target := ifelse(measuredItemParentCPC %in% FPCommodities,
                        "F", "B")]

# Convert units for tourist and industrial
message("Applying adjustments to commodity tree...")

tree = adjustCommodityTree(tree, parentColname = "measuredItemParentCPC",
                           childColname = "measuredItemChildCPC", nSigma = 2)
tree[, extractionRate := ifelse(is.na(extractionRate),
                                mean(extractionRate, na.rm = TRUE),
                                extractionRate),
     by = c("measuredItemParentCPC", "measuredItemChildCPC")]
tree[is.na(extractionRate), extractionRate := 1]

tree <- tree[geographicAreaM49=="4",]

itemMap = GetCodeList(domain = "agriculture", dataset = "aproduction", "measuredItemCPC")
itemMap = itemMap[, c("code", "type"), with = FALSE]
setnames(itemMap, "code", "measuredItemSuaFbs")
data = merge(data, itemMap, by = "measuredItemSuaFbs")
setnames(itemMap, "measuredItemSuaFbs", "measuredItemParentCPC")
tree = merge(tree, itemMap, by = "measuredItemParentCPC")


# Check Afghanistan Commodity trees
# 
# yearVals=c("1992","1993","1994","1995", "1996", "1997", "2000","2001","2002","2003","2007","2008")
# tree = getCommodityTree("4",timePointYears = yearVals)


# # Name country and parent and children
# 
# treeNamed <- tree
# setnames(treeNamed,"measuredItemParentCPC","measuredItemSuaFbs")
# treeNamed <- nameData("suafbs","sua",treeNamed)
# setnames(treeNamed,c("measuredItemSuaFbs","measuredItemSuaFbs_description","measuredItemChildCPC"),c("measuredItemParentCPC","measuredItemParentCPC_description","measuredItemSuaFbs"))
# treeNamed <- nameData("suafbs","sua",treeNamed,except = "geographicAreaM49")
# setnames(treeNamed,c("measuredItemSuaFbs","measuredItemSuaFbs_description"),c("measuredItemChildCPC","measuredItemChildCPC_description"))
# 
# # Tree Afghanistan
# 
# treeNamed <- treeNamed[geographicAreaM49=="4",]

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
params$residualCode = "residual"


message("Loading nutrient data...")

itemKeys = GetCodeList("agriculture", "aupus_ratio", "measuredItemCPC")[, code]

# Nutrients are:
# 1001 Calories
# 1003 Proteins
# 1005 Fats
nutrientCodes = c("1001", "1003", "1005")

nutrientData = getNutritiveFactors(measuredElement = nutrientCodes,
                                   timePointYears = yearVals)
setnames(nutrientData, c("measuredItemCPC", "timePointYearsSP"),
         c("measuredItemSuaFbs", "timePointYears"))

message("Defining vectorized standardization function...")

standardizationVectorized = function(data, tree, nutrientData){
  
  # record if output is being sunk and at what level
  sinkNumber <- sink.number()
  # Prevent sink staying open if function is terminated prematurely (such as
  # in debugging of functions in standardizationWrapper)
  on.exit(while (sink.number() > sinkNumber) sink())
  
  if (nrow(data) == 0) {
    message("No rows in data, nothing to do")
    return(data)
  }
  
  # If printCodes is length 0, neither the .md files nor plots are created
  # If it has a non-zero value, those are the codes which will have file outputs
  
  printCodes = character()
  
  ## printCodes=c("0111")
  ## samplePool = parentNodes[parentNodes %in% data$measuredItemSuaFbs]
  ## if (length(samplePool) == 0) samplePool = data$measuredItemSuaFbs
  ## printCodes = sample(samplePool, size = 1)
  ## if (!is.null(tree)) {
  ## printCodes = getChildren(commodityTree = tree,
  ##                          parentColname = params$parentVar,
  ##                          childColname = params$childVar,
  ##                          topNodes = printCodes)
  ## }
  
  dir.create(paste0(R_SWS_SHARE_PATH, "/", SWS_USER, "/standardizationHHBS/"), showWarnings = FALSE
            )
  
  
  ##,recursive = TRUE
  
  sink(paste0(R_SWS_SHARE_PATH, "/", SWS_USER, "/standardizationHHBS/",
              data$timePointYears[1], "_",
              data$geographicAreaM49[1], "_sample_test.md"),
       split = TRUE)

  out = standardizationWrapperHHBS(data = data, tree = tree, fbsTree = fbsTreeFra2, 
                                   standParams = params, printCodes = printCodes,
                                   nutrientData = nutrientData)
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

aggFun = function(x) {
    if (length(x) > 1)
        stop("x should only be one value!")
    return(sum(x))
}

standData = vector(mode = "list", length = nrow(uniqueLevels))

uniqueLevels=uniqueLevels[geographicAreaM49!="728",]

for (i in seq_len(nrow(uniqueLevels))) {
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
    setnames(subNutrientData, nutrientCodes,
             c("Calories", "Proteins", "Fats"))
    standData[[i]] = standardizationVectorized(data = dataSubset,
                                               tree = treeSubset,
                                               nutrientData = subNutrientData)
    
    standData[[i]] <- rbindlist(standData[[i]])
    names(standData[[i]])[grep("^fbsID", names(standData[[i]]))] <- params$itemVar
    standData[[i]][,(params$itemVar):= paste0("S", get(params$itemVar))] 
  
}


message("Combining standardized data...")


standData = rbindlist(standData)

warning("The below is a rough hack to convert codes back. In truth, I'm almost
certain that the units are not the same. A unit conversion needs to happen at
the beginning and at this step.")

fbs_sua_conversion <- data.table(measuredElementSuaFbs=c("Calories", "Fats", "Proteins", "exports", "feed", "food", 
                                                         "foodManufacturing", "imports", "loss", "production", 
                                                         "seed", "stockChange", "residual","industrial", "tourist"),
                                 code=c("261", "281", "271", "5910", "5520", "5141", 
                                        "5023", "5610", "5015", "5510",
                                        "5525", "5071", "5166","5165", "5164"))
##standData[measuredElementSuaFbs %in% c(params$touristCode, params$industrialCode), 
##  `:=`(measuredElementSuaFbs = "other", 
##       Value = sum(Value, na.rm=TRUE)), 
##  by=c(params$mergeKey)]

standData = merge(standData, fbs_sua_conversion, by = "measuredElementSuaFbs")
standData[,`:=`(measuredElementSuaFbs = NULL)]
setnames(standData, "code", "measuredElementSuaFbs")

standData[, standardDeviation := NULL]

## Assign flags: I for imputed (as we're estimating/standardizing) and s for
## "sum" (aggregate)
standData[, flagObservationStatus := "I"]
standData[, flagMethod := "s"]

setcolorder(standData, c("geographicAreaM49", "measuredElementSuaFbs", "measuredItemSuaFbs", "timePointYears", 
                         "Value", "flagObservationStatus", "flagMethod"))

# Remove NA Values
standData <- standData[!is.na(Value),]

message("Attempting to save standardized data...")

out = SaveData(domain = "suafbs", dataset = "fbs", data = standData)
cat(out$inserted + out$ignored, " observations written and problems with ",
    out$discarded, sep = "")
paste0(out$inserted + out$ignored, " observations written and problems with ",
       out$discarded)
