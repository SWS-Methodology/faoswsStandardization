## load the library
library(faosws)
library(faoswsUtil)
library(data.table)
library(igraph)
library(faoswsBalancing)
library(faoswsStandardization)
library(faoswsFlag)

library(stringr)
library(dplyr)
library(MASS) 
library(lattice)
library(reshape2)


if(packageVersion("faoswsStandardization") < package_version('0.1.0')){
  stop("faoswsStandardization is out of date")
}

## set up for the test environment and parameters
R_SWS_SHARE_PATH = Sys.getenv("R_SWS_SHARE_PATH")

if (CheckDebug()) {
  library(faoswsModules)
  message("Not on server, so setting up environment...")
  
  # Read settings file sws.yml in working directory. See 
  # sws.yml.example for more information
  PARAMS <- ReadSettings("modules/standardization/sws.yml")
  message("Connecting to server: ", PARAMS[["current"]])
  
  R_SWS_SHARE_PATH = PARAMS[["share"]]
  apiDirectory = "./R"
  
  ## Get SWS Parameters
  SetClientFiles(dir = PARAMS[["certdir"]])
  GetTestEnvironment(
    baseUrl = PARAMS[["server"]],
    token = PARAMS[["token"]]
  )
  
  ## Source local scripts for this local tes
  # for (file in dir(apiDirectory, full.names = T))
  #   source(file)
} else {
  message("Running on server, no need to call GetTestEnvironment...")
  
}

#User name is what's after the slash
SWS_USER = regmatches(swsContext.username, 
                      regexpr("(?<=/).+$", swsContext.username, perl = TRUE))
SWS_USER = "muschitielloBatch32"

message("Getting parameters/datasets...")

# start and end year for standardization come from user parameters
startYear = as.numeric(swsContext.computationParams$startYear)
endYear = as.numeric(swsContext.computationParams$endYear)
stopifnot(startYear <= endYear)

yearVals = as.character(startYear:endYear)




# Get commodity tree with child shares of parent
tree = getCommodityTree(timePointYears = yearVals)


### Francesca meat correction
meat=c("21118.01","21139","21111.01" ,"21112","21113.01","21114","21115","21116","21117.01","21117.02","21118.02","21118.03","21119.01","21121","21122",
       "21123","21124","21125","21131","21132","21133","21134","21135","21136","21137","21137","21138","21138","21138","21141",
       "21142","21143","21144","21145","21170.01")
animals= unique(tree[measuredItemChildCPC %in% meat, measuredItemParentCPC])
tree=tree[!measuredItemParentCPC %in% animals,]
tree=tree[measuredItemParentCPC!="01520",]
###

### Cristina Correction sugar Tree
tree = tree[!measuredItemParentCPC=="23670.01"] # All ER = NA (rows=3878)
tree = tree[!measuredItemParentCPC=="2351"] # All ER = NA 0.9200 0.9300 0.9650 0.9600 0.9350 0.9430 0.9346 (rows=3878)
tree = tree[!measuredItemParentCPC=="23511"] # All ER = NA (rows=3878)
tree = tree[!(measuredItemChildCPC=="2413"& measuredItemParentCPC %in% c("23520","23511.01","39160","24110"))] # NA 0.7 (rows=3878)
tree = tree[!(measuredItemChildCPC=="24110"& measuredItemParentCPC=="39160")] # NA 0.45 0.25 (rows=3878)
tree = tree[!(measuredItemChildCPC=="24490" & measuredItemParentCPC=="23511.01")] # NA 5 (rows=3878)
tree = tree[!(measuredItemChildCPC=="2351" & measuredItemParentCPC=="23512")] # All ER = NA (rows=3878)
tree = tree[!measuredItemChildCPC=="23511"] # NA 0.1 (rows=3878)
tree[measuredItemParentCPC=="01802" & measuredItemChildCPC=="23511.01", measuredItemChildCPC:="2351f"]
tree[measuredItemParentCPC=="01801" & measuredItemChildCPC=="23512", measuredItemChildCPC:="2351f"]
tree[measuredItemParentCPC=="23511.01", measuredItemParentCPC:="2351f"]
tree[measuredItemParentCPC=="23512", measuredItemParentCPC:="2351f"]
tree = tree[!(measuredItemParentCPC=="2351f" & measuredItemChildCPC == "2351f"),]


###


### Cristina Correction wheat Tree
# tree = tree[!(measuredItemChildCPC=="23140.03" & measuredItemParentCPC=="23110")]
###




### Graph of all commodities
# graphdata <- tree[, .(geographicAreaM49, measuredItemParentCPC, measuredItemChildCPC, extractionRate, share)]
# graphdata <- graphdata[, .(extractionRate = mean(extractionRate, na.rm = TRUE),
#                  share = mean(share, na.rm = TRUE)),
#           by = .(measuredItemParentCPC, measuredItemChildCPC)]
# codetable <- GetCodeList("agriculture", "aproduction", "measuredItemCPC", 
#             union(graphdata[, measuredItemParentCPC], graphdata[, measuredItemChildCPC]))[,.(code, description)]
# 
# graphdata <- merge(graphdata, codetable, by.x="measuredItemParentCPC", by.y="code")
# graphdata[, measuredItemParentCPC := NULL]
# setnames(graphdata, "description", "measuredItemParentCPC")
# 
# graphdata <- merge(graphdata, codetable, by.x="measuredItemChildCPC", by.y="code")
# graphdata[, measuredItemChildCPC := NULL]
# setnames(graphdata, "description", "measuredItemChildCPC")
# 
# setcolorder(graphdata, c("measuredItemParentCPC", "measuredItemChildCPC", "extractionRate", "share"))
# 
# g <- graph_from_data_frame(graphdata)
# 
# pdf(width=20, height=20, file="biggraph.pdf")
# plot(g, vertex.size = .8, vertex.label.cex = 3, vertex.label.color = "indianred", 
#      edge.arrow.size = 0.05, edge.width=.1, edge.curved = TRUE)
# dev.off()

###

areaKeys = GetCodeList(domain = "suafbs", dataset = "sua_unbalanced", "geographicAreaM49")
areaKeys = areaKeys[type == "country", code]

elemKeys = GetCodeTree(domain = "suafbs", dataset = "sua_unbalanced", "measuredElementSuaFbs")

#    code              description
# 1:   51                   Output
# 2:   61              Inflow (Qt)
# 3:   71 Variation Intial Exstenc
# 4:   91             Outflow (Qt)
# 5:  101     Use For Animals (Qt)
# 6:  111     Use For Same Product
# 7:  121                   Losses
# 8:  131 Reemployment Same Sector  (remove it)

fs_elements <- c("51", "61", "71", "91", "101", "111", "121", "131")

elemKeys = elemKeys[parent %in% fs_elements,
                    paste0(children, collapse = ", ")]

# code                  description
# 1: 5141                     Food [t]
# 2: 5164 Tourist consumption [1000 t]
# 3: 5165     Industrial uses [1000 t]


sws_elements <- c("5141", "5164", "5165")

elemKeys = c(strsplit(elemKeys, ", ")[[1]], sws_elements)
itemKeys = GetCodeList(domain = "suafbs", dataset = "sua_unbalanced", "measuredItemFbsSua")
itemKeys = itemKeys[, code]

key = DatasetKey(domain = "suafbs", dataset = "sua_unbalanced", dimensions = list(
  geographicAreaM49 = Dimension(name = "geographicAreaM49", keys = areaKeys),
  measuredElementSuaFbs = Dimension(name = "measuredElementSuaFbs", keys = elemKeys),
  measuredItemFbsSua = Dimension(name = "measuredItemFbsSua", keys = itemKeys),
  timePointYears = Dimension(name = "timePointYears", keys = yearVals)
))

message("Reading SUA data...")

## This gets the values for all countries, all elements which are children of the
## element classes listed above, all CPCs in suafbs and all years between those
## specified by the user.

##!! 3 warnings about things that need to be changed !!#
data = elementCodesToNames(data = GetData(key), itemCol = "measuredItemFbsSua",
elementCol = "measuredElementSuaFbs")
setnames(data, "measuredItemFbsSua", "measuredItemSuaFbs")


# save(data,file="C:/Users/muschitiello/Documents/StandardizationFrancescaCristina/SupportFiles_Standardization/260523_dataAllNew.RData")


# load("C:/Users/muschitiello/Documents/StandardizationFrancescaCristina/SupportFiles_Standardization/dataTradeChri.RData")
# load("C:/Users/muschitiello/Documents/StandardizationFrancescaCristina/SupportFiles_Standardization/dataTradeNewFoodBruno.RData")
# load("C:/Users/muschitiello/Documents/StandardizationFrancescaCristina/SupportFiles_Standardization/dataTradeNewFoodBruno2.RData")
# load("C:/Users/muschitiello/Documents/StandardizationFrancescaCristina/SupportFiles_Standardization/dataMirror2.RData")
# load("C:/Users/muschitiello/Documents/StandardizationFrancescaCristina/SupportFiles_Standardization/dataNoMirror.RData")
# load("C:/Users/muschitiello/Documents/StandardizationFrancescaCristina/SupportFiles_Standardization/180523_dataNewLoss.RData")

# last no Mirror import
# load("C:/Users/muschitiello/Documents/StandardizationFrancescaCristina/SupportFiles_Standardization/260523_dataAllNew.RData")

# last mirror import
# load("C:/Users/muschitiello/Documents/StandardizationFrancescaCristina/SupportFiles_Standardization/dataMirror3.RData")

# Faostat TRADE DATA
# load("C:/Users/muschitiello/Documents/StandardizationFrancescaCristina/SupportFiles_Standardization/dataTradeFAOSTAT.RData")




## Cristina: correcting for Serbia and Montenegro:
# 1. The data for Servia and Montenegro (fal=186, m49=891) exist even after 2006 but with the same value as for the 2005.
# then I removed the data after 2005.
# NB There is not 186 (m49891 in the CrudeBalEl the I duplicated the value of Serbia fal=272 m49=688)
# 2. There are data for Montenegro and Serbia (separated) even before 2006





### CRISTINA: SUGAR
### temporary change in the data for accounting for corrections in sugar Tree
# data[measuredItemSuaFbs %in% c("23511.01","23512"),measuredItemSuaFbs:= "2351f"]
# I just discovered that this change is already in the data file
data[!measuredItemSuaFbs %in% c("23511.01","23512")]


data=data[, list(Value = sum(Value, na.rm = TRUE)),
     by = c("measuredItemSuaFbs","measuredElementSuaFbs", "geographicAreaM49", "timePointYears","flagObservationStatus","flagMethod")]
data=left_join(data,flagValidTable,by=c("flagObservationStatus","flagMethod"))%>%
  data.table

data[flagObservationStatus=="",Official:=TRUE]
data[is.na(Official),Official:=FALSE]
data=data[,mget(c("measuredItemSuaFbs","measuredElementSuaFbs", "geographicAreaM49", "timePointYears","Value","Protected","Official"))]

###


### CRISTINA: This was needed for Updating the CutItems. The actual CutItems files is updated
# animalChildren=unique(tree[measuredItemParentCPC %in% animals,measuredItemChildCPC])
###



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
params$createIntermetiateFile= "TRUE"
params$protected = "Protected"
params$official = "Official"

#protected data
#### CRISTINA: after havig discovered that for crops , official food values are Wrong and have to be deleted. 
# now we have to delete all the wrong values:

primaryEq=fbsTree[,unique(measuredItemSuaFbs)]

#### CRISTINA: idea of treatment of official food values for the 19 commodities of the Questionnairses
# setnames(crudeBalEl,"measuredElementSuaFbs","balElement")
# dataBalEl = data.table(left_join(data,crudeBalEl,by=c("measuredItemSuaFbs","geographicAreaM49")))
# dataBalEl[get(params$elementVar)==params$foodCode
#           &get(params$itemVar) %in% primaryEq
#           &balElement%in%c(params$foodCode)
#                  ,Value:=NA]
# data=dataBalEl[,c(1,3,2,4:7),with=FALSE]
# 
# This part for the moment is not commented because protected is
# a parameter. When this removal will be decided definively, 
# this has to be removed from here and from argument of wrapper
protected = data[get(params$official)=="TRUE"
                 &get(params$protected)=="TRUE"
                 &get(params$elementVar)==params$foodCode
                 &get(params$itemVar) %in% primaryEq
                 # &!is.na(Value)
                 ,]
# 
# protected=merge(protected[,c(1,3,4),with=FALSE],data,by=c("measuredItemSuaFbs","geographicAreaM49","timePointYears"))
# protected=protected[!is.na(measuredElementSuaFbs)]
# 
# setnames(crudeBalEl,"balElement",params$elementVar)
###

### CRISTINA: trial for BAtch 30 (Germany based decision)
cropsOfficialFood = c("0111","0112","0115","0116","0117","01199.02","01801","01802")
data[get(params$itemVar)%in%cropsOfficialFood
     &get(params$official)==TRUE
     &get(params$elementVar)==params$foodCode
     ,Value:=NA]


###CRISTINA: Test for batch 28
data[get(params$protected)=="FALSE"
     &get(params$elementVar)==params$productionCode
     &!(get(params$itemVar) %in% primaryEq),Value:=NA]
###


# Convert units for tourist and industrialAfterCB
message("Applying adjustments to commodity tree...")

## Update tree by setting some edges to "F", computing average extraction rates
## when missing, and bringing in extreme extraction rates
# FPCommodities <- c("23511.01", "23512","01499.06", "01921.01")

##Cristina
FPCommodities <- c( "01499.06", "01921.01")
##


# These commodities are forwards processed instead of backwards processed:
#        code             description type
# 1: 23511.01 Cane sugar, centrifugal CRNP
# 2:    23512              Beet sugar CRNP
# 3: 01499.06      Kapokseed in shell CRNP
# 4: 01921.01   Seed cotton, unginned CRPR  

tree[, target := ifelse(measuredItemParentCPC %in% FPCommodities,
                        "F", "B")]
tree = adjustCommodityTree(tree, parentColname = "measuredItemParentCPC",
                           childColname = "measuredItemChildCPC", nSigma = 2)

# If there's no extraction rate, use the mean extraction rate for that parent
# child combinations
tree[, extractionRate := ifelse(is.na(extractionRate),
                                mean(extractionRate, na.rm = TRUE),
                                extractionRate),
     by = c("measuredItemParentCPC", "measuredItemChildCPC")]
# If there's still no extraction rate, use an extraction rate of 1
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

standardizationVectorized = function(data, tree, nutrientData
                                     , protected
                                     ){
  
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
  
  printCodes=c("0111")
  # ##samplePool = parentNodes[parentNodes %in% data$measuredItemSuaFbs]
  # ##if (length(samplePool) == 0) samplePool = data$measuredItemSuaFbs
  # ##printCodes = sample(samplePool, size = 1)
  #  if (!is.null(tree)) {
   printCodes = getChildren(commodityTree = tree,
                            parentColname = params$parentVar,
                            childColname = params$childVar,
                            topNodes = printCodes)

 
### Cristina for printing Germany and all the things involved in the wheat
 
 # printCodes1 = "0111"
 # printCodes2 = getChildren(commodityTree = tree,
 #                           parentColname = params$parentVar,
 #                           childColname = params$childVar,
 #                           topNodes = printCodes1)
 # printCodes3=tree[measuredItemChildCPC %in% printCodes2&measuredItemParentCPC%in%primaryEl,unique(measuredItemParentCPC)]
 # printCodes = getChildren(commodityTree = tree,
 #                          parentColname = params$parentVar,
 #                          childColname = params$childVar,
 #                          topNodes = printCodes3)
 
 
 
 
  dir.create(paste0(R_SWS_SHARE_PATH, "/", SWS_USER, "/standardization/"), showWarnings = FALSE



  ,recursive = TRUE
  )
  sink(paste0(R_SWS_SHARE_PATH, "/", SWS_USER, "/standardization/",
              data$timePointYears[1], "_",
              data$geographicAreaM49[1], "_sample_test.md"),
       split = TRUE)

  out = standardizationWrapper(data = data, tree = tree, fbsTree = fbsTree, 
                                   standParams = params, printCodes = printCodes,
                                   nutrientData = nutrientData, crudeBalEl = crudeBalEl,
                                   debugFile = params$createIntermetiateFile
                               , protected = protected
                               )
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

# uniqueLevels=uniqueLevels[geographicAreaM49 %in% c("4")&timePointYears=="2013",]
### for verify standardization
# uniqueLevels=uniqueLevels[geographicAreaM49 %in% c("646","250","276"),]
# uniqueLevels=uniqueLevels[geographicAreaM49 %in% c("276","8","380","246"),]
# uniqueLevels=uniqueLevels[geographicAreaM49 %in% c("276"),]
# uniqueLevels=uniqueLevels[geographicAreaM49 %in% c("36","96"),]
# uniqueLevels=uniqueLevels[geographicAreaM49 %in% c("144")&timePointYears=="2011",]

uniqueLevels=uniqueLevels[!geographicAreaM49 %in% c("728","886"),]
# uniqueLevels=uniqueLevels[!geographicAreaM49 %in% c("729", "166", "584", "580", "585", "674", "654", "238", "156")]


if(params$createIntermetiateFile){

  if(file.exists("debugFile/StandardizedPrimaryEquivalent.csv")){
    file.remove("debugFile/StandardizedPrimaryEquivalent.csv")
  }
  if(file.exists("debugFile/AfterCrudeBalancing_b.csv")){
    file.remove("debugFile/AfterCrudeBalancing_b.csv")
  }
}

ptm <- proc.time()
for (i in seq_len(nrow(uniqueLevels))) {
    filter = uniqueLevels[i, ]
    dataSubset = data[filter, , on = c("geographicAreaM49", "timePointYears")]
    treeSubset = tree[filter, , on = c("geographicAreaM49", "timePointYears")]
    protectedSubset = protected[filter, , on = c("geographicAreaM49", "timePointYears")]
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
                                               nutrientData = subNutrientData,
                                               protected = protectedSubset
                                               )
    
    standData[[i]] <- rbindlist(standData[[i]])
    names(standData[[i]])[grep("^fbsID", names(standData[[i]]))] <- params$itemVar
    standData[[i]][,(params$itemVar):= paste0("S", get(params$itemVar))] 
  
}


message((proc.time() - ptm)[3])

message("Combining standardized data...")
standData = rbindlist(standData)

save(standData,file=("debugFile/standData.RData"))

# batchnumber = 31
# 
# save(standData,file=paste0("C:/Users/muschitiello/Documents/StandardizationFrancescaCristina/TemporaryBatches/standDatabatch",batchnumber,".RData"))

#################################################################

###################################
### FIRST INTERMEDIATE SAVE


ptm <- proc.time()
AfterCB = read.table("debugFile/AfterCrudeBalancing.csv",
                     header=FALSE,sep=";",col.names=c("geographicAreaM49", "measuredElementSuaFbs", "measuredItemFbsSua", 
                                                      "timePointYears","Value","flagObservationStatus","flagMethod"),
                     colClasses = c("character","character","character","character","character","character","character"))
AfterCB = data.table(AfterCB)

# save(AfterCB,file=paste0("C:/Users/Muschitiello/Documents/StandardizationFrancescaCristina/debugFile/AfterCrudeBalancing_batch",batchnumber,".RData"))
SaveData(domain = "suafbs", dataset = "sua_balanced", data = AfterCB, waitTimeout = 20000)
message((proc.time() - ptm)[3])


###################################
### SECOND INTERMEDIATE SAVE

ptm <- proc.time()
StandPrEq = read.table("debugFile/StandardizedPrimaryEquivalent.csv",
                     header=FALSE,sep=";",col.names=c("geographicAreaM49", "measuredElementSuaFbs", "measuredItemFbsSua", 
                                                      "timePointYears","Value","flagObservationStatus","flagMethod"),
                     colClasses = c("character","character","character","character","character","character","character"))
StandPrEq = data.table(StandPrEq)
# save(StandPrEq,file=paste0("C:/Users/Muschitiello/Documents/StandardizationFrancescaCristina/debugFile/StandardizedPrimaryEquivalent_batch",batchnumber,".RData"))

SaveData(domain = "suafbs", dataset = "fbs_standardized", data = StandPrEq, waitTimeout = 20000)
message((proc.time() - ptm)[3])

###################################
### FINAL SAVE

warning("The below is a rough hack to convert codes back. In truth, I'm almost
        certain that the units are not the same. A unit conversion needs to happen at
        the beginning and at this step.")

fbs_sua_conversion <- data.table(measuredElementSuaFbs=c("Calories", "Fats", "Proteins", "exports", "feed", "food", 
                                                         "foodManufacturing", "imports", "loss", "production", 
                                                         "seed", "stockChange", "residual","industrial", "tourist",
                                                         "DES_calories","DES_proteins","DES_fats", "population"),
                                 code=c("261", "281", "271", "5910", "5520", "5141", 
                                        "5023", "5610", "5016", "5510",
                                        "5525", "5071", "5166","5165", "5164","664","674","684","5215"))

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


setnames(standData, "measuredItemSuaFbs", "measuredItemFbsSua")

ptm <- proc.time()
out = SaveData(domain = "suafbs", dataset = "fbs_balanced_", data = standData, waitTimeout = 2000000)
cat(out$inserted + out$ignored, " observations written and problems with ",
    out$discarded, sep = "")
paste0(out$inserted + out$ignored, " observations written and problems with ",
       out$discarded)
message((proc.time() - ptm)[3])



