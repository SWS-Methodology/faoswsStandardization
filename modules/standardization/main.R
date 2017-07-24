
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
  
  # always set 999 for subset batches for testing
  # Last complete batch Run 35 Cristina 11/7/17
  # Last complete batch Run 34 Francesca 6/7/17
  # Last complete batch Run 36 Francesca 
  # Last complete batch Run 37 Francesca 
  # Last complete batch Run 38 Cristina Sua filling cut corrected 
  # Last complete batch Run 39 Cristina Sua filling primary commodities & Share corrections
  # Last complete batch Run 40 Cristina Sua filling primary commodities with FAOSTAT TRADE
  # Last complete batch Run 41 Cristina Sua filling primary c.debugged, FAOSTAT TRADE, RICE food   
  # Last complete batch Run 42 Cristina Sua filling primary c.debugged, FAOSTAT TRADE, RICE food and Tree corrections  
  
  batchnumber = 42    # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! SET IT   

  
  ## Source local scripts for this local tes
  # for (file in dir(apiDirectory, full.names = T))
  #   source(file)
} else {
  message("Running on server, no need to call GetTestEnvironment...")
  
}

#User name is what's after the slash
SWS_USER = regmatches(swsContext.username, 
                      regexpr("(?<=/).+$", swsContext.username, perl = TRUE))

# I one want to create sub-folder in the share drive,
# instead of replace the existing 
# (for example save example files for different batches)
# put the name in the .yml file
# default is NULL
if(CheckDebug()){
SUB_FOLDER = paste0(PARAMS[["subShare"]],batchnumber) 
}

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

### Francesca cassava correction
tree=tree[measuredItemParentCPC!="01520",]



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

#### CRISTINA more corrections on sugar tree CRISTINA 7/7/2018
tree[measuredItemChildCPC=="2351f",extractionRate:=0.11]
tree[measuredItemParentCPC=="01343"& measuredItemChildCPC=="21419.01",extractionRate:=0.29]
tree[measuredItemParentCPC=="01491.01"& measuredItemChildCPC=="2165",extractionRate:=0.19]

###
##### FRANCESCA CORRECTIONS FOR TREES HAVE TO BE RUN HERE.

# tree=manualTreeCorrections(tree,cereals=TRUE)

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
# data = elementCodesToNames(data = GetData(key), itemCol = "measuredItemFbsSua",
# elementCol = "measuredElementSuaFbs")
# setnames(data, "measuredItemFbsSua", "measuredItemSuaFbs")



# if(CheckDebug()){
# save(data,file=file.path(PARAMS$localFiles, "05062017_dataAllNew_Seed.RData")
# }
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

if(CheckDebug()){
  # load(file.path(PARAMS$localFiles, "dataTradeChri.RData.RData"))
  # load(file.path(PARAMS$localFiles, "dataTradeNewFoodBruno.RData"))
  # load(file.path(PARAMS$localFiles, "dataTradeNewFoodBruno2.RData"))
  # load(file.path(PARAMS$localFiles, "dataMirror2.RData"))
  # load(file.path(PARAMS$localFiles, "dataNoMirror.RData"))
  # last no Mirror import
  # load(file.path(PARAMS$localFiles, "05062017_dataAllNew_Seed.RData.RData"))
  # last mirror import
  # load(file.path(PARAMS$localFiles, "dataMirror3.RData"))
  # Faostat TRADE DATA
  # load(file.path(PARAMS$localFiles, "dataTradeFAOSTAT.RData"))
  # NEW Faostat TRADE DATA (19/07/2017)
  load(file.path(PARAMS$localFiles, "dataTradeFAOSTAT2.RData"))
  
  # load(file.path(PARAMS$localFiles, "260523_dataAllNew.RData"))

}



## Cristina: correcting for Serbia and Montenegro:
# 1. The data for Servia and Montenegro (fal=186, m49=891) exist even after 2006 but with the same value as for the 2005.
# then I removed the data after 2005.
# NB There is not 186 (m49891 in the CrudeBalEl the I duplicated the value of Serbia fal=272 m49=688)
# 2. There are data for Montenegro and Serbia (separated) even before 2006





### CRISTINA: SUGAR
### temporary change in the data for accounting for corrections in sugar Tree
# data[measuredItemSuaFbs %in% c("23511.01","23512"),measuredItemSuaFbs:= "2351f"]
# I just discovered that this change is already in the data file
# data[!measuredItemSuaFbs %in% c("23511.01","23512")]


data=data[, list(Value = sum(Value, na.rm = TRUE)),
     by = c("measuredItemSuaFbs","measuredElementSuaFbs", "geographicAreaM49", "timePointYears","flagObservationStatus","flagMethod")]
data=left_join(data,flagValidTable,by=c("flagObservationStatus","flagMethod"))%>%
  data.table

data[flagObservationStatus=="",Official:=TRUE]
data[is.na(Official),Official:=FALSE]
data=data[,mget(c("measuredItemSuaFbs","measuredElementSuaFbs", "geographicAreaM49", "timePointYears","Value","Protected","Official"))]

########################################
# Final Changes in the data files for sugar

datas=data[measuredItemSuaFbs %in% c("23511.01","23512","2351f")]
datas[measuredElementSuaFbs=="tourist",measuredItemSuaFbs:="2351f"]
datas[measuredElementSuaFbs=="stockChange"&geographicAreaM49=="705",measuredItemSuaFbs:="2351f"]
datas[measuredElementSuaFbs=="food"&geographicAreaM49%in%c("422","28","308"),measuredItemSuaFbs:="2351f"]
datas[,s2351f:=sum(Value*
                     ifelse(get(params$elementVar) == params$productionCode&measuredItemSuaFbs=="2351f",1,0),na.rm = TRUE),
      by=c("geographicAreaM49","timePointYears")]
dataTorBind = unique(datas[measuredElementSuaFbs==params$productionCode,list(geographicAreaM49,timePointYears,measuredElementSuaFbs,s2351f)])
datas[,s2351f:=NULL]
dataTorBind = dataTorBind[,measuredItemSuaFbs:="2351f"]
# dataTorBind = dataTorBind[,flagObservationStatus:="I"]
# dataTorBind = dataTorBind[,flagMethod:="s"]
setnames(dataTorBind,"s2351f","Value")
dataTorBind=dataTorBind[,c(5,3,1,2,4),with=FALSE]
dataTorBind=dataTorBind[,Protected:="FALSE"]
dataTorBind=dataTorBind[,Official:="TRUE"]
datas=rbind(datas[!measuredElementSuaFbs==params$productionCode],dataTorBind)
data=data[!(measuredItemSuaFbs %in% c("23511.01","23512","2351f"))]
# 3523601-3513983
data=rbind(data,datas)
# 3523601-3521384
# 9618-7401
########################################


###


### CRISTINA: This was needed for Updating the CutItems. The actual CutItems files is updated
### so the following line is not needed
# animalChildren=unique(tree[measuredItemParentCPC %in% animals,measuredItemChildCPC])
###




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



# CRISTINA: I would chenge this to 0 because if no country report an extraction rate
# for a commodity, is probably because those commodities are not re;ated
# example: tree[geographicAreaM49=="276"&timePointYearsSP=="2012"&measuredItemParentCPC=="0111"]
# wheat germ shoul have ER max of 2% while here results in 100%

# tree[is.na(extractionRate), extractionRate := 1]
tree=tree[!is.na(extractionRate)]





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
                                     , protected, batchnumber,
                                     utilizationTable
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
  
  printCodes=c("0113")
 # printCodes=fcl2cpc(c("0267","0265","0310","0333","0263","0275","0280","0296","0299","0336","0339"))
  # ##samplePool = parentNodes[parentNodes %in% data$measuredItemSuaFbs]
  # ##if (length(samplePool) == 0) samplePool = data$measuredItemSuaFbs
  # ##printCodes = sample(samplePool, size = 1)
   # if (!is.null(tree)) {
  printCodes = getChildren(commodityTree = tree,
  parentColname = params$parentVar,
  childColname = params$childVar,
  topNodes = printCodes)

 
  dir.create(paste0(R_SWS_SHARE_PATH, "/", SWS_USER, "/", SUB_FOLDER, "/standardization/"), showWarnings = FALSE



  ,recursive = TRUE
  )
  sink(paste0(R_SWS_SHARE_PATH, "/", SWS_USER,"/", SUB_FOLDER, "/standardization/",
              data$timePointYears[1], "_",
              data$geographicAreaM49[1], "_sample_test.md"),
       split = TRUE)

  out = standardizationWrapper(data = data, tree = tree, fbsTree = fbsTree, 
                                   standParams = params, printCodes = printCodes,
                                   nutrientData = nutrientData, crudeBalEl = crudeBalEl,
                                   debugFile = params$createIntermetiateFile
                               , protected = protected, batchnumber = batchnumber,
                               utilizationTable = utilizationTable)
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

# uniqueLevels=uniqueLevels[geographicAreaM49 %in% c("376","36", "40")&timePointYears=="2013",]
### for verify standardization
# uniqueLevels=uniqueLevels[geographicAreaM49 %in% c("646","250","276"),]
# uniqueLevels=uniqueLevels[geographicAreaM49 %in% c("178","414"),]
# uniqueLevels=uniqueLevels[geographicAreaM49 %in% c("24","276","380","804","344"),]
# uniqueLevels=uniqueLevels[geographicAreaM49 %in% c("24","804"),]
# uniqueLevels=uniqueLevels[geographicAreaM49 %in% c("24")&timePointYears=="2001",]
# uniqueLevels=uniqueLevels[geographicAreaM49 %in% c("124")&timePointYears=="2002",]
# uniqueLevels=uniqueLevels[geographicAreaM49 %in% c("152","178","262","380"),]
# uniqueLevels=uniqueLevels[geographicAreaM49 %in% c("212","242","659","158")]
# uniqueLevels=uniqueLevels[geographicAreaM49 %in% c("659")] # Saint Kittes
# uniqueLevels=uniqueLevels[geographicAreaM49 %in% c("96","882","242","152")]
# uniqueLevels=uniqueLevels[geographicAreaM49 %in% c("96","882","158")]
# uniqueLevels=uniqueLevels[geographicAreaM49 %in% c("124","132","344")]
# uniqueLevels=uniqueLevels[geographicAreaM49 %in% c("50","52","68","1248")]
# uniqueLevels=uniqueLevels[geographicAreaM49 %in% c("50","1248")]
# uniqueLevels=uniqueLevels[geographicAreaM49 %in% c("158","1248" )]

uniqueLevels=uniqueLevels[!geographicAreaM49 %in% c("728","886","654"),]
# uniqueLevels=uniqueLevels[!geographicAreaM49 %in% c("729", "166", "584", "580", "585", "674", "654", "238", "156")]

if(params$createIntermetiateFile){
  if(file.exists(paste0("debugFile/Batch_",batchnumber,"/B",batchnumber,"_00a_AfterSuaFilling1.csv"))){
    file.remove(paste0("debugFile/Batch_",batchnumber,"/B",batchnumber,"_00a_AfterSuaFilling1.csv"))
  }
  if(file.exists(paste0("debugFile/Batch_",batchnumber,"/B",batchnumber,"_00b_AfterFoodProc.csv"))){
    file.remove(paste0("debugFile/Batch_",batchnumber,"/B",batchnumber,"_00b_AfterFoodProc.csv"))
  }
  if(file.exists(paste0("debugFile/Batch_",batchnumber,"/B",batchnumber,"_02_AfterSuaFilling_BeforeST.csv"))){
    file.remove(paste0("debugFile/Batch_",batchnumber,"/B",batchnumber,"_02_AfterSuaFilling_BeforeST.csv"))
  }
  if(file.exists(paste0("debugFile/Batch_",batchnumber,"/B",batchnumber,"_03_AfterST_BeforeFBSbal.csv"))){
    file.remove(paste0("debugFile/Batch_",batchnumber,"/B",batchnumber,"_03_AfterST_BeforeFBSbal.csv"))
  }
  if(file.exists(paste0("debugFile/Batch_",batchnumber,"/B",batchnumber,"_10_ForcedProduction.csv"))){
    file.remove(paste0("debugFile/Batch_",batchnumber,"/B",batchnumber,"_10_ForcedProduction.csv"))
  }
}


if(CheckDebug()){
  dir.create(paste0(PARAMS$debugFolder,"/Batch_",batchnumber), showWarnings = FALSE,recursive=TRUE)
}


## IMBALANCE ANALYSIS 1: INITIAL SUA IMBALANCE 
# save the initial data locally for future reports
if(CheckDebug()){
initialSua = data
save(initialSua,file=paste0(PARAMS$debugFolder,"/Batch_",batchnumber,"/B",batchnumber,"_01_InitialSua_BeforeCB.RData"))
}


# Run all the standardization and balancig for combination of country/year
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
    utilizationTableSubset = utilizationTable[get(params$geoVar)==as.character(uniqueLevels[i,1,with=FALSE])]

    standData[[i]] = standardizationVectorized(data = dataSubset,
                                               tree = treeSubset,
                                               nutrientData = subNutrientData,
                                               protected = protectedSubset,
                                               batchnumber = batchnumber,
                                               utilizationTable = utilizationTableSubset
                                               )
    
    standData[[i]] <- rbindlist(standData[[i]])
    names(standData[[i]])[grep("^fbsID", names(standData[[i]]))] <- params$itemVar
    standData[[i]][,(params$itemVar):= paste0("S", get(params$itemVar))] 
  
}



message((proc.time() - ptm)[3])

message("Combining standardized data...")
standData = rbindlist(standData)

# Save the StandData
save(standData,file=("debugFile/standData.RData"))

# 

# Save the StandData LOCALLY
if(CheckDebug()){
  save(standData,file=paste0(PARAMS$temporaryStandData,"/standDatabatch",batchnumber,".RData"))
}
#################################################################
###################################
## INITIAL IMBALANCE
  # write.table(initialSua, paste0("debugFile/Batch_",batchnumber,"/B",batchnumber,"_000_InitialSua.csv"))



###################################
## AFTER SUA FILLING 1 (No intermediate saving)
ptm <- proc.time()
if(file.exists(paste0("debugFile/Batch_",batchnumber,"/B",batchnumber,"_00a_AfterSuaFilling1.csv"))){
  AfterSuaFilling1 = read.table(paste0("debugFile/Batch_",batchnumber,"/B",batchnumber,"_00a_AfterSuaFilling1.csv"),
                           header=FALSE,sep=";",col.names=c("geographicAreaM49", "measuredElementSuaFbs", "measuredItemFbsSua",
                                                            "timePointYears","Value","flagObservationStatus","flagMethod"),
                           colClasses = c("character","character","character","character","character","character","character"))
  AfterSuaFilling1 = data.table(AfterSuaFilling1)
  message((proc.time() - ptm)[3])
  
  # Save these data LOCALLY
  if(CheckDebug()){
    save(AfterSuaFilling1,file=paste0(PARAMS$debugFolder,"/Batch_",batchnumber,"/B",batchnumber,"_00a_AfterSuaFilling1.RData"))
  }
}
###################################


###################################
## AFTER FOOD PROCESSING
ptm <- proc.time()
if(file.exists(paste0("debugFile/Batch_",batchnumber,"/B",batchnumber,"_00b_AfterFoodProc.csv"))){
  AfterFoodProc = read.table(paste0("debugFile/Batch_",batchnumber,"/B",batchnumber,"_00b_AfterFoodProc.csv"),
                           header=FALSE,sep=";",col.names=c("geographicAreaM49", "measuredElementSuaFbs", "measuredItemFbsSua",
                                                            "timePointYears","Value","flagObservationStatus","flagMethod"),
                           colClasses = c("character","character","character","character","character","character","character"))
  AfterFoodProc = data.table(AfterFoodProc)
  message((proc.time() - ptm)[3])
  
  # Save these data LOCALLY
  if(CheckDebug()){
    save(AfterFoodProc,file=paste0(PARAMS$debugFolder,"/Batch_",batchnumber,"/B",batchnumber,"_00b_AfterFoodProc.RData"))
  }
}
###################################

###################################
## FORCED COMMODITIES IN THE SUA FILLING PROCESS
ptm <- proc.time()
if(file.exists(paste0("debugFile/Batch_",batchnumber,"/B",batchnumber,"_10_ForcedProduction.csv"))){
FORCED_PROD = read.table(paste0("debugFile/Batch_",batchnumber,"/B",batchnumber,"_10_ForcedProduction.csv"),
                              header=FALSE,sep=";",col.names=c("geographicAreaM49", "measuredElementSuaFbs", "measuredItemFbsSua",
                                                               "timePointYears","Value","flagObservationStatus","flagMethod"),
                              colClasses = c("character","character","character","character","character","character","character"))
FORCED_PROD = data.table(FORCED_PROD)
message((proc.time() - ptm)[3])
message(paste0( length(FORCED_PROD[,unique(measuredItemFbsSua)])," Commodity have a FORCED supply element"))

# Save these data LOCALLY
if(CheckDebug()){
  save(FORCED_PROD,file=paste0(PARAMS$debugFolder,"/Batch_",batchnumber,"/B",batchnumber,"_10_ForcedProduction.RData"))
 }
}
###################################


###################################
## IMBALANCE ANALYSIS SAVE 2
### FIRST INTERMEDIATE SAVE

ptm <- proc.time()
AfterSuaFilling = read.table(paste0("debugFile/Batch_",batchnumber,"/B",batchnumber,"_02_AfterSuaFilling_BeforeST.csv"),
                     header=FALSE,sep=";",col.names=c("geographicAreaM49", "measuredElementSuaFbs", "measuredItemFbsSua",
                                                      "timePointYears","Value","flagObservationStatus","flagMethod"),
                     colClasses = c("character","character","character","character","character","character","character"))
AfterSuaFilling = data.table(AfterSuaFilling)
# SaveData(domain = "suafbs", dataset = "sua_balanced", data = AfterSuaFilling, waitTimeout = 20000)
message((proc.time() - ptm)[3])

# Save these data LOCALLY
if(CheckDebug()){
  save(AfterSuaFilling,file=paste0(PARAMS$debugFolder,"/Batch_",batchnumber,"/B",batchnumber,"_02_AfterSuaFilling_BeforeST.RData"))
}

###################################
## IMBALANCE ANALYSIS SAVE 3
### SECOND INTERMEDIATE SAVE
ptm <- proc.time()
AfterST_BeforeFBSbal = read.table(paste0("debugFile/Batch_",batchnumber,"/B",batchnumber,"_03_AfterST_BeforeFBSbal.csv"),
                       header=FALSE,sep=";",col.names=c("geographicAreaM49", "measuredElementSuaFbs", "measuredItemFbsSua", 
                                                        "timePointYears","Value","flagObservationStatus","flagMethod"),
                       colClasses = c("character","character","character","character","character","character","character"))
AfterST_BeforeFBSbal = data.table(AfterST_BeforeFBSbal)
# SaveData(domain = "suafbs", dataset = "fbs_standardized", data = AfterST_BeforeFBSbal, waitTimeout = 20000)
message((proc.time() - ptm)[3])

# Save these data LOCALLY
if(CheckDebug()){
  save(AfterST_BeforeFBSbal,file=paste0(PARAMS$debugFolder,"/Batch_",batchnumber,"/B",batchnumber,"_03_AfterST_BeforeFBSbal.RData"))
}

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



