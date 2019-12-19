
#sapply(dir("R", full.names = TRUE), source)
options(scipen=999)

commence<-Sys.time()
## load the library
library(faosws)
library(faoswsUtil)
library(faoswsBalancing)
library(faoswsStandardization)
library(faoswsFlag)

message("libraries loaded")

library(data.table)
library(igraph)
library(stringr)
library(dplyr)
# library(dtplyr)
library(MASS) 
library(lattice)
library(reshape2)
library(sendmailR)
library(tidyr)

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
  PARAMS <- ReadSettings("modules/fullStandardizationAndBalancing/sws.yml")
  message("Connecting to server: ", PARAMS[["current"]])
  
  R_SWS_SHARE_PATH = PARAMS[["share"]]
  apiDirectory = "./R"
  
  ## Get SWS Parameters
  SetClientFiles(dir = PARAMS[["certdir"]])
  GetTestEnvironment(
    baseUrl = PARAMS[["server"]],
    token = PARAMS[["token"]]
  )
  
  
  batchnumber = 1 # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! SET IT   
  
} else {
  batchnumber = 000 # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! SET IT   
  message("Running on server, no need to call GetTestEnvironment...")
  
}


#User name is what's after the slash
SWS_USER = regmatches(swsContext.username, 
                      regexpr("(?<=/).+$", swsContext.username, perl = TRUE))

# instead of replace the existing 
# (for example save example files for different batches)
# put the name in the .yml file
# default is NULL

if(CheckDebug()){
  SUB_FOLDER = paste0(PARAMS[["subShare"]],batchnumber) 
}

message("Getting parameters/datasets...")
COUNTRY <- as.character(swsContext.datasets[[1]]@dimensions$geographicAreaM49@keys)

COUNTRY_NAME <-
  nameData(
    "suafbs", "sua_unbalanced",
    data.table(geographicAreaM49 = COUNTRY))$geographicAreaM49_description

# start and end year for standardization come from user parameters
startYear = swsContext.computationParams$startYear
endYear = swsContext.computationParams$endYear
geoM49 = swsContext.computationParams$geom49
stopifnot(startYear <= endYear)
yearVals = as.character(startYear:endYear)
outlierMail = swsContext.computationParams$checks

##  Get data configuration and session
sessionKey_fbsBal = swsContext.datasets[[1]]
#sessionKey_suaUnb = swsContext.datasets[[2]]
sessionKey_suabal = swsContext.datasets[[2]]
sessionKey_fbsStand = swsContext.datasets[[3]]


sessionCountries =
  getQueryKey("geographicAreaM49", sessionKey_fbsBal)

geoKeys = GetCodeList(domain = "agriculture", dataset = "aproduction",
                      dimension = "geographicAreaM49")[type == "country", code]

# Select the countries based on the user input parameter
selectedGEOCode =
  switch(geoM49,
         "session" = sessionCountries,
         "all" = geoKeys)
areaKeys = selectedGEOCode


##############################################################
############ DOWNLOAD AND VALIDATE TREE ######################
##############################################################

ptm <- proc.time()
#tree=getCommodityTreeNewMethod(areaKeys,yearVals)
message("Downloading tree...")
tree=getCommodityTreeNewMethod(areaKeys,as.character(2000:2017))

message((proc.time() - ptm)[3])

# Exception: high share conmfirmed by official data

tree_exceptions <- tree[geographicAreaM49 == "392" & measuredItemParentCPC == "0141" & measuredItemChildCPC == "23995.01"]

if (nrow(tree_exceptions) > 0) {
  tree <- tree[!(geographicAreaM49 == "392" & measuredItemParentCPC == "0141" & measuredItemChildCPC == "23995.01")]
}

validateTree(tree)

if (nrow(tree_exceptions) > 0) {
  tree <- rbind(tree, tree_exceptions)
  rm(tree_exceptions)
}


# NA ExtractionRates are recorded in the sws dataset as 0
# for the standardization, we nee them to be treated as NA
# therefore here we are re-changing it

tree[Value==0,Value:=NA]

#fILL EXTRACTION RATE---------------------
expanded_tree <-
  merge(
    data.table(
      expand.grid(
        geographicAreaM49 = unique(tree$geographicAreaM49),
        timePointYears = sort(unique(tree$timePointYears))
      )),
    unique(tree[, .(geographicAreaM49, measuredElementSuaFbs,
                    measuredItemParentCPC, measuredItemChildCPC)]),
    by = "geographicAreaM49",
    all = TRUE,
    allow.cartesian = TRUE
  )

tree <- tree[expanded_tree, on = colnames(expanded_tree)]

# flags for carry forward/backward
tree[is.na(Value), c("flagObservationStatus", "flagMethod") := list("E", "t")]

tree <-
  tree[!is.na(Value)][
    tree,
    on = c("geographicAreaM49", "measuredElementSuaFbs",
           "measuredItemParentCPC", "measuredItemChildCPC",
           "timePointYears"),
    roll = -Inf
    ]

tree <-
  tree[!is.na(Value)][
    tree,
    on = c("geographicAreaM49", "measuredElementSuaFbs",
           "measuredItemParentCPC", "measuredItemChildCPC",
           "timePointYears"),
    roll = Inf
    ]

# keep orig flags
tree[, flagObservationStatus := i.i.flagObservationStatus]
tree[, flagMethod := i.i.flagMethod]

tree[, names(tree)[grep("^i\\.", names(tree))] := NULL]

# saveRDS(
#   tree[
#     !is.na(Value) & measuredElementSuaFbs == "extractionRate",
#     -grepl("measuredElementSuaFbs", names(tree)),
#     with = FALSE
#     ],
#   file.path(R_SWS_SHARE_PATH, "FBS_validation", COUNTRY, "tree.rds")
# )
#END FILL EXTRACTION RATE-------------


##################################################################
##########################FUNCTIONS###############################
#################################################################

send_mail <- function(from = NA, to = NA, subject = NA,
                      body = NA, remove = FALSE) {
  
  if (missing(from)) from <- 'no-reply@fao.org'
  
  if (missing(to)) {
    if (exists('swsContext.userEmail')) {
      to <- swsContext.userEmail
    }
  }
  
  if (is.null(to)) {
    stop('No valid email in `to` parameter.')
  }
  
  if (missing(subject)) stop('Missing `subject`.')
  
  if (missing(body)) stop('Missing `body`.')
  
  if (length(body) > 1) {
    body <-
      sapply(
        body,
        function(x) {
          if (file.exists(x)) {
            # https://en.wikipedia.org/wiki/Media_type 
            file_type <-
              switch(
                tolower(sub('.*\\.([^.]+)$', '\\1', basename(x))),
                txt  = 'text/plain',
                csv  = 'text/csv',
                png  = 'image/png',
                jpeg = 'image/jpeg',
                jpg  = 'image/jpeg',
                gif  = 'image/gif',
                xls  = 'application/vnd.ms-excel',
                xlsx = 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
                doc  = 'application/msword',
                docx = 'application/vnd.openxmlformats-officedocument.wordprocessingml.document',
                pdf  = 'application/pdf',
                zip  = 'application/zip',
                # https://stackoverflow.com/questions/24725593/mime-type-for-serialized-r-objects
                rds  = 'application/octet-stream'
              )
            
            if (is.null(file_type)) {
              stop(paste(tolower(sub('.*\\.([^.]+)$', '\\1', basename(x))),
                         'is not a supported file type.'))
            } else {
              return(sendmailR:::.file_attachment(x, basename(x), type = file_type))
            }
            
            if (remove) {
              unlink(x)
            }
          } else {
            return(x)
          }
        }
      )
  } else if (!is.character(body)) {
    stop('`body` should be either a string or a list.')
  }
  
  sendmailR::sendmail(from, to, subject, as.list(body))
}

calculateImbalance <- function(data,
                               supply_add = c("production", "imports"),
                               supply_subtract = c("exports", "stockChange"),
                               supply_all = union(supply_add, supply_subtract),
                               item_name = "measuredItemFbsSua",
                               bygroup = c("geographicAreaM49", "timePointYears", item_name),
                               keep_supply = TRUE,
                               keep_utilizations = TRUE) {
  
  stopifnot(is.data.table(data))
  
  data[measuredElementSuaFbs %!in% c("residual","TotalCalories","TotalProteins",
                                     "TotalFats","calories","proteins","fats"),
       `:=`(
         supply =
           sum(Value[measuredElementSuaFbs %chin% supply_add],
               - Value[measuredElementSuaFbs %chin% supply_subtract],
               na.rm = TRUE),
         # All elements that are NOT supply elements
         utilizations =
           sum(Value[!(measuredElementSuaFbs %chin% supply_all)],
               na.rm = TRUE)
       ),
       by = bygroup
       ][,
         imbalance := supply - utilizations
         ]
  
  if (keep_supply == FALSE) {
    data[, supply := NULL]
  }
  
  if (keep_utilizations == FALSE) {
    data[, utilizations := NULL]
  }
  
}


RemainingToProcessedParent <- function(data) {
  data[, 
       parent_already_processed :=
         ifelse(
           is.na(parent_qty_processed),
           parent_qty_processed,
           sum(processed_to_child / extractionRate, na.rm = TRUE)
         ),
       by = c("geographicAreaM49", "measuredItemParentCPC", "timePointYears")
       ]
  
  data[, remaining_processed_parent := round(parent_qty_processed - parent_already_processed)]
  
  data[remaining_processed_parent < 0, remaining_processed_parent := 0]
  
  data[,
       only_child_left :=
         sum(is.na(processed_to_child)) == 1 &
         is.na(processed_to_child) &
         !is.na(production_of_child) &
         !is.na(parent_qty_processed) &
         production_of_child > 0,
       by = c("geographicAreaM49", "measuredItemParentCPC", "timePointYears")
       ]
  
  data[
    only_child_left == TRUE,
    processed_to_child := remaining_processed_parent * extractionRate
    ]
  
  data[,
       parent_already_processed :=
         ifelse(
           is.na(parent_qty_processed),
           parent_qty_processed,
           sum(processed_to_child / extractionRate, na.rm = TRUE)
         ),
       by = c("geographicAreaM49", "measuredItemParentCPC", "timePointYears")
       ]
  
  data[, remaining_processed_parent := round(parent_qty_processed - parent_already_processed)]
  
  data[remaining_processed_parent < 0, remaining_processed_parent := 0]
  
  return(data)
}


RemainingProdChildToAssign <- function(data) {
  
  data[,
       available_processed_child := sum(processed_to_child, na.rm = TRUE),
       by = c("geographicAreaM49", "measuredItemChildCPC", "timePointYears")
       ]
  
  data[, remaining_to_process_child := round(production_of_child - available_processed_child)]
  
  data[remaining_to_process_child < 0, remaining_to_process_child := 0]
  
  data[,
       only_parent_left :=
         sum(is.na(processed_to_child)) == 1 &
         is.na(processed_to_child) &
         !is.na(parent_qty_processed) &
         parent_qty_processed >= 0
       ]
  
  data[only_parent_left==TRUE,processed_to_child:=ifelse(only_parent_left==TRUE,remaining_to_process_child,processed_to_child)]
  
  data[,
       available_processed_child := sum(processed_to_child, na.rm = TRUE),
       by = c("geographicAreaM49", "measuredItemChildCPC", "timePointYears")
       ]
  
  data[, remaining_to_process_child := round(production_of_child - available_processed_child)]
  
  data[remaining_to_process_child < 0, remaining_to_process_child := 0]
  
  return(data)
}

# Replacement for merge(x, y, by = VARS, all.x = TRUE) that do not set keys
# By default it behaves as dplyr::left_join(). If nomatch = 0, non-matching
# rows will not be returned
dt_left_join <- function(x, y, by = NA, allow.cartesian = FALSE,
                         nomatch = NA) {
  if (anyNA(by)) {
    stop("'by' is required")
  }
  
  if (any(!is.data.table(x), !is.data.table(y))) {
    stop("'x' and 'y' should be data.tables")
  }
  
  res <- y[x, on = by, allow.cartesian = allow.cartesian, nomatch = nomatch]
  
  setcolorder(res, c(names(x), setdiff(names(y), names(x))))
  
  res
}


standardizeTree = function(data, tree, elements, standParams,zeroWeight=c(),
                           sugarHack = TRUE){
  
  ## Assign parameters
  geoVar = standParams$geoVar
  yearVar = standParams$yearVar
  itemVar = standParams$itemVar
  elementPrefix = standParams$elementPrefix
  childVar = standParams$childVar
  parentVar = standParams$parentVar
  extractVar = standParams$extractVar
  shareVar = standParams$shareVar
  protected = standParams$protected
  
  ## Data Quality Checks
  stopifnot(is(data, "data.table"))
  stopifnot(is(tree, "data.table"))
  stopifnot(c(geoVar, yearVar, itemVar, paste0(elementPrefix, elements)) %in%
              colnames(data))
  stopifnot(c(geoVar, yearVar, childVar, parentVar, extractVar, shareVar)
            %in% colnames(tree))
  if(!all(sapply(data[, paste0(elementPrefix, c(elements)), with = FALSE],
                 is.numeric))){
    stop("Some of the elements passed are not numeric!")
  }
  if(!"target" %in% colnames(tree)){
    tree[, target := "B"]
  }
  stopifnot(all(tree[, target] %in% c("B", "T", "F")))
  if(!"standDev" %in% colnames(tree)){
    returnStandDev = FALSE
    tree[, standDev := 0]
  } else {
    returnStandDev = TRUE
  }
  stopifnot(all(tree[, standDev] >= 0))
  
  elements = paste0(elementPrefix, elements)
  
  ## Restructure the data for easier standardization
  standardizationData = data.table::melt.data.table(
    data = data, measure.vars = elements,
    id.vars = c(geoVar, yearVar, itemVar,protected),
    variable.name = "measuredElement", value.name = "Value")
  standardizationData[, measuredElement :=
                        gsub(elementPrefix, "", measuredElement)]
  
  ## To ensure commodities are standardized up multiple levels, we have to
  ## collapse the tree (otherwise if A -> B -> C in the tree, C may be
  ## standardized only to B and not to A, as desired).
  standKey = standParams$mergeKey[standParams$mergeKey != standParams$itemVar]
  if(dim(tree)[1]!=0){
    tree = collapseEdges(edges = tree, parentName = standParams$parentVar,
                         childName = standParams$childVar,
                         extractionName = standParams$extractVar,
                         keyCols = standKey)
    
    ## Merge the tree with the node data
    tree[, c(parentVar, childVar, yearVar, geoVar) :=
           list(as.character(get(parentVar)), as.character(get(childVar)),
                as.character(get(yearVar)), as.character(get(geoVar)))]
  }
  setnames(standardizationData, itemVar, childVar)
  standardizationData[, c(childVar, yearVar, geoVar) :=
                        list(as.character(get(childVar)),
                             as.character(get(yearVar)),
                             as.character(get(geoVar)))]
  
  ## To deal with joint byproducts
  
  standardizationData = merge(standardizationData, tree,
                              by = c(yearVar, geoVar, childVar),
                              all.x = TRUE, allow.cartesian = TRUE)
  
  ##' If an element is not a child in the tree, then "standardize" it to
  ##' itself with a rate of 1 and a share of 1.
  standardizationData[is.na(get(parentVar)),
                      c(parentVar, extractVar, shareVar) :=
                        list(get(childVar), 1, 1)]
  
  
  standardizationData[,weight:=1]
  standardizationData[measuredItemChildCPC %in% zeroWeight , weight:=0]
  
  ## Standardizing backwards is easy: we just take the value, divide by the 
  ## extraction rate, and multiply by the shares.  However, we don't 
  ## standardize the production element (because production of flour is 
  ## derived from the production of wheat already).  We standardize everything
  ## backwards, and then edges marked as forwards (i.e. target == "F") get
  ## standardized down.
  
  # Extraction rates of zero will cause us to divide by zero, so we must
  # remove them
  extract0 <- standardizationData[abs(get(extractVar)) < .Machine$double.eps ^ .5]
  if(nrow(extract0) > 0){
    # Check for tricky floating point issues, but checking if equal to 0
    warning(sprintf("Extraction rates of 0 present in commodity codes: {%s} in country {%s}.
                    Ignoring all extraction rates of 0 in backwards standardization",
                    paste0(unique(extract0[, get(parentVar)]), collapse = ", "), unique(extract0[, get(geoVar)])))
    standardizationData[abs(get(extractVar)) < .Machine$double.eps^.5, Value := NA]
  }
  output = standardizationData[, list(
    Value = sum( Value  *    weight   /get(extractVar)*get(shareVar), na.rm = TRUE)),
    by = c(yearVar, geoVar,
           "measuredElement", parentVar)]
  
  forwardEdges = tree[target == "F", ]
  
  ## Reshape to put back into the same shape as the passed data
  setnames(output, parentVar, itemVar)
  output[, measuredElement := paste0(elementPrefix,
                                     measuredElement)]
  form = as.formula(paste(yearVar, "+", geoVar, "+", itemVar, "~ measuredElement"))
  output = dcast.data.table(data = output, formula = form, value.var = "Value",
                            fun.aggregate = mean, na.rm = TRUE)
  return(output)
}


#TO DO: move this function bellow to the R folder and commit in github

collapseEdges_NEW = function(edges, parentName = "parentID",
                             childName = "childID",
                             extractionName = "extractionRate",
                             keyCols = c("timePointYearsSP", "geographicAreaFS"),
                             notStandChild,weight="weight",standard_child="standard_child"){
  
  ## Data quality checks
  stopifnot(is(edges, "data.table"))
  stopifnot(c(parentName, childName, extractionName) %in% colnames(edges))
  if(max(edges[[extractionName]][edges[[extractionName]] < Inf]) > 100)
    stop("Extraction rates larger than 100 indicate they are probably ",
         "expressed in different units than on [0,1].  This will cause ",
         "huge problems when multiplying, and should be fixed.")
  #cyclic parent-child relation bring infinit loop
  
  edges<-edges[!(get(parentName)=="21529.03" & get(childName)=="21523")]
  
  ## Test for loops
  # findProcessingLevel(edgeData = edges, from = parentName,
  #                     to = childName)
  
  nonstand<-edges[get(parentName) %in% notStandChild] 
  
  nonstand<-nonstand[,get(parentName)]
  
  targetNodes = setdiff(edges[[parentName]], edges[[childName]])
  targetNodes=unique(c(targetNodes,nonstand))
  
  edgesCopy = copy(edges[, c(parentName, childName, extractionName,p$shareVar,weight,standard_child,
                             keyCols), with = FALSE])
  
  setnames(edgesCopy, c(parentName, childName, extractionName,p$shareVar,weight,standard_child),
           c("newParent", parentName, "extractionMult","share.parent","weight.Parent","standard_child.parent"))
  finalEdges = edges[get(parentName) %in% targetNodes, ]
  currEdges = edges[!get(parentName) %in% targetNodes, ]
  
  while(nrow(currEdges) > 0){
    currEdges = merge(currEdges, edgesCopy, by = c(parentName, keyCols),
                      all.x = TRUE, allow.cartesian = TRUE)
    ## Update edges table with new parents/extraction rates.  For edges that
    ## didn't get changed, we keep the old parent name and extraction rate.
    
    currEdges[, c(p$shareVar) := get(p$shareVar) *(share.parent/sum(share.parent,na.rm = TRUE)),
              by=c(p$geoVar,p$yearVar,childName,parentName)
              ]
    
    currEdges[, c(parentName) := ifelse(is.na(newParent), get(parentName),
                                        newParent)]
    currEdges[, c(extractionName) := get(extractionName) *
                ifelse(is.na(extractionMult), 1, extractionMult)]
    
    
    # currEdges[,c(weight):=ifelse(!is.na(weight.Parent),weight.Parent,get(weight))]
    currEdges[,c(weight):=ifelse(weight.Parent==0,weight.Parent,weight)]
    currEdges[,c(standard_child):=ifelse(standard_child.parent==TRUE & standard_child==TRUE,
                                         TRUE,FALSE)]
    
    currEdges[, c("newParent", "extractionMult","share.parent","weight.Parent","standard_child.parent") := NULL]
    
    finalEdges = rbind(finalEdges, currEdges[get(parentName) %in% targetNodes, ])
    currEdges = currEdges[!get(parentName) %in% targetNodes, ]
  }
  
  finalEdges = unique(finalEdges,by=colnames(finalEdges))
  
  # finalEdges[,standard_child:=ifelse(get(childName) %in% notStandChild,FALSE,TRUE)]
  
  finalEdges = unique(finalEdges,by=colnames(finalEdges))
  
  return(finalEdges)
}


computeFbsAggregate = function(data, fbsTree, standParams){
  ## Data Quality Checks
  stopifnot(standParams$itemVar %in% colnames(data))
  stopifnot(standParams$itemVar %in% colnames(fbsTree))
  stopifnot(paste0("fbsID", 1:4) %in% colnames(fbsTree))
  
  fbsTree[measuredItemSuaFbs=="23670.01",fbsID4:=2542]
  fbsTree[measuredItemSuaFbs=="23670.01",fbsID2:=2903]   
  
  if(data[,unique(geographicAreaM49)]%in%c("72")){
    fbsTree[measuredItemSuaFbs=="23670.01",fbsID4:=2543]
    fbsTree[measuredItemSuaFbs=="23670.01",fbsID2:=2903]
  }
  data = merge(data, fbsTree, by = standParams$itemVar)
  out = list()
  
  out[[1]] = data[,list(Value = sum(Value, na.rm = TRUE)),
                  by = c(standParams$elementVar, standParams$yearVar,
                         standParams$geoVar, "fbsID4")]
  
  out[[2]] = data[, list(Value = sum(Value, na.rm = TRUE)),
                  by = c(standParams$elementVar, standParams$yearVar,
                         standParams$geoVar, "fbsID3")]
  
  out[[3]] = data[ get(standParams$elementVar) %in% c(standParams$calories, 
                                                      standParams$proteins,
                                                      standParams$fats,
                                                      "TotalCalories",
                                                      "TotalProteins",
                                                      "TotalFats"), 
                   list(Value = sum(Value, na.rm = TRUE)),
                   by = c(standParams$elementVar, standParams$yearVar,
                          standParams$geoVar, "fbsID2")]
  
  out[[4]] = data[get(standParams$elementVar) %in% c(standParams$calories, 
                                                     standParams$proteins,
                                                     standParams$fats,
                                                     "TotalCalories",
                                                     "TotalProteins",
                                                     "TotalFats"), 
                  list(Value = sum(Value, na.rm = TRUE)),
                  by = c(standParams$elementVar, standParams$yearVar,
                         standParams$geoVar, "fbsID1")]
  return(out)
}


getShareUpDownTree = function(geographicAreaM49 = NULL, timePointYears = NULL){
  
  treeelemKeys = c("5431")
  treeitemPKeys = GetCodeList(domain = "suafbs", dataset = "up_down_share", "measuredItemParentCPC_tree")
  treeitemPKeys = treeitemPKeys[, code]
  
  treeitemCKeys = GetCodeList(domain = "suafbs", dataset = "up_down_share", "measuredItemChildCPC_tree")
  treeitemCKeys = treeitemCKeys[, code]
  
  treekey = faosws::DatasetKey(domain = "suafbs", dataset = "up_down_share", dimensions = list(
    geographicAreaM49 = Dimension(name = "geographicAreaM49", keys = geographicAreaM49),
    measuredElementSuaFbs = Dimension(name = "measuredElementSuaFbs", keys = treeelemKeys),
    measuredItemParentCPC = Dimension(name = "measuredItemParentCPC_tree", keys = treeitemPKeys),
    measuredItemChildCPC = Dimension(name = "measuredItemChildCPC_tree", keys = treeitemCKeys),
    timePointYears = Dimension(name = "timePointYears", keys = timePointYears)
  ))
  
  ## Extract the specific tree
  
  ShareUpDownTree = faosws::GetData(treekey,omitna = FALSE)
  
  
  ShareUpDownTree[measuredElementSuaFbs=="5431",measuredElementSuaFbs:="shareUpDown"]
  
  message("ShareUpDownTree correctly downloaded")
  
  setnames(ShareUpDownTree,c("measuredItemParentCPC_tree","measuredItemChildCPC_tree"),
           c("measuredItemParentCPC","measuredItemChildCPC"))
  
  return(ShareUpDownTree)  
  
}

#Function that allow to compute the list of child commodities that
#should not be standardized (because they are not in the same fbstree than their parent)

NonStandardizedChidren<-function(fbsTree,tree,standParams){
  
  ## Data Quality Checks
  stopifnot(standParams$parentVar %in% colnames(tree))
  stopifnot(standParams$childVar %in% colnames(tree))
  stopifnot(standParams$itemVar %in% colnames(fbsTree))
  stopifnot(paste0("fbsID", 1:4) %in% colnames(fbsTree))
  
  
  fbstreebis<-copy(fbsTree)
  
  #FSB group of parents
  setnames(fbstreebis,"measuredItemSuaFbs",standParams$parentVar)
  
  treeFBSmerge<-merge(
    tree,
    fbstreebis,
    by=c(standParams$parentVar),
    allow.cartesian = TRUE,
    all.x = TRUE
  )
  
  setnames(treeFBSmerge,"fbsID4","fbsID4_parent")
  treeFBSmerge[,`:=`(fbsID3=NULL,fbsID2=NULL,fbsID1=NULL)]
  
  
  #FSB group of child commdities
  setnames(fbstreebis,standParams$parentVar,standParams$childVar)
  treeFBSmerge<-merge(
    treeFBSmerge,
    fbstreebis,
    by=c(standParams$childVar),
    allow.cartesian = TRUE,
    all.x = TRUE
  )
  
  setnames(treeFBSmerge,"fbsID4","fbsID4_child")
  treeFBSmerge[,`:=`(fbsID3=NULL,fbsID2=NULL,fbsID1=NULL)]
  
  #this variable takes TRUE if the child should be standardized ( have the same 
  #FBS tree than the parent)
  treeFBSmerge[,standard_child:=ifelse(fbsID4_parent==fbsID4_child,TRUE,FALSE)]
  
  #tree[is.na(standard_child),standard_child:=FALSE]
  treeFBSmerge<-#unique(
    treeFBSmerge[,list(geographicAreaM49,timePointYears,measuredItemParentCPC,measuredItemChildCPC,standard_child)]
  #by=c("measuredItemChildCPC","standard_child")
  #)
  treefbs<-unique(treeFBSmerge, by=c(colnames(treeFBSmerge)))
  #there some cases where the multiparent child commodity have 2 parents and is in the FBS
  #group than only 1 parent. In that case it is comnsidered as standardized (even if for the other parent it will be false)
  #it is the case starch of potaote that has 2 parent: potaote ans sweet potatoe but is the same FBS group than potatoe
  # 
  # treeFBSmerge[,som:=sum(standard_child,na.rm = TRUE),
  #           by=c("measuredItemChildCPC")] 
  # 
  # #keep only child commodities that should not be standardized 
  # treeFBSmerge<-treeFBSmerge[som==0 & !is.na(standard_child)]
  # treeFBSmerge[,som:=NULL]
  
  # output<-treeFBSmerge #[,get(p$childVar)]
  
  return(treeFBSmerge)
}


# rollavg() is a rolling average function that uses computed averages
# to generate new values if there are missing values (and FOCB/LOCF).
# I.e.:
# vec <- c(NA, 2, 3, 2.5, 4, 3, NA, NA, NA)
#
#> RcppRoll::roll_mean(myvec, 3, fill = 'extend', align = 'right')
#[1]       NA       NA       NA 2.500000 3.166667 3.166667       NA       NA       NA 
#
#> rollavg(myvec)
#[1] 2.000000 2.000000 3.000000 2.500000 4.000000 3.000000 3.166667 3.388889 3.185185

# rollavg <- function(x, order = 3) {
#   # order should be > 2
#   stopifnot(order >= 3)
#   
#   non_missing <- sum(!is.na(x))
#   
#   # For cases that have just two non-missing observations
#   order <- ifelse(order > 2 & non_missing == 2, 2, order)
#   
#   if (non_missing == 1) {
#     x[is.na(x)] <- na.omit(x)[1]
#   } else if (non_missing >= order) {
#     n <- 1
#     while(any(is.na(x)) & n <= 10) { # 10 is max tries
#       movav <- suppressWarnings(RcppRoll::roll_mean(x, order, fill = 'extend', align = 'right'))
#       movav <- data.table::shift(movav)
#       x[is.na(x)] <- movav[is.na(x)]
#       n <- n + 1
#     }
#     
#     x <- zoo::na.fill(x, 'extend')
#   }
#   
#   return(x)
# }

`%!in%` = Negate(`%in%`)


###########END FUNCTION--------------------------------------------
#QUick way to have the exact shareDownUp
#LoadShareUpDowm
shareUpDownTree=getShareUpDownTree(areaKeys,as.character(2000:2017)) 
shareUpDownTree<-shareUpDownTree[!is.na(Value)]
shareUpDownTree[,shareUpDown:=Value]
shareUpDownTree[,Value:=NULL]
shareUpDownTree[,flagObservationStatus:=NULL]
shareUpDownTree[,flagMethod:=NULL]


##############################################################
################### MARK OILS COMMODITY ######################
##############################################################

oilFatsCPC=c("2161", "2162", "21631.01", "21641.01", "21641.02", "2168",
             "21691.14", "2165", "34120", "21932.02", "2166", "21691.07",
             "2167", "21673", "21691.01", "21691.02", "21691.03", "21691.04",
             "21691.05", "21691.06", "21631.02", "21691.08", "21691.09", "21691.10",
             "21691.11", "21691.12", "21691.13", "21691.90", "23620", "21700.01",
             "21700.02", "21693.02", "34550", "F1275", "21512", "21512.01",
             "21513", "21514", "F0994", "21515", "21511.01", "21511.02", "21521",
             "21511.03", "21522", "21519.02", "21519.03", "21529.03", "21529.02",
             "21932.01", "21523", "F1243", "F0666")

tree[(measuredItemParentCPC%in%oilFatsCPC|measuredItemChildCPC%in%oilFatsCPC),oil:=TRUE]

##############################################################
################ CLEAN NOT OFFICIAL SHARES ###################
################   & SHARES EXCEPT  OILS   ###################
##############################################################

## (E,f) have to be kept
## any other has to ve cleaned except the oils 
tree[,checkFlags:=paste0("(",flagObservationStatus,",",flagMethod,")")]

tree[measuredElementSuaFbs=="share"&(checkFlags=="(E,f)"|oil==TRUE),keep:=TRUE]
tree[measuredElementSuaFbs=="share"&is.na(keep),Value:=NA]
tree<-tree[timePointYears %in% yearVals,]

tree[,checkFlags:=NULL]
tree[,oil:=NULL]
tree[,keep:=NULL]

##############################################################
############ Set parameters for specific dataset #############
##############################################################

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
params$calories = "calories"
params$proteins = "proteins"
params$fats = "fats"


##############################################################
######## CLEAN ALL SESSION TO BE USED IN THE PROCESS #########
##############################################################
## CLEAN fbs_standardized
message("wipe fbs_standardized session")

CONFIG <- GetDatasetConfig(sessionKey_fbsStand@domain, sessionKey_fbsStand@dataset)

fbs_standardized=GetData(sessionKey_fbsStand)
fbs_standardized=fbs_standardized[timePointYears%in%yearVals]

fbs_standardized[, Value := NA_real_]
fbs_standardized[, CONFIG$flags := NA_character_]

SaveData(CONFIG$domain, CONFIG$dataset , data = fbs_standardized, waitTimeout = Inf)

## CLEAN fbs_balanced
message("wipe fbs_balanced session")

CONFIG <- GetDatasetConfig(sessionKey_fbsBal@domain, sessionKey_fbsBal@dataset)

fbs_balancedData=GetData(sessionKey_fbsBal)
fbs_balancedData=fbs_balancedData[timePointYears%in%yearVals]

fbs_balancedData[, Value := NA_real_]
fbs_balancedData[, CONFIG$flags := NA_character_]

# SaveData(CONFIG$domain, CONFIG$dataset , data = fbs_balancedData, waitTimeout = Inf)

##############################################################
#################### SET KEYS FOR DATA #######################
##############################################################

elemKeys=c("5510", "5610", "5071", "5023", "5910", "5016", 
           "5165", "5520","5525","5164","5166","5141")

# 5510 Production[t]
# 5610 Import Quantity [t]
# 5071 Stock Variation [t]
# 5023 Export Quantity [t]
# 5910 Loss [t]
# 5016 Industrial uses [t]
# 5165 Feed [t]
# 5520 Seed [t]
# 5525 Tourist Consumption [t]
# 5164 Residual other uses [t]
# 5166 Food [t]
# 5141 Food Supply (/capita/day) [Kcal]

#desKeys = c("664","674","684")
desKeys = c("664") #TODO GENERATE FATES AND PROTEINS IN THE sua balanced

# 664 Food Supply (Kcal/caput/day) [kcal]
# 674 Protein Supply quantity (g/caput/day) [g]
# 684 Fat supply quantity (g/caput/day) [g]

itemKeys = GetCodeList(domain = "suafbs", dataset = "sua_balanced", "measuredItemFbsSua")

itemKeys = itemKeys[, code]

key = DatasetKey(domain = "suafbs", dataset = "sua_unbalanced", dimensions = list(
  geographicAreaM49 = Dimension(name = "geographicAreaM49", keys = areaKeys),
  measuredElementSuaFbs = Dimension(name = "measuredElementSuaFbs", keys = elemKeys),
  measuredItemFbsSua = Dimension(name = "measuredItemFbsSua", keys = itemKeys),
  timePointYears = Dimension(name = "timePointYears", keys = as.character(2000:2017))))

# key = DatasetKey(domain = "suafbs", dataset = "sua_balanced", dimensions = list(
#   geographicAreaM49 = Dimension(name = "geographicAreaM49", keys = areaKeys),
#   measuredElementSuaFbs = Dimension(name = "measuredElementSuaFbs", keys = elemKeys),
#   measuredItemFbsSua = Dimension(name = "measuredItemFbsSua", keys = itemKeys),
#   timePointYears = Dimension(name = "timePointYears", keys = yearVals)))
# 
# keyDes = DatasetKey(domain = "suafbs", dataset = "sua_balanced", dimensions = list(
#   geographicAreaM49 = Dimension(name = "geographicAreaM49", keys = areaKeys),
#   measuredElementSuaFbs = Dimension(name = "measuredElementSuaFbs", keys = desKeys),
#   measuredItemFbsSua = Dimension(name = "measuredItemFbsSua", keys = itemKeys),
#   timePointYears = Dimension(name = "timePointYears", keys = yearVals)))


##############################################################
####################### DOWNLOAD  DATA #######################
##############################################################

message("Reading SUA data...")
#get sua bal Data
CONFIG <- GetDatasetConfig(sessionKey_suabal@domain, sessionKey_suabal@dataset)
SuabalData=GetData(sessionKey_suabal)
SuabalData=SuabalData[timePointYears%in%yearVals]

# SuabalData=SuabalData[geographicAreaM49 %in% c("360")]


setnames(SuabalData,"measuredItemFbsSua",params$itemVar)

#Sua balanced quantitise
data=SuabalData[get(params$elementVar) %in% elemKeys,]
data=elementCodesToNames(data,standParams = params)

data[measuredElementSuaFbs=="foodmanufacturing",measuredElementSuaFbs:="foodManufacturing"]
data[measuredElementSuaFbs=="stock_change",measuredElementSuaFbs:="stockChange"]
#data[measuredElementSuaFbs=="stock",measuredElementSuaFbs:="stockChange"]
message("delete null elements")
data=data[!is.na(measuredElementSuaFbs)]

data_residual<-data[measuredElementSuaFbs %!in% c("residual")]
setnames(data_residual,"measuredItemSuaFbs" ,"measuredItemFbsSua")
calculateImbalance(data=data_residual,keep_supply = FALSE,keep_utilizations = FALSE)

data_residual[,`:=`(Value=imbalance,
                    measuredElementSuaFbs="residual",
                    flagObservationStatus="I",
                    flagMethod="i",
                    imbalance=NULL)]

setnames(data_residual,"measuredItemFbsSua","measuredItemSuaFbs")

data_residual<-unique(data_residual, by=colnames(data))

data<-rbind(data[measuredElementSuaFbs %!in% c("residual")],data_residual)

#Sua balanced DES
# dataDes<-SuabalData[get(params$elementVar) %in% desKeys]
# dataDes[get(params$elementVar)=="664",params$elementVar:=params$calories]

message("load SUA unbal data...")

# dataSuaUn = elementCodesToNames(data = GetData(key), itemCol = "measuredItemFbsSua",
#                                 elementCol = "measuredElementSuaFbs")


#########SHARE DOWNUP   ----------------------------------------------

############################nEW WAY TO GENERATE SHAREDOWN UP--------------------------------------
message("derivation of shareDownUp")

p<-params
############# ShareDownUp ----------------------------------------

#this table will be used to assign to zeroweight comodities 
#the processed quantities of their coproduct
coproduct_table <- ReadDatatable('zeroweight_coproducts')
zeroWeight=ReadDatatable("zero_weight")[,item_code]


stopifnot(nrow(coproduct_table) > 0)

# Can't do anything if this information if missing, so remove these cases
coproduct_table <- coproduct_table[!is.na(branch)]

### XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
### XXX removing these cases as '22242.01' appears as zero-weight XXXX
### XXX for main product = '22110.04'                             XXXX
### XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
coproduct_table <- coproduct_table[branch != '22242.01 + 22110.04']

coproduct_table <- coproduct_table[, .(measured_item_child_cpc, branch)]

coproduct_for_sharedownup <- copy(coproduct_table)

coproduct_for_sharedownup_easy <- coproduct_for_sharedownup[!grepl('\\+|or', branch)]

coproduct_for_sharedownup_plus <- coproduct_for_sharedownup[grepl('\\+', branch)]

coproduct_for_sharedownup_plus <-
  rbind(
    tidyr::separate(
      coproduct_for_sharedownup_plus,
      branch,
      into = c('main1', 'main2'),
      remove = FALSE,
      sep = ' *\\+ *')[, .(measured_item_child_cpc, branch= main1)],
    tidyr::separate(
      coproduct_for_sharedownup_plus,
      branch,
      into = c('main1', 'main2'),
      remove = FALSE,
      sep = ' *\\+ *')[, .(measured_item_child_cpc,branch = main2)]
  )

coproduct_for_sharedownup_plus <- unique(coproduct_for_sharedownup_plus)


coproduct_for_sharedownup_or <- coproduct_for_sharedownup[grepl('or', branch)]

coproduct_for_sharedownup_or <-
  rbind(
    #coproduct_table_or,
    tidyr::separate(
      coproduct_for_sharedownup_or,
      branch,
      into = c('main1', 'main2'),
      remove = FALSE,
      sep = ' *or *')[, .(measured_item_child_cpc, branch= main1)],
    tidyr::separate(
      coproduct_for_sharedownup_or,
      branch,
      into = c('main1', 'main2'),
      remove = FALSE,
      sep = ' *or *')[, .(measured_item_child_cpc,branch = main2)]
  )

coproduct_for_sharedownup_or <- unique(coproduct_for_sharedownup_or)

coproduct_for_sharedownup <-
  rbind(
    coproduct_for_sharedownup_easy,
    coproduct_for_sharedownup_plus,
    coproduct_for_sharedownup_or
  )

# tree<-tree[geographicAreaM49=="360" & timePointYears>2013]

#Using the whole tree not by level
ExtrRate <-
  tree[
    !is.na(Value) &
      measuredElementSuaFbs == 'extractionRate'
    ][,
      .(
        measuredItemParentCPC,
        geographicAreaM49,
        measuredItemChildCPC,
        timePointYears,
        extractionRate = Value
      )
      ]

# We include utilizations to identify if proceseed if the only utilization
data_tree <-
  data[
    measuredElementSuaFbs %chin%
      c('production', 'imports', 'exports',
        'stockChange', 'foodManufacturing', 'loss',
        'food', 'industrial', 'feed', 'seed')
    ]


#subset the tree accordingly to parents and child present in the SUA data

# ExtrRate <-
#   ExtrRate[
#     measuredItemChildCPC %chin% data_tree$measuredItemSuaFbs &
#       measuredItemParentCPC %chin% data_tree$measuredItemSuaFbs
#     ]


setnames(data_tree, "measuredItemSuaFbs", "measuredItemParentCPC")

data_tree <-
  merge(
    data_tree,
    ExtrRate,
    by = c(p$parentVar, p$geoVar, p$yearVar),
    allow.cartesian = TRUE,
    all.y = TRUE
  )

data_tree <- as.data.table(data_tree)

# the availability for parent that have one child and only processed as utilization will 
# be entirely assigned to processed for that its unique child even for 2014 onwards
data_tree[,
          availability :=
            sum(
              Value[get(p$elementVar) %in% c(p$productionCode, p$importCode)],
              - Value[get(p$elementVar) %in% c(p$exportCode, "stockChange")],
              na.rm = TRUE
            ),
          by = c(p$geoVar, p$yearVar, p$parentVar, p$childVar)
          ]

# used to chack if a parent has processed as utilization
data_tree[,
          proc_Median :=
            median(
              Value[measuredElementSuaFbs == "foodManufacturing" & timePointYears %in% 2000:2017],
              na.rm=TRUE
            ),
          by = c(p$parentVar, p$geoVar)
          ]

# boolean variable taking TRUE if the parent has only processed as utilization 
data_tree[,
          unique_proc :=
            proc_Median > 0 &
            !is.na(proc_Median) &
            # ... is the only utilization
            all(is.na(Value[!(measuredElementSuaFbs %chin%
                                c('production', 'imports', 'exports',
                                  'stockChange','foodManufacturing'))])),
          by = c(p$parentVar, p$geoVar, p$yearVar)
          ]

sel_vars <-
  c("measuredItemParentCPC", "geographicAreaM49", "timePointYears",
    "measuredElementSuaFbs", "flagObservationStatus", "flagMethod",
    "Value", "measuredItemChildCPC", "extractionRate"
  )

data_tree<-
  unique(
    data_tree[, c(sel_vars, "availability", "unique_proc"), with = FALSE],
    by = sel_vars
  )


# dataset to calculate the number of parent of each child and the number of children of each parent
# including zeroweight commodities
sel_vars <- c("geographicAreaM49", "measuredItemParentCPC",
              "measuredItemChildCPC","timePointYears")
data_count <- unique(data_tree[, sel_vars, with = FALSE], by = sel_vars)

#Caculate the number of parent of each child
data_count[,
           number_of_parent := .N,
           by = c("geographicAreaM49", "measuredItemChildCPC", "timePointYears")
           ]

# calculate the number of children of each parent
# we exclude zeroweight to avoid doublecounting of children (processing)
data_count[measuredItemChildCPC %!in% zeroWeight,
           number_of_children := uniqueN(measuredItemChildCPC),
           by = c("geographicAreaM49", "measuredItemParentCPC", "timePointYears")
           ]

data_tree <-
  dt_left_join(
    data_tree,
    data_count,
    by = c(p$parentVar, p$childVar, p$geoVar, p$yearVar),
    allow.cartesian = TRUE
  )

# dataset containing the processed quantity of parents
food_proc <-
  unique(
    data_tree[
      measuredElementSuaFbs == "foodManufacturing",
      c("geographicAreaM49", "measuredItemParentCPC", "timePointYears", "Value"),
      with = FALSE
      ],
    by = c("geographicAreaM49", "measuredItemParentCPC", "timePointYears", "Value")
  )

setnames(food_proc, "Value", "parent_qty_processed")

# avoid recaculation of shareDownUp from 2014 onwards
# food_proc[timePointYears > 2013, parent_qty_processed := NA_real_]

data_tree <-
  dt_left_join(
    data_tree,
    food_proc,
    by = c(p$parentVar, p$geoVar, p$yearVar),
    allow.cartesian = TRUE
  )

sel_vars <- 
  c("geographicAreaM49", "measuredItemParentCPC", "measuredItemChildCPC",
    "timePointYears", "extractionRate", "parent_qty_processed", "number_of_parent", "number_of_children")

data_tree <-
  unique(
    data_tree[, c(sel_vars, "availability", "unique_proc"), with = FALSE],
    by = sel_vars
  )


# dataset containing the production of child commodities
dataprodchild <- data[measuredElementSuaFbs %chin% c('production')]

setnames(dataprodchild, "measuredItemSuaFbs", "measuredItemChildCPC")

dataprodchild <-
  unique(
    dataprodchild[,
                  c("geographicAreaM49", "measuredItemChildCPC",
                    "timePointYears", "Value", "flagObservationStatus",
                    "flagMethod"),
                  with = FALSE
                  ]
  )

setnames(dataprodchild, "Value", "production_of_child")


# to avoid resestimation based on estimated data (processed and production of child) from 2014 onwards
# dataprodchild[timePointYears > 2013, production_of_child := NA_real_]

data_tree <-
  dt_left_join(
    data_tree,
    dataprodchild,
    by = c(p$geoVar, p$childVar, p$yearVar)
  )

# ShareDownups for zeroweights are calculated  separately
# to avoid double counting when agregating processed quantities of parent

# dataset containing informations of zeroweight commodities
data_zeroweight <- data_tree[measuredItemChildCPC %chin% zeroWeight]

# import data for coproduct relation
zw_coproduct <-
  coproduct_for_sharedownup[,
                            .(zeroweight = measured_item_child_cpc, measuredItemChildCPC = branch)
                            ]

zw_coproduct <- unique(zw_coproduct, by = c("measuredItemChildCPC", "zeroweight"))

# We subset the zeroweight coproduct reference table by taking only zeroweights and their coproduct
# that are childcommodities in the tree of the country
zw_coproduct <-
  zw_coproduct[
    measuredItemChildCPC %chin% data_tree$measuredItemChildCPC &
      zeroweight %chin% data_tree$measuredItemChildCPC
    ]


# Computing information for non zeroweight commodities
data_tree <- data_tree[measuredItemChildCPC %!in% zeroWeight]

# this dataset will be used when creating processing share and shareUpdown
dataComplete <- copy(data_tree)

# Quantity of parent destined to the production of the given child (only for child with one parent for the moment)
data_tree[, processed_to_child := ifelse(number_of_parent == 1, production_of_child, NA_real_)]

# if a parent has one child, all the production of the child comes from that parent
data_tree[
  number_of_children == 1,
  processed_to_child := parent_qty_processed * extractionRate,
  processed_to_child
  ]

data_tree[production_of_child == 0, processed_to_child := 0]

# assigning the entired availability to processed for parent having only processed as utilization
data_tree[
  number_of_children == 1 & unique_proc == TRUE,
  processed_to_child := availability * extractionRate
  ]


# mirror assignment for imputing processed quantity for multple parent children
# 5 loop is sufficient to deal with all the cases

for (k in 1:5) {
  data_tree <- RemainingToProcessedParent(data_tree)
  data_tree <- RemainingProdChildToAssign(data_tree)
}

data_tree <- RemainingToProcessedParent(data_tree)

# proportional allocation of the remaing production of multiple parent children
data_tree[,
          processed_to_child :=
            ifelse(
              number_of_parent > 1 & is.na(processed_to_child),
              (remaining_to_process_child * is.na(processed_to_child) * remaining_processed_parent) / sum((remaining_processed_parent * is.na(processed_to_child)), na.rm = TRUE),
              processed_to_child),
          by = c("geographicAreaM49", "measuredItemChildCPC", "timePointYears")
          ]

# Update of remaining production to assing ( should be zero for 2000:2013)
data_tree[,
          parent_already_processed :=
            ifelse(
              is.na(parent_qty_processed),
              parent_qty_processed,
              sum(processed_to_child / extractionRate,na.rm = TRUE)
            ),
          by = c("geographicAreaM49", "measuredItemParentCPC", "timePointYears")
          ]

data_tree[, remaining_processed_parent := round(parent_qty_processed - parent_already_processed)]

data_tree[
  remaining_processed_parent < 0,
  remaining_processed_parent := 0
  ]


# Impute processed quantity for 2014 onwards using 3 years average
# (this only to imput shareDownUp)
# data_tree <-
#   data_tree[
#     order(geographicAreaM49, measuredItemParentCPC, measuredItemChildCPC, timePointYears),
#     processed_to_child_avg := rollavg(processed_to_child, order = 3),
#     by = c("geographicAreaM49", "measuredItemParentCPC", "measuredItemChildCPC")
#     ]
# 
# setkey(data_tree, NULL)
# 
# data_tree[timePointYears > 2013 & is.na(processed_to_child), processed_to_child := processed_to_child_avg]


# Back to zeroweight cases(we assign to zeroweights the processed quantity of their coproduct(already calculated))

zw_coproduct_bis <-
  merge(
    data_tree,
    zw_coproduct,
    by = "measuredItemChildCPC",
    allow.cartesian = TRUE,
    all.y = TRUE
  )

zw_coproduct_bis[,
                 `:=`(
                   measuredItemChildCPC = zeroweight,
                   processed_to_child = processed_to_child / extractionRate
                 )
                 ]

sel_vars <- 
  c("geographicAreaM49", "measuredItemParentCPC",
    "measuredItemChildCPC", "timePointYears")

zw_coproduct_bis <-
  zw_coproduct_bis[, c(sel_vars, "processed_to_child"), with = FALSE]

# Correction to milk tree issue ( zeroweight can be associated with 2 main products from the same parent)
# example: butter of cow milk
zw_coproduct_bis[,
                 processed_to_child := sum(processed_to_child, na.rm = TRUE),
                 by = sel_vars 
                 ]

zw_coproduct_bis <-
  unique(zw_coproduct_bis, by = colnames(zw_coproduct_bis))

data_zeroweight <-
  dt_left_join(data_zeroweight, zw_coproduct_bis, by = sel_vars)

data_zeroweight[,
                processed_to_child := processed_to_child * extractionRate
                ]

sel_vars <-
  c("geographicAreaM49", "measuredItemParentCPC", "measuredItemChildCPC",
    "timePointYears", "number_of_parent", "parent_qty_processed",
    "production_of_child", "processed_to_child")

data_zeroweight <- data_zeroweight[, sel_vars, with = FALSE]

data_tree <- data_tree[, sel_vars, with = FALSE]

#combining zeroweight and non zero weight commodities
data_tree <- rbind(data_tree, data_zeroweight)


#Correction 

#calculate ShareDownUp
data_tree[,
          shareDownUp := processed_to_child / sum(processed_to_child, na.rm = TRUE),
          by = c("geographicAreaM49", "measuredItemChildCPC", "timePointYears")
          ]

#some corrections...

data_tree[is.na(shareDownUp) & number_of_parent == 1, shareDownUp := 1]

# data_tree[
#   (production_of_child == 0 | is.na(production_of_child)) &
#     measuredItemChildCPC %!in% zeroWeight &
#     timePointYears < 2014,
#   shareDownUp := 0
#   ]
# 
# data_tree[
#   (parent_qty_processed == 0 | is.na(parent_qty_processed)) &
#     timePointYears < 2014,
#   shareDownUp :=0
#   ]

data_tree[is.na(shareDownUp), shareDownUp := 0]
data_tree[,share:=shareDownUp]

data_tree <-
  unique(
    data_tree[geographicAreaM49 %!in% unique(shareUpDownTree$geographicAreaM49),
              c("geographicAreaM49", "measuredItemParentCPC",
                "measuredItemChildCPC", "timePointYears", "share"),
              with = FALSE         ]
  )

# data_tree<-rbind(data_tree_f1,data_tree_f2)
setDT(data_tree)


# calculating shareUpdown for each child
data_ShareUpDoawn <-
  dataComplete[,
               c("geographicAreaM49", "timePointYears", "measuredItemParentCPC",
                 "availability", "parent_qty_processed", "measuredItemChildCPC",
                 "extractionRate", "production_of_child", "number_of_children",
                 "number_of_parent"),
               with = FALSE
               ]

data_ShareUpDoawn <-
  unique(
    data_ShareUpDoawn,
    by = colnames(data_ShareUpDoawn)
  )

data_ShareUpDoawn <- merge(
  data_ShareUpDoawn,
  data_tree,
  by = c("geographicAreaM49", "measuredItemParentCPC",
         "measuredItemChildCPC", "timePointYears"),
  all = TRUE
)

setnames(data_ShareUpDoawn,"share","shareDownUp")
data_ShareUpDoawn <- data_ShareUpDoawn[measuredItemChildCPC %!in% zeroWeight]
data_ShareUpDoawn[, shareUpDown := NA_real_]

data_ShareUpDoawn[
  !is.na(parent_qty_processed) & extractionRate>0,
  shareUpDown := (production_of_child / extractionRate) * shareDownUp / sum(production_of_child / extractionRate * shareDownUp, na.rm = TRUE),
  by = c("geographicAreaM49", "timePointYears", "measuredItemParentCPC")
  ]

data_ShareUpDoawn[is.nan(shareUpDown), shareUpDown := NA_real_]

data_ShareUpDoawn[, shareUpDown:=ifelse(
  number_of_parent==1,
  shareUpDown / sum(shareUpDown[number_of_parent==1], na.rm = TRUE) *
    (1 - sum(shareUpDown[number_of_parent==1], na.rm = TRUE)),
  shareUpDown
),
by = c("geographicAreaM49","timePointYears","measuredItemParentCPC")
]

# data_ShareUpDoawn <-
#   data_ShareUpDoawn[
#     order(geographicAreaM49, measuredItemParentCPC, measuredItemChildCPC, timePointYears),
#     shareUpDown_avg := rollavg(shareUpDown, order = 3),
#     by = c("geographicAreaM49", "measuredItemParentCPC", "measuredItemChildCPC")
#     ]
# 
# setkey(data_ShareUpDoawn, NULL)
# 
# data_ShareUpDoawn[timePointYears > 2013, shareUpDown := shareUpDown_avg]
# 
# data_ShareUpDoawn[, shareUpDown_avg := NULL]
# 
# data_ShareUpDoawn[
#   timePointYears > 2013,
#   shareUpDown := shareUpDown / sum(shareUpDown, na.rm = TRUE),
#   by = c("geographicAreaM49", "timePointYears", "measuredItemParentCPC")
#   ]

sel_vars <-
  c("geographicAreaM49", "measuredItemParentCPC",
    "measuredItemChildCPC", "timePointYears", "shareUpDown")

data_ShareUpDoawn_final <- data_ShareUpDoawn[, sel_vars, with = FALSE]

shareUpDown_zeroweight <-
  merge(
    data_ShareUpDoawn_final,
    zw_coproduct,
    by = "measuredItemChildCPC",
    allow.cartesian = TRUE,
    all.y = TRUE
  )

shareUpDown_zeroweight[, measuredItemChildCPC := zeroweight]

shareUpDown_zeroweight <- shareUpDown_zeroweight[, sel_vars, with = FALSE]

# Correction to milk tree issue ( zeroweight can be associated with 2 main products from the same parent)
# example: butter of cow milk
shareUpDown_zeroweight<-
  shareUpDown_zeroweight[,
                         shareUpDown := sum(shareUpDown, na.rm = TRUE),
                         by = c("geographicAreaM49", "measuredItemParentCPC", "measuredItemChildCPC", "timePointYears")
                         ]

shareUpDown_zeroweight <-
  unique(
    shareUpDown_zeroweight,
    by = colnames(shareUpDown_zeroweight)
  )

# /correction


data_ShareUpDoawn_final <- rbind(data_ShareUpDoawn_final, shareUpDown_zeroweight)

# some correction
data_ShareUpDoawn_final[is.na(shareUpDown), shareUpDown := 0]
data_ShareUpDoawn_final[!is.na(shareUpDown), flagObservationStatus := "I"]
data_ShareUpDoawn_final[!is.na(shareUpDown), flagMethod := "c"]

# setnames(data_ShareUpDoawn_final,"measuredItemChildCPC","measuredElementSuaFbs")
data_ShareUpDoawn_final[,measuredElementSuaFbs:="shareUpDown"]
data_ShareUpDoawn_final[,flagObservationStatus:=NULL]
data_ShareUpDoawn_final[,flagMethod:=NULL]

data_ShareUpDoawn_final<-data_ShareUpDoawn_final[,colnames(shareUpDownTree),with=FALSE]

data_ShareUpDoawn_final<-unique(data_ShareUpDoawn_final,by=colnames(shareUpDownTree),with=FALSE)

shareUpDownTree<-rbind(
  shareUpDownTree,
  data_ShareUpDoawn_final[!shareUpDownTree, on=c("geographicAreaM49")]
  
)

######END NEW WAY TO GENERATE SHAREDOWNUP-------------------------------------------------




message("Calculating shareDownUps...")


#Using the whole tree not by level


# if (nrow(shareUpDownTree)>0){

ExtrRate <-
  tree[
    !is.na(Value) &
      measuredElementSuaFbs == 'extractionRate' &
      geographicAreaM49 %in% unique(shareUpDownTree$geographicAreaM49)
    ][,
      list(
        measuredItemParentCPC,
        geographicAreaM49,
        measuredItemChildCPC,
        timePointYears,
        extractionRate = Value
      )
      ]


data_tree <- data[measuredElementSuaFbs %chin% c('foodManufacturing') &
                    geographicAreaM49 %in% unique(shareUpDownTree$geographicAreaM49)]

# setnames(data_tree, "measuredItemFbsSua", "measuredItemParentCPC")

data_tree <-
  merge(
    data_tree[,list(geographicAreaM49,timePointYears,
                    ProcessedParent=Value,
                    measuredItemParentCPC=measuredItemSuaFbs)],
    ExtrRate,
    by = c(params$parentVar, params$geoVar, params$yearVar),
    allow.cartesian = TRUE,
    all.y = TRUE
  )

data_tree<-as.data.table(data_tree)

#dataset to calculate the number of parent of each child and the number of children of each parent
#including zeroweight commodities
data_count<-unique(
  data_tree[,
            .(geographicAreaM49,measuredItemParentCPC,measuredItemChildCPC,timePointYears)],
  by=c("geographicAreaM49","measuredItemParentCPC","measuredItemChildCPC","timePointYears")
)

#Caculate the number of parent of each child
data_count[,number_of_parent:=.N,
           by=c("geographicAreaM49","measuredItemChildCPC","timePointYears")
           ]

# #calculate the number of children of each parent
data_count[measuredItemChildCPC %!in% zeroWeight,number_of_children:=uniqueN(measuredItemChildCPC),
           by=c("geographicAreaM49","measuredItemParentCPC","timePointYears")
           ]

data_tree<-merge(
  data_tree,
  data_count,
  by = c(params$parentVar,params$childVar, params$geoVar, params$yearVar),
  allow.cartesian = TRUE,
  all.x = TRUE
)


data_tree<-data_tree[,list(geographicAreaM49,measuredItemParentCPC,measuredItemChildCPC,
                           timePointYears,extractionRate,ProcessedParent,
                           number_of_parent,number_of_children)]

data_tree<-unique(data_tree,by=c(colnames(data_tree)))


#dataset containing the production of child commodities
# dataprodchild <- dataSuaUn[measuredElementSuaFbs %chin% c('production')]
dataprodchild <- data[measuredElementSuaFbs %chin% c('production') &
                        geographicAreaM49 %chin% unique(shareUpDownTree$geographicAreaM49)]

# setnames(dataprodchild, "measuredItemFbsSua", "measuredItemChildCPC")
setnames(dataprodchild, "measuredItemSuaFbs", "measuredItemChildCPC")


dataprodchild<-
  unique(
    dataprodchild[,list(geographicAreaM49,measuredItemChildCPC,
                        timePointYears,Value,flagObservationStatus, flagMethod)],
    by=c("geographicAreaM49","measuredItemChildCPC","timePointYears",
         "Value","flagObservationStatus","flagMethod")
  )
setnames(dataprodchild, "Value", "production_of_child")


data_tree<-merge(
  data_tree,
  dataprodchild,
  by=c(params$geoVar,params$childVar,params$yearVar),
  all.x = TRUE
)


data_tree<-merge(
  data_tree,
  shareUpDownTree,
  by=c(params$geoVar,params$parentVar, params$childVar,params$yearVar),
  all.x = TRUE
)

data_tree<-data_tree[timePointYears>2013]


data_tree[,shareDownUp:=(ProcessedParent*shareUpDown*extractionRate)/
            sum(ProcessedParent*shareUpDown*extractionRate,na.rm = TRUE),
          by=c(params$geoVar,params$childVar,params$yearVar)
          ]
data_tree[is.na(shareDownUp) & number_of_parent==1,shareDownUp:=1]
data_tree[is.na(shareDownUp),shareDownUp:=0]

data_tree[,share:=shareDownUp]
data_tree[,shareDownUp:=NULL]
data_tree<-unique(
  data_tree[,
            list(geographicAreaM49, measuredItemParentCPC, measuredItemChildCPC, timePointYears, share)
            ],
  by=c("geographicAreaM49", "measuredItemParentCPC", "measuredItemChildCPC", "timePointYears", "share")
  
)

setDT(data_tree)
#   
# } else {
#   
#   data_tree_f1 <-
#     data.table(
#       geographicAreaM49 = character(),
#       timePointYears = character(),
#       measuredItemParentCPC = character(),
#       measuredItemChildCPC = character(),
#       share = numeric()
#     )
# }

##############################################################
######### SUGAR RAW CODES TO BE CONVERTED IN 2351F ###########
##############################################################
data=convertSugarCodes(data)

##############################################################
############### CREATE THE COLUMN "OFFICIAL" #################
##############################################################
flagValidTable = ReadDatatable("valid_flags")

data=left_join(data,flagValidTable,by=c("flagObservationStatus","flagMethod"))%>%
  data.table

data[flagObservationStatus%in%c("","T"),Official:=TRUE]
data[is.na(Official),Official:=FALSE]
data[flagObservationStatus%in%c("","T"),Protected:=TRUE]

#######################################
# The following copy is needed for saving back some of the intermediate
# files. These intermediate steps will come without flag and the flag
# will be merged with this original data object
dataFlags = copy(data)





##############################################################
# For DERIVED select only the protected and the estimation 
# (coming from the submodule of derived and Livestock)
# I have to select Protected and Estimation (I,e) and (I,i)
# For all the others delete the production value
# this will leave the Sua Filling creting prodcution, where needed

level = findProcessingLevel(tree, from = params$parentVar,
                            to = params$childVar, aupusParam = params)
primaryEl = level[processingLevel == 0, get(params$itemVar)]

data[!(get(params$protected)=="TRUE"|(flagObservationStatus=="I"&flagMethod%in%c("i","e")))
     &get(params$elementVar)==params$productionCode
     &!(get(params$itemVar) %in% primaryEl),Value:=NA]


p=params


##############################################################
##############  LAST MANIPULATIONS ON TREE   #################
##############################################################

tree[,c("flagObservationStatus","flagMethod"):=NULL]
tree=data.table(dcast(tree,geographicAreaM49 + measuredItemParentCPC + measuredItemChildCPC + timePointYears
                      ~ measuredElementSuaFbs,value.var = "Value"))

tree=tree[!is.na(extractionRate)]
tree=tree[!is.na(measuredItemChildCPC)]

tree=tree[,share:=NULL]

tree<-merge(
  tree,
  data_tree,
  by=c(params$geoVar,params$parentVar,params$childVar,params$yearVar)
)

message("Download fbsTree from SWS...")
fbsTree=ReadDatatable("fbs_tree")
fbsTree=data.table(fbsTree)
setnames(fbsTree,colnames(fbsTree),c( "fbsID1", "fbsID2", "fbsID3","fbsID4", "measuredItemSuaFbs"))
setcolorder(fbsTree,c("fbsID4", "measuredItemSuaFbs", "fbsID1", "fbsID2", "fbsID3"))

#some correction
fbsTree<-fbsTree[measuredItemSuaFbs %in% c("34120","21932.02"),fbsID4:="2586"]

treeFBSmerge<-NonStandardizedChidren(fbsTree = fbsTree,tree = tree,standParams = p)

tree<-merge(
  tree,
  treeFBSmerge,
  by=c(params$geoVar,params$parentVar,params$childVar,params$yearVar)
)

tree<-tree[timePointYears %in% yearVals,]

data = data[!is.na(measuredElementSuaFbs), ]
data=data[,c("measuredItemSuaFbs", "measuredElementSuaFbs", "geographicAreaM49", 
             "timePointYears", "Value", "flagObservationStatus", "flagMethod", 
             "Valid", "Protected", "Official"),with=FALSE]


#######################################################
data=data[,mget(c("measuredItemSuaFbs","measuredElementSuaFbs", "geographicAreaM49", "timePointYears","Value","Official","Protected","flagObservationStatus","flagMethod"))]
# data=data[,mget(c("measuredItemSuaFbs","measuredElementSuaFbs", "geographicAreaM49", "timePointYears","Value","Official","Protected","type"))]

#############################################################
##########    LOAD NUTRIENT DATA AND CORRECT    #############
#############################################################
message("Loading nutrient data...")

itemKeys = GetCodeList("agriculture", "aupus_ratio", "measuredItemCPC")[, code]

# Nutrients are:
# 1001 Calories
# 1003 Proteins
# 1005 Fats
nutrientCodes = c("1001", "1003", "1005")

nutrientData = getNutritiveFactors(measuredElement = nutrientCodes,
                                   timePointYears = as.character(2014:2017),
                                   geographicAreaM49 = COUNTRY
)
setnames(nutrientData, c("measuredItemCPC", "timePointYearsSP"),
         c("measuredItemSuaFbs", "timePointYears"))

# It has been found that some Nutrient Values are wrong in the Nutrient Data Dataset

######### CREAM SWEDEN 

nutrientData[geographicAreaM49=="752"&measuredItemSuaFbs=="22120"&measuredElement=="1001",Value:=195]
nutrientData[geographicAreaM49=="752"&measuredItemSuaFbs=="22120"&measuredElement=="1003",Value:=3]
nutrientData[geographicAreaM49=="752"&measuredItemSuaFbs=="22120"&measuredElement=="1005",Value:=19]

### MILK SWEDEN
nutrientData[geographicAreaM49%in%c("756","300","250","372","276")&measuredItemSuaFbs=="22251.01"&measuredElement=="1001",Value:=387]
nutrientData[geographicAreaM49%in%c("756","300","250","372","276")&measuredItemSuaFbs=="22251.01"&measuredElement=="1003",Value:=26]
nutrientData[geographicAreaM49%in%c("756","300","250","372","276")&measuredItemSuaFbs=="22251.01"&measuredElement=="1005",Value:=30]

nutrientData[geographicAreaM49=="300"&measuredItemSuaFbs=="22253"&measuredElement=="1001",Value:=310]
nutrientData[geographicAreaM49=="300"&measuredItemSuaFbs=="22253"&measuredElement=="1003",Value:=23]
nutrientData[geographicAreaM49=="300"&measuredItemSuaFbs=="22253"&measuredElement=="1005",Value:=23]

setnames(nutrientData,"measuredElement","measuredElementSuaFbs")
nutrientData[get(params$elementVar)=="1001",params$elementVar:=params$calories]
nutrientData[get(params$elementVar)=="1003",params$elementVar:=params$proteins]
nutrientData[get(params$elementVar)=="1005",params$elementVar:=params$fats]


############################ POPULATION #####################################

key <-
  DatasetKey(
    domain = "population",
    dataset = "population_unpd",
    dimensions =
      list(
        geographicAreaM49 = Dimension(name = "geographicAreaM49", keys = COUNTRY),
        measuredElementSuaFbs = Dimension(name = "measuredElement", keys = "511"), # 511 = Total population
        timePointYears = Dimension(name = "timePointYears", keys = as.character(2000:2017))
      )
  )


popSWS <- GetData(key)

stopifnot(nrow(popSWS) > 0)

popSWS[geographicAreaM49 == "156", geographicAreaM49 := "1248"]


############################ / POPULATION ##################################


# # Calculate calories

calories_per_capita <-
  merge(
    # Food
    data[
      measuredElementSuaFbs == "food",
      list(
        geographicAreaM49,
        # measuredElementSuaFbs = "664",
        measuredItemSuaFbs,
        timePointYears,
        food = Value #,
        # flagObservationStatus = "T",
        # flagMethod = "i"
      )
      ],
    # Calories
    nutrientData[,
                 list(
                   geographicAreaM49,
                   measuredItemSuaFbs, #= measuredItemCPC,
                   timePointYears, #= timePointYearsSP,
                   measuredElementSuaFbs,
                   nutrient = Value
                 )
                 ],
    by = c('geographicAreaM49', 'timePointYears', 'measuredItemSuaFbs'),
    all.x = TRUE,
    allow.cartesian = TRUE
  )
# 
calories_per_capita <-
  merge(
    calories_per_capita,
    popSWS[, list(geographicAreaM49, timePointYears, population = Value)],
    by = c('geographicAreaM49', 'timePointYears'),
    all.x = TRUE
  )


#calories_per_capita[, Protected := FALSE]
calories_per_capita[, flagObservationStatus := "T"]
calories_per_capita[, flagMethod := "I"]

calories_per_capita_total<-copy(calories_per_capita)


calories_per_capita[, Value := food * nutrient / population / 365 * 10]

calories_per_capita[, c("food", "nutrient", "population") := NULL]

# calories_per_capita_total<-copy(calories_per_capita)

calories_per_capita_total[, Value := food * nutrient/100]
calories_per_capita_total[, c("food", "nutrient", "population") := NULL]


calories_per_capita_total[measuredElementSuaFbs=="calories",
                          measuredElementSuaFbs:="TotalCalories"]

calories_per_capita_total[measuredElementSuaFbs=="proteins",
                          measuredElementSuaFbs:="TotalProteins"]

calories_per_capita_total[measuredElementSuaFbs=="fats",
                          measuredElementSuaFbs:="TotalFats"]



dataDes<-rbind(calories_per_capita,calories_per_capita_total)
#################################################################
#################################################################
#################################################################
message("Download Utilization Table from SWS...")

#utilizationTable=ReadDatatable("utilization_table")
Utilization_Table <- ReadDatatable("utilization_table_2018")

DerivedItem <- Utilization_Table[derived == 'X', get("cpc_code")]

message("Download zero Weight from SWS...")

zeroWeight=ReadDatatable("zero_weight")[,item_code]


message("Defining vectorized standardization function...")

## Split data based on the two factors we need to loop over
uniqueLevels = data[, .N, by = c("geographicAreaM49", "timePointYears")]
uniqueLevels[, N := NULL]
parentNodes = getCommodityLevel(tree, parentColname = "measuredItemParentCPC",
                                childColname = "measuredItemChildCPC")

parentNodes = parentNodes[level == 0, node] 

aggFun = function(x) {
  if (length(x) > 1)
    stop("x should only be one value!")
  return(sum(x))
}

standData = vector(mode = "list", length = nrow(uniqueLevels))
standData0 = vector(mode = "list", length = nrow(uniqueLevels))
standQTY=vector(mode = "list", length = nrow(uniqueLevels))
NonStanditemChild = vector(mode = "list", length = nrow(uniqueLevels))

onlyCalories<-c("39120.01","23140.01","23220.02","39130.01","39120.02","39120.03",
                "23120.02","23140.06","39120.04","39130.02",	"39120.05","39120.06",
                "39120.07","39120.08","39120.09","39120.10","39120.11","39120.12","39120.13",
                "23180","23999.02","23540","39120.14","22130.01","22290","22270","23993.01")

#########STANDARDIZATION AND AGGREGATION-----------------------------------
message("Beginning actual standardization process...")

#
for (i in seq_len(nrow(uniqueLevels))) {
  #i=4
  
  message(paste("Standardizing ",uniqueLevels$geographicAreaM49[i]," for the year ",uniqueLevels$timePointYears[i]))
  
  filter = uniqueLevels[i, ]
  dataSubset = data[filter, , on = c("geographicAreaM49", "timePointYears")]
  dataDesSubset = dataDes[filter, , on = c("geographicAreaM49", "timePointYears")]
  treeSubset = tree[filter, , on = c("geographicAreaM49", "timePointYears")]
  treeSubset[, c("geographicAreaM49", "timePointYears") := NULL]
  
  # there some cases where the multiparent child commodity have 2 parents and is in the FBS
  # group than only 1 parent. In that case it is comnsidered as standardized (even if for the other parent it will be false)
  # it is the case starch of potaote that has 2 parent: potaote ans sweet potatoe but is the same FBS group than potatoe
  
  nonStandChildren<-treeSubset[,som:=sum(standard_child,na.rm = TRUE),
                               by=c("measuredItemChildCPC")]
  
  nonStandChildren<-unique(
    nonStandChildren[,list(measuredItemChildCPC,som,standard_child)],
    by=c("measuredItemChildCPC","som","standard_child")
  )
  
  #keep only child commodities that should not be standardized
  nonStandChildren<-nonStandChildren[som==0 & !is.na(standard_child)]
  nonStandChildren[,som:=NULL]
  
  nonStandChildren<-nonStandChildren [,get(p$childVar)]
  
  #cottonseet is not a promary in the tree but is the only commodity in the FBS group Cottonseed
  nonStandChildren<-c(nonStandChildren,"0143")
  
  # nonStandChildren<-NonStandardizedChidren(fbsTree = fbsTree,tree = treeSubset,standParams = p)
  #************************************
  #child items that are not standardized
  
  #Items being alone in FBS groups
  OneItemFBS<-fbsTree[,number_item:=.N,
                      by=c("fbsID4")][number_item==1,get("measuredItemSuaFbs")]
  
  #Palm oil issue(indonesia)
  OneItemFBS<-c(OneItemFBS,"2165")
  
  
  parentNod<-setdiff(treeSubset[,get(p$parentVar)],treeSubset[,get(p$childVar)])
  parentNod<-c(parentNod,nonStandChildren)
  
  NonStandItems<-dataSubset[get(p$itemVar) %!in% parentNod & 
                              get(p$itemVar) %in% DerivedItem &
                              get(p$itemVar) %!in% treeSubset[,get(p$childVar)] &
                              get(p$itemVar) %in% fbsTree[,get(p$itemVar)] & 
                              abs(Value)>1]
  
  NonStandItems<-unique(NonStandItems[,get(p$itemVar)])
  
  NonStanditemChild[[i]]=as.data.table(cbind(geographicAreaM49=dataSubset$geographicAreaM49[1],
                                             measuredItemSuaFbs=NonStandItems,
                                             timePointYears=dataSubset$timePointYears[1]))
  
  #message("Download cut Items from SWS...")
  #cutItems=ReadDatatable("cut_items2")[,cpc_code]
  
  treeSubset[,weight:=1]
  treeSubset[measuredItemChildCPC %in% zeroWeight , weight:=0]
  treeSubset[measuredItemChildCPC %in% onlyCalories , weight:=0]
  
  #**************************************
  #data<-dataSubset
  #tree<-treeSubset
  standParams<-p
  sugarHack<-FALSE
  specificTree<-FALSE
  #cut<-cutItems
  #additiveElements<-nutrientElements
  keyCols = standParams$mergeKey[standParams$mergeKey != standParams$itemVar]
  if(!specificTree){
    if(nrow(dataSubset[, .N, by = c(standParams$geoVar, standParams$yearVar)]) > 1)
      stop("If not using a specificTree, there should only be one ",
           "country and year!")
    keyCols = keyCols[!keyCols %in% c(standParams$geoVar, standParams$yearVar)]
    treeSubset[, c(standParams$yearVar) := dataSubset[, get(standParams$yearVar)][1]]
    treeSubset[, c(standParams$geoVar) := dataSubset[, get(standParams$geoVar)][1]]
  }
  if(dim(treeSubset)[1]!=0){
    
    
    standTree = collapseEdges_NEW(edges = treeSubset, keyCols = keyCols,
                                  parentName = standParams$parentVar,
                                  childName = standParams$childVar,
                                  extractionName = standParams$extractVar,notStandChild = nonStandChildren)
    standTree[,weight:=1]
    standTree[measuredItemChildCPC %in% zeroWeight , weight:=0]
    standTree[measuredItemParentCPC %in% zeroWeight , weight:=0]
  }else{
    standTree = treeSubset
  }
  
  #new to standardize quantity
  dataTest<-copy(dataSubset) 
  
  standKey = standParams$mergeKey[standParams$mergeKey != standParams$itemVar]
  treeTest = collapseEdges_NEW(edges = treeSubset, parentName = standParams$parentVar,
                               childName = standParams$childVar,
                               extractionName = standParams$extractVar,
                               keyCols = standKey, notStandChild = nonStandChildren)
  
  #build a function that take:
  # treesubset, 
  # datasubset, 
  # datadessubset
  # function name : standardization
  #approach to change here 2166 to 2206
  #use findprocessinglevel function
  
  level <- findProcessingLevel(treeSubset, from = standParams$parentVar,
                               to = standParams$childVar, aupusParam = standParams)
  
  tree_level<-merge(
    
    treeSubset,
    level[,list(measuredItemChildCPC=measuredItemSuaFbs,processingLevel)],
    by=c("measuredItemChildCPC")
  )
  
  tree_level[,
             processingLevel := max(processingLevel, na.rm = TRUE),
             by = c("geographicAreaM49", "timePointYears",
                    "measuredItemParentCPC", "measuredItemChildCPC")
             ]
  
  tree_level<-tree_level[measuredItemChildCPC %in% unique(fbsTree$measuredItemSuaFbs) &
                           measuredItemParentCPC %in% unique(fbsTree$measuredItemSuaFbs)]
  
  tree_level<-tree_level[measuredItemChildCPC %in% unique(dataSubset$measuredItemSuaFbs) &
                           measuredItemParentCPC %in% unique(dataSubset$measuredItemSuaFbs)]
  
  for (lev in rev(unique(tree_level$processingLevel))) {
    
    #lev=3
    
    tree_loop <- tree_level[processingLevel == lev & standard_child==TRUE]
    
    setnames(tree_loop,"measuredItemChildCPC","measuredItemSuaFbs")
    data_level<-merge(
      tree_loop,
      dataSubset[measuredElementSuaFbs=="production",c(names(dataSubset)),with=FALSE],
      by=c("geographicAreaM49", "timePointYears","measuredItemSuaFbs")
    )
    
    if(nrow(data_level)>0){
      
      procdata<-data_level[, list(
        Value = sum( Value*weight /get(standParams$extractVar)*get(standParams$shareVar), na.rm = TRUE)),
        by = c(standParams$yearVar, standParams$geoVar,standParams$elementVar, standParams$parentVar)]
      
      procdata[,measuredElementSuaFbs:=standParams$foodProcCode]
      setnames(procdata,standParams$parentVar,"measuredItemSuaFbs")
      setnames(procdata,"Value","Value_proc")
      
      dataSubset<-merge(
        dataSubset,
        procdata,
        by=c("geographicAreaM49","measuredElementSuaFbs", "timePointYears","measuredItemSuaFbs"),
        all.x = TRUE
      )
      
      dataSubset[measuredElementSuaFbs==standParams$foodProcCode & !is.na(Value_proc), Value:=Value-Value_proc]
      dataSubset[measuredElementSuaFbs==standParams$foodProcCode & Value<0, Value:=0]
      dataSubset[,Value_proc:=NULL]
    }
    
    #our things.....
  }
  ##################################################
  
  standardization<-function(dataQTY=dataSubset,
                            dataDES=dataDesSubset,
                            standTree=treeTest,
                            params=standParams){
    
    #TO DO: quality controle with stopifnot
    
    
    StandtreeQTY<-copy(standTree)
    StandtreeDES<-copy(standTree)
    
    ## Merge the tree with the node data
    StandtreeQTY[, c(params$parentVar, params$childVar, params$yearVar, params$geoVar) :=
                   list(as.character(get(params$parentVar)), as.character(get(params$childVar)),
                        as.character(get(params$yearVar)), as.character(get(params$geoVar)))] 
    
    #correction of some weight (in principale this should be done in the SUA caluclation)
    # StandtreeQTY[get(params$childVar)=="22241.01",weight:=1] #Butter of cow milk
    # StandtreeQTY[get(params$childVar)=="22120",weight:=1]  #Cream fresh  
    
    setnames(dataQTY, params$itemVar, params$childVar)
    
    dataQTY <-merge(
      dataQTY, 
      StandtreeQTY,
      by = c(params$yearVar, params$geoVar, params$childVar),
      all.x = TRUE, 
      allow.cartesian = TRUE
    )
    
    dataQTY[is.na(get(params$parentVar)) & get(params$childVar) %!in% zeroWeight,
            c(params$parentVar, params$extractVar, params$shareVar,"weight") :=
              list(get(params$childVar), 1, 1,1)]
    
    #zeroweight that are not in the tree shoulg have weight 0 so thet they will not be standardized
    # case of bran of pulse(paraguay)
    
    dataQTY[is.na(get(params$parentVar)) & get(params$childVar) %in% zeroWeight,
            c(params$parentVar, params$extractVar, params$shareVar,"weight") :=
              list(get(params$childVar), 1, 1,0)]
    
    dataQTY[get(params$childVar) %in% onlyCalories, weight:=0]
    #Cut this connection for Korea
    dataQTY[get(params$childVar)=="24230.01", share:=0]
    
    
    
    
    # dataQTY_pprocessed<-dataQTY[standard_child==FALSE & get(params$elementVar)==c(params$productionCode)]
    # 
    # dataQTY_pprocessed[,params$elementVar:=params$foodProcCode]
    # 
    # 
    # 
    # 
    # #If the parent is a zerweight change the weight of the child to 0
    # # Issue find in Germany, child of molasses were standardized
    # dataQTY_pprocessed[get(params$parentVar) %in% zeroWeight,weight:=0]
    # 
    # 
    # outDataQTY<-dataQTY_pprocessed[, list(
    #   Value = sum( Value*weight /get(params$extractVar)*get(params$shareVar), na.rm = TRUE)),
    #   by = c(params$yearVar, params$geoVar,params$elementVar, params$parentVar)]
    # 
    # #For commodities that are alone in their FBS group, we keep their proces as it is
    # dataQTY_procPar<-copy(dataQTY)
    # dataQTY_procPar<-dataQTY[,
    #                          c(params$geoVar,params$childVar,
    #                            params$elementVar,params$yearVar,"Value"),
    #                          with=FALSE
    #                          ] 
    # 
    # setnames(dataQTY_procPar,params$childVar,params$itemVar)
    # 
    # dataQTY_procPar[,`:=`(Value.par=Value,Value=NULL)]
    # 
    # dataQTY_procPar<-unique(dataQTY_procPar, by=colnames(dataQTY_procPar))
    # setnames(dataQTY_procPar,params$itemVar,params$parentVar)
    # 
    # outDataQTY<-merge(
    #   outDataQTY,
    #   dataQTY_procPar,
    #   by=c(params$geoVar,params$parentVar,params$elementVar,params$yearVar),
    #   all.x = TRUE
    # )
    # 
    # outDataQTY[get(params$elementVar)==params$foodProcCode & get(params$parentVar) %in% OneItemFBS,
    #            Value:=Value.par]
    # 
    # outDataQTY[,Value.par:=NULL]
    #-------------------------
    
    dataQTY_other<-copy(dataQTY)
    
    dataQTY_other[get(params$childVar) %in% nonStandChildren,
                  standard_child:=FALSE]
    
    dataQTY_other[get(params$childVar) %in% nonStandChildren, #TO DO: add nonStandChildren to the arguments
                  c(params$parentVar, params$extractVar, params$shareVar,"weight") :=
                    list(get(params$childVar), 1, 1,1)]
    
    dataQTY_other<-unique(
      dataQTY_other,by=colnames(dataQTY)
    )
    
    #WEIGH correction
    dataQTY_other[get(params$childVar) %!in% nonStandChildren & standard_child==FALSE, weight:=0]
    dataQTY_other[get(params$childVar) %in% onlyCalories, weight:=0]
    dataQTY_other[get(params$parentVar) %in% zeroWeight[! zeroWeight %in% c("22120","22241.01")],
                  weight:=0]
    
    outDataQTY_other = dataQTY_other[, list(
      Value = sum( Value*weight /get(params$extractVar)*get(params$shareVar), na.rm = TRUE)),
      by = c(params$yearVar, params$geoVar,params$elementVar, params$parentVar)]
    
    
    dataQTY_prod<-copy(dataSubset)
    dataQTY_prod<-dataQTY[,
                          c(params$geoVar,params$childVar,
                            params$elementVar,params$yearVar,"Value"),
                          with=FALSE
                          ] 
    
    setnames(dataQTY_prod,params$childVar,params$itemVar)
    
    dataQTY_prod[,`:=`(Value.par=Value,Value=NULL)]
    
    dataQTY_prod[get(params$itemVar) %in% zeroWeight[! zeroWeight %in% c("22120","22241.01")],
                 Value.par:=0]
    
    dataQTY_prod<-unique(dataQTY_prod, by=colnames(dataQTY_prod))
    setnames(dataQTY_prod,params$itemVar,params$parentVar)
    
    outDataQTY_other<-merge(
      outDataQTY_other,
      dataQTY_prod,
      by=c(params$geoVar,params$parentVar,params$elementVar,params$yearVar),
      all.x = TRUE
    )
    
    outDataQTY_other[get(params$elementVar)==params$productionCode,
                     Value:=Value.par] 
    
    
    #do not change the process of items that are alone
    outDataQTY_other[get(params$elementVar)==params$foodProcCode & get(params$parentVar) %in% OneItemFBS,
                     Value:=Value.par]
    
    outDataQTY_other[,Value.par:=NULL]
    
    # outDataQTY<-
    #   outDataQTY[,
    #              c(params$geoVar,params$parentVar,params$elementVar,params$yearVar,"Value"),
    #              with=FALSE]
    
    outDataQTY_other<-
      outDataQTY_other[,
                       c(params$geoVar,params$parentVar,params$elementVar,params$yearVar,"Value"),
                       with=FALSE
                       ]
    #DES AGGREGATION
    
    ## Merge the tree with the node data
    StandtreeDES[, c(params$parentVar, params$childVar, params$yearVar, params$geoVar) :=
                   list(as.character(get(params$parentVar)), as.character(get(params$childVar)),
                        as.character(get(params$yearVar)), as.character(get(params$geoVar)))]
    
    setnames(dataDES, params$itemVar, params$childVar)
    
    dataDES<-merge(dataDES, StandtreeDES,
                   by = c(params$yearVar, params$geoVar, params$childVar),
                   all.x = TRUE, allow.cartesian = TRUE)
    
    dataDES[is.na(get(params$parentVar)) | get(params$childVar)%in% nonStandChildren,
            c(params$parentVar, params$extractVar, params$shareVar) :=
              list(get(params$childVar), 1, 1)]
    
    dataDES[,missedDES:=mean(Value,na.rm = TRUE)>0 & sum(share,na.rm = TRUE)==0,
            by = c(params$yearVar, params$geoVar, params$childVar,params$elementVar)
            ]
    
    dataDES[get(params$childVar) %in% nonStandChildren | missedDES==TRUE,
            `:=`(measuredItemParentCPC=measuredItemChildCPC,share=1,
                 standard_child=FALSE)]
    
    dataDES[,weight:=1]
    dataDES[,params$extractVar:=1]
    
    dataDES<-unique(
      dataDES,by=names(dataDES)
    )
    
    outDataDes = dataDES[, list(
      Value = sum( Value*get(params$shareVar), na.rm = TRUE)),
      by = c(params$yearVar, params$geoVar, params$parentVar,params$elementVar)]
    
    outDataDes<-outDataDes[,c(colnames(outDataQTY_other)),with=FALSE]
    
    out = rbind( #outDataQTY,
                outDataQTY_other,
                outDataDes)
    
    setnames(out,params$parentVar,params$itemVar)
    
    
    
    #Standardized file
    standardizeQty<-rbind(dataQTY_other #,
                          #dataQTY_pprocessed
                          )
    
    standardizeQty<-standardizeQty[,list(geographicAreaM49,timePointYears,measuredItemParentCPC,
                                         measuredItemChildCPC,measuredElementSuaFbs,
                                         Value,flagObservationStatus,flagMethod,extractionRate,share,
                                         standard_child,weight,Stand_Value=Value/extractionRate*share*weight)]
    
    output<-list(fbs=out,stand=standardizeQty)
    
    return(output)
  }
  
  output=standardization(dataSubset,dataDesSubset,treeTest,standParams)
  out=output$fbs
  
  standData0[[i]] <- out
  standQTY[[i]] <- output$stand
  
  # STEP 7: Aggregate to FBS Level
  if(is.null(fbsTree)){
    # If no FBS tree, just return SUA-level results
    outOut=dataSubset
  } else {
    outOut = computeFbsAggregate(data =out , fbsTree = fbsTree,
                                 standParams = p)
  }
  standData[[i]] <- rbindlist(outOut)
  names(standData[[i]])[grep("^fbsID", names(standData[[i]]))] <- params$itemVar
  standData[[i]][,(params$itemVar):= paste0("S", get(params$itemVar))]
  
}

##############ITEMS THAT ARE NOT STANDARDIZED##################################################
standQTY<-rbindlist(standQTY)
NonStanditemChild<-rbindlist(NonStanditemChild)
NonStanditemChild<-unique(NonStanditemChild[,list(geographicAreaM49,measuredItemSuaFbs)], by=c(p$geoVar,p$itemVar))
setnames(NonStanditemChild,"measuredItemSuaFbs","measuredItemFbsSua")
NonStanditemChild<-nameData("suafbs", "sua_balanced", NonStanditemChild)

#if the number of SUAs is more than 1 we cannot include COUNTRY_NAME, in the file name
if(length(selectedGEOCode)==1){
  tmp_file_nonStandItemps<- tempfile(pattern = paste0("NON_STANDARDIZED_ITEMS_", COUNTRY_NAME,
                                                      "_"), fileext = '.csv')
}else{
  tmp_file_nonStandItemps<- tempfile(pattern = paste0("NON_STANDARDIZED_ITEMS_",
                                                      "_"), fileext = '.csv')}
write.csv(NonStanditemChild, tmp_file_nonStandItemps)
############################################################################################

# XXX fix this
codes <- tibble::tribble(
  ~code,  ~name,
  "5910", "exports",
  "5520", "feed",
  "5141", "food",
  "5023", "foodManufacturing",
  "5610", "imports",
  "5165", "industrial",
  "5016", "loss",
  "5510", "production",
  "5525", "seed",
  "5071", "stockChange",
  "664", "calories",
  "674", "proteins",
  "684", "fats",
  "5166", "residual",
  "5164", "tourist",
  "261", "TotalCalories",
  "271", "TotalProteins",
  "281", "TotalFats"
)

setDT(codes)

#####sSAVING FBS STANDARDIZED#########################################----------------------------
fbs_standardized<-rbindlist(standData0)
fbs_standardized[,`:=`(flagObservationStatus="I",
                       flagMethod="s")]
setnames(fbs_standardized,"measuredItemSuaFbs","measuredItemFbsSua")
setDT(fbs_standardized)



fbsstand_residual<-copy(fbs_standardized)

calculateImbalance(data=fbsstand_residual,keep_supply = FALSE,keep_utilizations = FALSE)

fbsstand_residual[,`:=`(Value=imbalance,
                        measuredElementSuaFbs="residual",
                        imbalance=NULL)]
fbsstand_residual<-unique(fbsstand_residual, by=colnames(fbsstand_residual))

fbs_standardized<-rbind(fbs_standardized[measuredElementSuaFbs %!in% c("residual")],fbs_standardized)


fbs_standardized <- fbs_standardized[codes, on = c('measuredElementSuaFbs' = 'name')]
fbs_standardized[,measuredElementSuaFbs:=code]
fbs_standardized[,code:=NULL]
fbs_standardized[is.na(Value),flagObservationStatus:=NA]
fbs_standardized[is.na(Value),flagMethod:=NA]
fbs_standardized<-fbs_standardized[measuredElementSuaFbs %!in% c("261","271","281")]


message("saving FBS standardized...")
SaveData(domain = "suafbs", dataset = "fbs_standardized", data = fbs_standardized, waitTimeout = 20000)

#end fns standardized-------------------------------------------------------------

########SAVING FBS BALANCED#################################################-------------------

fbs_balanced<-rbindlist(standData)
fbs_balanced[,`:=`(flagObservationStatus="I",
                   flagMethod="s")]
setnames(fbs_balanced,"measuredItemSuaFbs","measuredItemFbsSua")

#calculate imbalance for fbs and put it the residual and othe ruses

fbs_residual<-copy(fbs_balanced)

fbs_residual<-fbs_residual[measuredItemFbsSua %!in% c("S2901","S2903","S2941")]
calculateImbalance(data=fbs_residual,keep_supply = TRUE,keep_utilizations = FALSE)

fbs_residual<-
  fbs_residual[!is.na(imbalance),list(geographicAreaM49,timePointYears,
                                      measuredItemFbsSua,supply,imbalance)]
fbs_residual<-unique(fbs_residual,by=c(colnames(fbs_residual)))
fbs_residual[,imbalance_percentage:=round(imbalance/supply*100,2)]



fbs_residual1<-copy(fbs_balanced)
calculateImbalance(data=fbs_residual1,keep_supply = FALSE,keep_utilizations = FALSE)

fbs_residual1[,`:=`(Value=imbalance,
                    measuredElementSuaFbs="residual",
                    imbalance=NULL)]
fbs_residual1<-unique(fbs_residual1, by=colnames(fbs_residual1))

fbs_balanced<-rbind(fbs_balanced[measuredElementSuaFbs %!in% c("residual")],fbs_residual1)


fbs_balanced <- fbs_balanced[codes, on = c('measuredElementSuaFbs' = 'name')]
fbs_balanced[,measuredElementSuaFbs:=code]
fbs_balanced[,code:=NULL]
fbs_balanced[is.na(Value),flagObservationStatus:=NA]
fbs_balanced[is.na(Value),flagMethod:=NA]




popData<-
  popSWS[,list(geographicAreaM49,
               timePointYears,
               measuredElementSuaFbs=measuredElement,
               measuredItemFbsSua="S2501",
               Value,
               flagObservationStatus,
               flagMethod)]

popData<-popData[timePointYears %in% unique(fbs_balanced$timePointYears)]

#correct process at FBS level

Item_with_unbalanced<-data[measuredElementSuaFbs=="residual"]
Item_with_unbalanced<-Item_with_unbalanced[,balanced:=ifelse(abs(round(Value,0))<5000,TRUE,FALSE)]
Item_with_unbalanced<-Item_with_unbalanced[balanced==FALSE]

Item_with_unbalanced<-merge(
  Item_with_unbalanced,
  fbsTree,
  by=c(p$itemVar),
  allow.cartesian = TRUE,
  all.x = TRUE
)

Item_with_unbalanced<-Item_with_unbalanced[!is.na(fbsID4) & (measuredItemSuaFbs %!in% onlyCalories),
                                           list(geographicAreaM49,timePointYears,
                                                measuredItemFbsSua=paste0("S",fbsID4),
                                                cpc_unbalanced=measuredItemSuaFbs)
                                           ]
Item_with_unbalanced<-unique(Item_with_unbalanced,by=c(colnames(Item_with_unbalanced)))


Item_with_unbalanced<-aggregate(
  cpc_unbalanced ~ geographicAreaM49+measuredItemFbsSua+timePointYears, 
  Item_with_unbalanced, paste0, collapse = "; ")

setDT(Item_with_unbalanced)

fbs_balanced_bis<-merge(
  fbs_balanced,
  Item_with_unbalanced,
  by=c(p$geoVar,p$yearVar, "measuredItemFbsSua"),
  all.x = TRUE
)

fbs_balanced_bis[,IsSUAbal:=ifelse(is.na(cpc_unbalanced),TRUE,FALSE)]

fbs_balanced_bis<-merge(
  fbs_balanced_bis,
  fbs_residual[,list(geographicAreaM49,timePointYears,measuredItemFbsSua,imbalance=round(imbalance,0))]
)

fbsid4<-paste0("S",unique(fbsTree$fbsID4))
fbs_balanced_bis[measuredItemFbsSua %!in% fbsid4,IsSUAbal:=FALSE]
fbs_balanced_bis<-fbs_balanced_bis[!is.na(Value)]

fbs_balanced_bis[,update_balance:=FALSE]

fbs_balanced_bis[measuredElementSuaFbs %in% c("5023") & !is.na(imbalance) & !is.na(Value),
                 update_balance:=ifelse(IsSUAbal==TRUE & Value+imbalance >0,TRUE,FALSE),
                 by=c(p$geoVar,p$yearVar,"measuredItemFbsSua")]

fbs_balanced_bis[measuredElementSuaFbs=="5023" ,
                 Value:=ifelse(update_balance==TRUE ,Value+imbalance,Value),
                 by=c(p$geoVar,p$yearVar,"measuredItemFbsSua")]

fbs_balanced_bis[measuredElementSuaFbs %in% c("5023","5166"),
                 update_balance:=update_balance[measuredElementSuaFbs=="5023"],
                 by=c(p$geoVar,p$yearVar,"measuredItemFbsSua")]

fbs_balanced_bis<-fbs_balanced_bis[!is.na(Value)]

fbs_balanced_bis[measuredElementSuaFbs=="5166" ,
                 Value:=ifelse(update_balance==TRUE ,0,Value),
                 by=c(p$geoVar,p$yearVar,"measuredItemFbsSua")]

fbs_balanced_bis<-fbs_balanced_bis[,names(fbs_balanced),with=FALSE]

#correct other level

fbstree_otherlev<-copy(fbsTree)

fbstree_otherlev[,c("measuredItemSuaFbs","number_item"):=NULL]
fbstree_otherlev<-unique(fbstree_otherlev)
setnames(fbstree_otherlev,"fbsID4","measuredItemFbsSua")

fbstree_otherlev[, measuredItemFbsSua :=paste0("S",measuredItemFbsSua)]
fbstree_otherlev[, fbsID1 :=paste0("S",fbsID1)]
fbstree_otherlev[, fbsID2 :=paste0("S",fbsID2)]
fbstree_otherlev[, fbsID3 :=paste0("S",fbsID3)]

fbs_other_level<-merge(
  fbs_balanced_bis[measuredElementSuaFbs %in% c("5023","5166")],
  
  fbstree_otherlev,
  by=c("measuredItemFbsSua"),
  all.x = TRUE
)



# aggregate(fbs_other_level, by =list(p$yearVar,p$geoVar,p$elementVar, "fbsID3"), FUN=sum(Value))

level_3 <- aggregate(
  Value ~ geographicAreaM49+fbsID3 +timePointYears + measuredElementSuaFbs, 
  fbs_other_level, sum, na.rm = TRUE)

setnames(level_3,"fbsID3","measuredItemFbsSua")

# level_2 <- aggregate(
#   Value ~ geographicAreaM49+fbsID2 +timePointYears + measuredElementSuaFbs, 
#   fbs_other_level, sum, na.rm = TRUE)
# setnames(level_2,"fbsID2","measuredItemFbsSua")


# level_1 <- aggregate(
#   Value ~ geographicAreaM49+fbsID1 +timePointYears + measuredElementSuaFbs, 
#   fbs_other_level, sum, na.rm = TRUE)
# setnames(level_1,"fbsID1","measuredItemFbsSua")



data_level<-rbind(level_3)
setDT(data_level)
data_level[,flagObservationStatus:="I"]
data_level[,flagMethod:="s"]

data_level<-data_level[,names(fbs_balanced_bis),with=FALSE]

fbs_balanced_bis<-rbind(
  fbs_balanced_bis[!data_level, on=c("geographicAreaM49","timePointYears","measuredItemFbsSua")],
  data_level
)





# aggregate(
#   measuredItemSuaFbs ~ geographicAreaM49+measuredItemFbsSua+timePointYears, 
#   Item_with_unbalanced, paste0, collapse = "; ")

message("saving FBS balanced...")
SaveData(domain = "suafbs", dataset = "fbs_balanced_", data = fbs_balanced_bis, waitTimeout = 20000)

SaveData(domain = "suafbs", dataset = "fbs_balanced_", data = popData, waitTimeout = 20000)



#end fbs balanced---------------------------------------

######################


# data_residual<-copy(data)
# setnames(data_residual,"measuredItemSuaFbs" ,"measuredItemFbsSua")
# calculateImbalance(data=data_residual,keep_supply = FALSE,keep_utilizations = FALSE)
# 
# data_residual[,`:=`(Value=imbalance,
#                     measuredElementSuaFbs="residual",
#                     imbalance=NULL)]
# 
# setnames(data_residual,"measuredItemFbsSua","measuredItemSuaFbs")
# 
# data_residual<-unique(data_residual, by=colnames(data))
# 
# data<-rbind(data[measuredElementSuaFbs %!in% c("residual")],data_residual)

Item_with_unbalanced<-
  data[measuredElementSuaFbs=="residual"][,
                                          balanced:=ifelse(abs(Value)<1000,TRUE,FALSE)][balanced==FALSE]

Item_with_unbalanced<-merge(
  Item_with_unbalanced,
  fbsTree,
  by=c(p$itemVar),
  allow.cartesian = TRUE,
  all.x = TRUE
)

Item_with_unbalanced<-Item_with_unbalanced[!is.na(fbsID4) & (measuredItemSuaFbs %!in% onlyCalories),
                                           list(geographicAreaM49,timePointYears,
                                                measuredItemFbsSua=paste0("S",fbsID4),
                                                measuredItemSuaFbs)
                                           ]
Item_with_unbalanced<-unique(Item_with_unbalanced,by=c(colnames(Item_with_unbalanced)))


Item_with_unbalanced<-aggregate(
  measuredItemSuaFbs ~ geographicAreaM49+measuredItemFbsSua+timePointYears, 
  Item_with_unbalanced, paste0, collapse = "; ")



# 

# check if the primary or proxy primary is balanced
primProxiPrim<-unique(Utilization_Table[primary_item=="X" | proxy_primary=="X",get("cpc_code")])

balanceSUA<-data[measuredItemSuaFbs %in% primProxiPrim & measuredElementSuaFbs=="residual"]
balanceSUA<-balanceSUA[measuredItemSuaFbs %!in% onlyCalories]

balanceSUA[,balanced:=ifelse(abs(Value)<1000,TRUE,FALSE)]

balanceSUA<-balanceSUA[, list(geographicAreaM49,timePointYears,measuredItemSuaFbs,balanced)]

balanceSUA<-merge(
  balanceSUA,
  fbsTree,
  by=c(p$itemVar),
  allow.cartesian = TRUE,
  all= TRUE
)


balanceSUA<-balanceSUA[,list(geographicAreaM49,timePointYears,parent=measuredItemSuaFbs,
                             balanced,measuredItemFbsSua=fbsID4)]

balanceSUA<-unique(balanceSUA,by=c(names(balanceSUA)))

balanceSUA[,measuredItemFbsSua:=paste0("S",measuredItemFbsSua)]

### File containing imbalances greater that 1 MT----------------------
# fbs_residual_to_send<-fbs_residual[abs(imbalance_percentage)>=1]

fbs_residual_to_send<-copy(fbs_balanced_bis[measuredElementSuaFbs=="5166" & abs(Value)>1000])

fbs_residual_to_send<-merge(
  fbs_residual_to_send,
  fbs_residual[,list(geographicAreaM49,timePointYears,measuredItemFbsSua,supply)],
  by=c(p$geoVar,p$yearVar,"measuredItemFbsSua"),
  all.x =TRUE
)

fbs_residual_to_send[,imbalance_percentage:=round((Value/supply)*100,0),
                     by=c(p$geoVar,p$yearVar,"measuredItemFbsSua")
                     ]

fbs_residual_to_send<-merge(
  fbs_residual_to_send,
  balanceSUA,
  by=c(p$geoVar,p$yearVar,"measuredItemFbsSua"),
  allow.cartesian = TRUE,
  all.x =TRUE
)



fbs_residual_to_send<-merge(
  fbs_residual_to_send,
  Item_with_unbalanced,
  by=c(p$geoVar,p$yearVar,"measuredItemFbsSua"),
  allow.cartesian = TRUE,
  all = TRUE
)

# fbs_residual_to_send<-fbs_residual_to_send[abs(imbalance)>=1000 | !is.na(measuredItemSuaFbs)]

# fbs_residual_to_send[,unbalanced:=ifelse(balanced==FALSE & abs(imbalance)>1000,TRUE,FALSE)]

# fbs_residual_to_send[balanced==FALSE & abs(imbalance)<1000,unbalanced:=NA]
fbs_residual_to_send[,balanced:=NULL]

fbs_residual_to_send<-nameData("suafbs", "fbs_balanced_", fbs_residual_to_send)



LabelItem<-unique(
  data[,list(measuredItemFbsSua=measuredItemSuaFbs)],by=c("measuredItemFbsSua")
)
LabelItem<-nameData("suafbs", "fbs_balanced_", LabelItem)

standQTY<-merge(
  standQTY,
  LabelItem[,list(measuredItemChildCPC=measuredItemFbsSua,
                  measuredItemChildCPC_name=measuredItemFbsSua_description)],
  by=c("measuredItemChildCPC")
)

standQTY<-merge(
  standQTY,
  LabelItem[,list(measuredItemParentCPC=measuredItemFbsSua,
                  measuredItemParentCPC_name=measuredItemFbsSua_description)],
  by=c("measuredItemParentCPC")
)

standQTY<-merge(
  standQTY,
  fbsTree[,list(measuredItemParentCPC=measuredItemSuaFbs,
                FBS_group=fbsID4)]
)

# extracting the dataset contains items of the commodity tree that are standardized outside the 
#FBS group: this will be merged in the summary file containing FBS imbalances

Items_outside_tree<-standQTY[measuredElementSuaFbs=="foodManufacturing" & Value>0,
                             list(geographicAreaM49,timePointYears,Child_outside_tree=measuredItemChildCPC,
                                  measuredItemFbsSua=paste0("S",FBS_group))]

Items_outside_tree<-aggregate(
  Child_outside_tree ~measuredItemFbsSua+ geographicAreaM49+timePointYears, 
  Items_outside_tree, paste0, collapse = "; ")

fbs_residual_to_send<-merge(
  fbs_residual_to_send,
  Items_outside_tree,
  by=c("geographicAreaM49","timePointYears","measuredItemFbsSua"),
  all.x = TRUE
)

#Consider only fbsID4 groups for the final summary file

fbsID4_groups<-paste0("S",unique(fbsTree$fbsID4))
#fbs_residual_to_send<-fbs_residual_to_send[measuredItemFbsSua %in% fbsID4_groups]
#-----

standQTY<-standQTY[,list(FBS_group,geographicAreaM49,timePointYears,measuredElementSuaFbs,
                         measuredItemParentCPC,measuredItemParentCPC_name,
                         measuredItemChildCPC,measuredItemChildCPC_name,Value,
                         flagObservationStatus,flagMethod,extractionRate,share,weight,
                         standard_child,Stand_Value)]


#if the number of SUAs is more than 1 we cannot include COUNTRY_NAME, in the file name
if(length(selectedGEOCode)==1){
  tmp_file_residual<- tempfile(pattern = paste0("FBS_IMBALANCES_", COUNTRY_NAME, "_"), fileext = '.csv')
  tmp_file_standData<- tempfile(pattern = paste0("Stand_SUA_", COUNTRY_NAME, "_"), fileext = '.csv')
  
}else{
  tmp_file_residual<- tempfile(pattern = paste0("FBS_IMBALANCES_"), fileext = '.csv')
  tmp_file_standData<- tempfile(pattern = paste0("Stand_SUA_"), fileext = '.csv')
  
}

fbs_residual_to_send<-unique(fbs_residual_to_send, by=c(names(fbs_residual_to_send)))
write.csv(fbs_residual_to_send, tmp_file_residual)
write.csv(standQTY, tmp_file_standData)

### End File containing imbalances greater that 1 MT----------------------

#File containing aggregated DES from FBS-----------------------
fbs_des_to_send<-fbs_balanced[measuredElementSuaFbs=="664"]
fbs_des_to_send<-nameData("suafbs", "fbs_balanced_", fbs_des_to_send)
fbs_des_to_send <-
  dcast(
    fbs_des_to_send,
    geographicAreaM49_description + measuredElementSuaFbs_description+measuredItemFbsSua_description ~ timePointYears,
    fun.aggregate = sum,
    value.var = "Value"
  )

#if the number of SUAs is more than 1 we cannot include COUNTRY_NAME, in the file name
if(length(selectedGEOCode)==1){
  tmp_file_des<- tempfile(pattern = paste0("FBS_DES_", COUNTRY_NAME, "_"), fileext = '.csv')
}else{
  tmp_file_des<- tempfile(pattern = paste0("FBS_DES_"), fileext = '.csv')
}
write.csv(fbs_des_to_send, tmp_file_des)
#End File containing aggregated DES from FBS-----------------------

#DES comparison: SUA and FBS-----------------
DEssua<-dataDes[measuredElementSuaFbs=="calories",]
DEssua<-DEssua[,
               list(Value=sum(Value,na.rm = TRUE)),
               by=c("geographicAreaM49","measuredElementSuaFbs","timePointYears")
               ]

DEssua<-nameData("suafbs", "sua_balanced", DEssua)
DEssua[,measuredItemFbsSua_description:="DES from SUA_bal"]

DEssua <-
  dcast(
    DEssua,
    geographicAreaM49_description + measuredElementSuaFbs_description+measuredItemFbsSua_description ~ timePointYears,
    fun.aggregate = sum,
    value.var = "Value"
  )

setDT(DEssua)
DEssua[,`measuredElementSuaFbs_description`:="Food supply (/capita/day) [kcal]"]

setDT(fbs_des_to_send)
DesFBS<-fbs_des_to_send[measuredItemFbsSua_description=="GRAND TOTAL - DEMAND"]
DesFBS[,measuredItemFbsSua_description:="DES from FBS"]
ComparativeDES<-rbind(DEssua,DesFBS)

#if the number of SUAs is more than 1 we cannot include COUNTRY_NAME, in the file name
if(length(selectedGEOCode)==1){
  tmp_file_desSuaFbs<- tempfile(pattern = paste0("DES_SUA_vs_FBS_", COUNTRY_NAME, "_"), fileext = '.csv')
}else{
  tmp_file_desSuaFbs<- tempfile(pattern = paste0("DES_SUA_vs_FBS_"), fileext = '.csv')
}
write.csv(ComparativeDES, tmp_file_desSuaFbs)
#End DES comparison: SUA and FBS-----------------

#Items with DES and without FBS group----------------------
DESItems_noFBSGroup<-dataDes[measuredItemSuaFbs %!in% fbsTree[,get(p$itemVar)] & Value>0,]
DESItems_noFBSGroup[,measuredElementSuaFbs:="664"]
DESItems_noFBSGroup<-nameData("suafbs", "sua_balanced", DESItems_noFBSGroup)

if(nrow(DESItems_noFBSGroup)>0){
  
  DESItems_noFBSGroup <-
    dcast(
      DESItems_noFBSGroup,
      geographicAreaM49_description + measuredElementSuaFbs_description+measuredItemSuaFbs ~ timePointYears,
      fun.aggregate = sum,
      value.var = "Value"
    )
}

#if the number of SUAs is more than 1 we cannot include COUNTRY_NAME, in the file name
if(length(selectedGEOCode)==1){
  tmp_file_noFbsGroup<- tempfile(pattern = paste0("ITEMS_NO_FBSGROUP_", COUNTRY_NAME, "_"), fileext = '.csv')
}else{
  tmp_file_noFbsGroup<- tempfile(pattern = paste0("ITEMS_NO_FBSGROUP_"), fileext = '.csv')
}

write.csv(DESItems_noFBSGroup, tmp_file_noFbsGroup)
#End Items with DES and without FBS group----------------------

fin<-Sys.time()

duree<-fin-commence

if (!CheckDebug()) {
  send_mail(
    from = "do-not-reply@fao.org",
    to = swsContext.userEmail,
    subject = "Results from newBalancing plugin",
    body = c("If all commodities of a tree (FBS item) are balanced at SUA level, then the 
             FBS item is balanced by moving eventual residual to process.",
             tmp_file_des,
             tmp_file_residual,
             tmp_file_noFbsGroup,
             tmp_file_desSuaFbs,
             tmp_file_nonStandItemps #,
             #tmp_file_standData
    )
  )
}


