
##' FULL STANDARDIZATION PROCESS 
##' 
##' This function implements the new standardization process.  The algorithm is 
##' as follows:
##' 
##' 1. Any edges with a target of "F" should be immediately "processed forward".
##' This amounts to computing the residual and allocating it to food processing,
##' and, hence, to production of the children of this node.
##' 
##' 2. Balance the processed products in the SUA by creating production of 
##' processed products.  If a node is set to be processed forward, then it is 
##' also balanced via a residual (i.e. food processing).
##' 
##' 2.1 Since food quantities are now available, we can compute initial calorie 
##' estimates.
##' 
##' 3. Availability at the "balancing level" (usually primary level, but 
##' possibly lower if a node is processed forward or standardized in a different
##' tree) is determined.  Note that at this point, all edges designated with an 
##' "F" (i.e. forward) will have been removed, and so "balancing level" is the 
##' same as the top node of the tree.  This availability defines shares for 
##' standardization.  If no availability of parents is available, the initial 
##' shares are used (in tree[, get(standParams$shareVar)]).
##' 
##' 4. Standardize commodities according to the commodity tree.  This defines 
##' the food processing element at balancing level.
##' 
##' 5. Balance at the balancing level.
##' 
##' 6. Update calories of processed products proportionally based on updated 
##' food element values.
##' 
##' 7. (only if fbsTree is provided) Sum all commodities up to their FBS level 
##' categories.  This is the final step to prepare data for FAOSTAT.
##' 
##' @param data The data.table containing the full dataset for standardization. 
##'   It should have columns corresponding to year, country, element, commodity,
##'   and value.  The specific names of these columns should be in standParams.
##' @param tree The commodity tree which details how elements can be processed 
##'   into other elements.  It does not, however, specify which elements get 
##'   aggregated into others.  This data.table should have columns parent, 
##'   child, extraction rate, share, and target (target specifying if an element
##'   is processed forward or not).  Names of these columns should be provided
##'   in standParams.
##' @param fbsTree This "tree" should just have three columns: 
##'   standParams$parentID, standParams$childID, and standParams$extractVar 
##'   (which if missing will just be assigned all values of 1).  This tree 
##'   specifies how SUA commodities get combined to form the FBS aggregates.  If
##'   NULL, the last step (aggregation to FBS codes) is skipped and data is 
##'   simply returned at SUA level.
##' @param standParams The parameters for standardization.  These parameters 
##'   provide information about the columns of data and tree, specifying (for 
##'   example) which columns should be standardized, which columns represent 
##'   parents/children, etc.
##' @param nutrientData A data.table containing one column with the item codes 
##'   (and this column's name must match standParams$itemVar) and additional 
##'   columns representing nutrient information.  For example, you could have 4 
##'   columns: measuredItemCPC, calories, proteins, fats.  In the calories, 
##'   proteins, and fats columns there should be numeric values representing 
##'   multipliers to convert kilograms of the item into calories/proteins/fats. 
##'   If NULL, nothing is done with nutrients.
##' @param printCodes A list of the item codes which should be printed at each 
##'   step to show the progress of the algorithm.
##' @param debugFile folder for saving the intermediate files.
##' @param batchnumber Number of batch running.
##' @param utilizationTable Table of utilization for suaFilling
##' @param cutItems tree's cuts
##' @return A data.table containing the final balanced and standardized SUA 
##'   data.  Additionally, this table will have new elements in it if 
##'   nutrientData was provided.
##' @importFrom faoswsBalancing balancing
##' 
##' @export
##' 

standardizationWrapper_NW = function(data, tree, fbsTree = NULL, standParams,
                                     nutrientData = NULL, printCodes = c(),
                                     debugFile= NULL,batchnumber=batchnumber,
                                     utilizationTable = utilizationTable,cutItems=cutItems
){
  
  data_Stock_Industrial = as.data.frame(data)
  data_Stock_Industrial = dplyr::select(data_Stock_Industrial,
                                 measuredItemSuaFbs,
                                 geographicAreaM49,
                                 timePointYears,
                                 classification,
                                 Median_Value_Stock,
                                 Median_Value_Industrial)
  data_Stock_Industrial = data_Stock_Industrial[!duplicated(dplyr::select(data_Stock_Industrial,geographicAreaM49,measuredItemSuaFbs)),]
  data = dplyr::select(data,-classification,-Median_Value_Stock,-Median_Value_Industrial)
  data = as.data.table(data)
  
  dataFlags=data.table(data.frame(data))
  dataFlags=dataFlags[,mget(c("measuredItemSuaFbs","measuredElementSuaFbs", "geographicAreaM49", "timePointYears","Value","flagObservationStatus","flagMethod"))]
  
  data=data[,mget(c("measuredItemSuaFbs","measuredElementSuaFbs", "geographicAreaM49", "timePointYears","Value","Official","Protected","type"))]
  
  ## Reassign standParams to p for brevity
  p = standParams
  
  ## STEP 0: Data Quality Checks
  # Checks for data
  stopifnot(c(p$geoVar, p$yearVar, p$itemVar,
              p$elementVar, "Value") %in% colnames(data))
  if(!"standardDeviation" %in% colnames(data))
    data[, standardDeviation := NA_real_]
  data[, c(p$geoVar) := as.character(get(p$geoVar))]
  data[, c(p$yearVar) := as.character(get(p$yearVar))]
  data[, c(p$itemVar) := as.character(get(p$itemVar))]
  # Checks for tree
  stopifnot(c(p$childVar, p$parentVar, p$extractVar,
              p$targetVar, p$shareVar) %in% colnames(tree))
  if(nrow(data[, .N, by = c(p$geoVar, p$yearVar)]) > 1)
    stop("standardizationWrapper works with one country/year at a time only!")
  if(any(is.na(tree[, get(p$childVar)]))){
    warning("tree has some NA children.  Those edges have been deleted.")
    tree = tree[!is.na(get(p$childVar)), ]
  }
  if(!p$standParentVar %in% colnames(tree)){
    warning("p$standParentVar is not in the colnames of tree!  All ",
            "commodities will be standardized to the parents that ",
            "produced them!")
    tree[, c(p$standParentVar) := NA]
  }
  if(!p$standExtractVar %in% colnames(tree)){
    warning("p$standExtractVar is not in the colnames of tree!  No ",
            "new extraction rates will be used!")
    tree[!is.na(get(p$standParentVar)),
         c(p$standExtractVar) := get(p$extractVar)]
  }
  stopifnot(!is.na(tree[, get(p$extractVar)]))
  ## Check that all standParentVar are NA or a value, never ""
  stopifnot(tree[!is.na(get(p$standParentVar)), get(p$standParentVar)] != "")
  # Checks for fbsTree
  if(!is.null(fbsTree)){
    stopifnot(c(p$itemVar, "fbsID1",
                "fbsID2", "fbsID3", "fbsID4") %in% colnames(fbsTree))
  }
  if(any(is.na(tree[, get(p$parentVar)]))){
    warning("tree has some NA parents.  Those edges have been deleted.")
    tree = tree[!is.na(get(p$parentVar)), ]
  }
  # Checks for nutrientData
  if(!is.null(nutrientData)){
    if(colnames(nutrientData)[1] != p$itemVar)
      stop("First column of nutrient data must match standParams$itemVar!")
    stopifnot(ncol(nutrientData) > 1)
    # message("Nutrients are assumed to be:", paste(colnames(nutrientData)[-1], collapse = ", "))
    geo=nameData(domain = "suafbs", dataset = "fbs_standardized",data.table(geographicAreaM49=data[,unique(get(p$geoVar))]))
    yea=data[,unique(timePointYears)]
    message("Country = ",as.character(geo[,2,with=FALSE])," (M49=",as.character(geo[,1,with=FALSE]), ") , year = ",yea)
    nutrientElements = colnames(nutrientData)[2:ncol(nutrientData)]
  } else {
    cat("No nutrient information provided, hence no nutrients are computed.")
    nutrientElements = c()
  }
  
  message("Download cut Items from SWS...")
  
  cutItems=ReadDatatable("cut_items2")[,cpc_code]
  
  ## STEP 1: Add missing element codes for commodities that are in the data
  ## (so that we can print it).  Then, print the table!
  ## Note that this function has been repeted juast after the processForward
  ## because missingElements have to be created for those children which were not in 
  ## dataset previuosly
  
  #for saving the TREE
  yearT=data[,unique(timePointYears)]
  
  data = addMissingElements(data, p)
  data2keep=data.table(data.frame(data))
  
  setnames(data,"measuredItemSuaFbs","measuredItemFbsSua")
  data=nameData("suafbs","sua_unbalanced",data,except = c("measuredElementSuaFbs","geographicAreaM49","timePointYears"))
  setnames(data,"measuredItemFbsSua","measuredItemSuaFbs")
  
  if(length(printCodes) > 0){
    cat("sua_unbalanced")
    old = copy(data[,c(params$mergeKey,params$elementVar,"Value"),with=FALSE])
    printSUATableNames(data = data, standParams = p, printCodes = printCodes)
  }
  data=data.table(data.frame(data2keep))
  data = addMissingElements(data, p)
  
  ### STEP2 Initial Sua Filling 
  
  if(dim(tree)[1]!=0){
    level = findProcessingLevel(tree, from = p$parentVar,
                                to = p$childVar, aupusParam = p)
    primaryEl = level[processingLevel == 0, get(p$itemVar)]
  }else{
    primaryEl=c()
  }
  
  ## Add in elements not in the tree, as they are essentially parents
  nonTreeEl = data[[p$itemVar]]
  nonTreeEl = nonTreeEl[!nonTreeEl %in% level[[p$itemVar]]]
  # primaryEl = c(primaryEl, nonTreeEl)
  
  
  data[, ProtectedProd := any(get(standParams$elementVar) == standParams$productionCode &
                                Protected==TRUE),
       by = c(standParams$itemVar)]
  data[, ProtectedFood := any(get(standParams$elementVar) == standParams$foodCode &
                                Protected==TRUE),
       by = c(standParams$itemVar)]
  
  data[is.na(ProtectedProd), ProtectedProd :=FALSE]
  data[is.na(ProtectedFood), ProtectedFood :=FALSE]
  
  data=data.table(left_join(data,utilizationTable,by=c("geographicAreaM49","measuredElementSuaFbs","measuredItemSuaFbs")))
  
  data = unique(data)
  
  data=suaFilling_NW(data, p=p, tree=tree,
                     primaryCommodities = primaryEl, debugFile=p$createIntermetiateFile,
                     stockCommodities = stockCommodities,
                     utilizationTable = utilizationTable, imbalanceThreshold = 10,loop1=TRUE)
  
  data2keep=data.table(data.frame(data))
  setnames(data,"measuredItemSuaFbs","measuredItemFbsSua")
  data=nameData("suafbs","sua_unbalanced",data,except = c("measuredElementSuaFbs","geographicAreaM49","timePointYears"))
  setnames(data,"measuredItemFbsSua","measuredItemSuaFbs")
  
  if(length(printCodes) > 0){
    cat("\n\nsua_unbalanced + production filled (step 1 of suaFilling):")
    data = markUpdated(new = data, old = old, standParams = p)
    old = copy(data[,c(params$mergeKey,params$elementVar,"Value"),with=FALSE])
    printSUATableNames(data = data, standParams = p,
                       printCodes = printCodes)
  }
  data=data.table(data.frame(data2keep))
  
  ### STEP 3: Compute availability and SHARE 1 
  
  data[, availability := sum(ifelse(is.na(Value), 0, Value) *
                               ifelse(get(p$elementVar) == p$productionCode, 1,
                                      ifelse(get(p$elementVar) == p$importCode, 1,
                                             ifelse(get(p$elementVar) == p$exportCode, -1,
                                                    ifelse(get(p$elementVar) == p$stockCode, 0,
                                                           ifelse(get(p$elementVar) == p$foodCode, 0,
                                                                  ifelse(get(p$elementVar) == p$foodProcCode, 0,
                                                                         ifelse(get(p$elementVar) == p$feedCode, 0,
                                                                                ifelse(get(p$elementVar) == p$wasteCode, 0,
                                                                                       ifelse(get(p$elementVar) == p$seedCode, 0,
                                                                                              ifelse(get(p$elementVar) == p$industrialCode, 0,
                                                                                                     ifelse(get(p$elementVar) == p$touristCode, 0,
                                                                                                            ifelse(get(p$elementVar) == p$residualCode, 0, 0))))))))))))),
       by = c(p$mergeKey)]
  
  
  
  mergeToTree = data[, list(availability = mean(availability)),
                     by = c(p$itemVar)]
  setnames(mergeToTree, p$itemVar, p$parentVar)
  plotTree = copy(tree)
  tree = merge(tree, mergeToTree, by = p$parentVar, all.x = TRUE)
  
  #### SHAREs 1 
  # share are the proportion of availability of each parent
  # on the total availability by child
  
  tree[,availability.child:=availability*get(p$extractVar)]
  tree[, newShare := availability.child / sum(availability.child, na.rm = TRUE),
       by = c(params$childVar)]         
  tree[, c(params$shareVar) :=
         ifelse(is.na(newShare), get(params$shareVar), newShare)]
  # tree[, c("newShare","availability.child", "availability") := NULL]
  tree[, c("newShare") := NULL]
  
  # share are the proportion of availability of each parent
  # on the total availability by child
  # if availability is negative
  # shares are  a proportion of the number of child of each parent
  
  if(dim(tree)[1]!=0){
    freqChild= data.table(table(tree[, get(params$childVar)]))
    setnames(freqChild, c("V1","N"), c(params$childVar, "freq"))
    tree=merge(tree, freqChild , by=params$childVar)
  }
  ### CRISTINA this function has to be used also when availability is NA
  
  tree[availability.child<=0|is.na(availability.child), negShare:=1/freq]
  # tree[availability.child<=0, availability.child:=0]
  
  # because a child can have some positive and negative availabilities
  # new avail. and shares have to be calculated for all the child
  # in order to make shares sum at 1
  
  tree[,sumPositiveAvail:=sum(availability.child*ifelse(availability.child>0,1,0),na.rm=TRUE),by = c(params$childVar)]
  
  tree[,tempAvailability:=ifelse(availability.child<=0|is.na(availability.child),negShare*sumPositiveAvail,availability)]
  
  tree[, newShare := ifelse(tempAvailability==0,negShare, tempAvailability / sum(tempAvailability, na.rm = TRUE)),
       by = c(params$childVar)]
  tree[,availability.child:=tempAvailability]
  tree[,availability:=availability.child]
  
  tree[,c("freq","tempAvailability","sumPositiveAvail","negShare","availability.child"):=NULL]
  tree[, c(params$shareVar) :=
         ifelse(is.na(newShare), get(params$shareVar), newShare)]
  tree[, newShare := NULL]
  
  # weight
  
  treeShares=plotTree
  treeShares=treeShares[!is.na(get(p$shareVar))]
  setnames(treeShares,"share","oldShare")
  tree=data.table(left_join(tree,treeShares,by=colnames(treeShares)[c(1:3,5:7)]))
  tree[, c(params$shareVar) :=
         ifelse(is.na(oldShare), get(params$shareVar), oldShare)]
  tree[, oldShare := NULL]
  
  
  tree[,weight:=1]
  tree[measuredItemChildCPC %in% zeroWeight , weight:=0]
  
  
  printTree = data.table(data.frame(tree))
  
  #####################
  roundNum = function(x){
    if(is.na(x)){
      return(x)
    }
    initialSign = sign(x)
    x = abs(x)
    x=round(x,4)
    x = x * initialSign
    x = prettyNum(x, big.mark = ",", scientific = FALSE)
    return(x)
  }
  ##############################
  
  printTree[,availability:=ifelse(is.na(availability), "-", sapply(availability, roundNum))]
  printTree[,share:=round(share,2)]
  
  setnames(printTree,"measuredItemChildCPC","measuredItemFbsSua")
  printTree=nameData("suafbs","sua_unbalanced",printTree)
  setnames(printTree,c("measuredItemFbsSua","measuredItemFbsSua_description"),c("Child","ChildName"))
  setnames(printTree,"measuredItemParentCPC","measuredItemFbsSua")
  printTree=nameData("suafbs","sua_unbalanced",printTree)
  setnames(printTree,c("measuredItemFbsSua","measuredItemFbsSua_description"),c("Parent","ParentName"))
  printTree=printTree[Child %in% printCodes,
                      c("Child","ChildName", "Parent","ParentName", p$extractVar, "availability","share","weight"),
                      with = FALSE]
  printTree[,ChildName:=strtrim(ChildName,20)]
  printTree[,ParentName:=strtrim(ParentName,20)]
  
  if(length(printCodes) > 0){
    cat("\n\nAvailability of Parent for Food Processing Calculation = Prod+Imp-Exp | Shares by Child | weight of children:")
    print(knitr::kable(printTree[Child %in% printCodes,
                                 c("Child","ChildName" ,"Parent", "ParentName",p$extractVar, "availability","share","weight"),
                                 with = FALSE], align = 'r'))
    
  }
  
  
  
  ### STEP 4: Compute FOOD PROCESSING
  
  
  foodProc=calculateFoodProc(data=data, params=p, tree=tree, zeroWeight=zeroWeight)
  
  data=merge(data,foodProc, by="measuredItemSuaFbs", all.x = TRUE)
  
  #integrate the food processing in the elements
  
  data[measuredElementSuaFbs==p$foodProcCode&!is.na(foodProcElement),Value:=foodProcElement]
  
  data[,foodProcElement:=NULL]
  
  tree[, c("availability","foodProcElement"):=NULL]
  data[,c("availability","updateFlag"):=NULL]
  data2keep=data.table(data.frame(data))
  setnames(data,"measuredItemSuaFbs","measuredItemFbsSua")
  data=nameData("suafbs","sua_unbalanced",data,except = c("measuredElementSuaFbs","geographicAreaM49","timePointYears"))
  setnames(data,"measuredItemFbsSua","measuredItemSuaFbs")
  if(length(printCodes) > 0){
    cat("\nsua_unbalanced + production + food processing caluclated (step 2 of suaFilling):")
    data = markUpdated(new = data, old = old, standParams = p)
    old = copy(data[,c(params$mergeKey,params$elementVar,"Value"),with=FALSE])
    printSUATableNames(data = data, standParams = p,
                       printCodes = printCodes)
  }
  data=data.table(data.frame(data2keep))
  
  
  ### STEP 5: Execute Sua Filling again with Food processing
  
  data[,c("availability","updateFlag"):=NULL]
  
  data=suaFilling_NW(data, p=p, tree=tree,
                     primaryCommodities = primaryEl,
                     debugFile = params$createIntermetiateFile, stockCommodities = stockCommodities,
                     utilizationTable = utilizationTable, imbalanceThreshold = 10,loop1=FALSE)
  
  data$newValue[is.na(data$newValue)] = data$Value[is.na(data$newValue)]
  
   ### STEP 3: Compute availability and SHARE 2 
  
  # Recalculate the imbalance
  data$Diff = data$Value - data$newValue
  data = as.data.table(data)

  data[, imbalance_Fix := sum(ifelse(is.na(Value), 0, Value) *
                              ifelse(get(p$elementVar) == p$productionCode, 1,
                                     ifelse(get(p$elementVar) == p$importCode, 1,
                                            ifelse(get(p$elementVar) == p$exportCode, -1,
                                                   ifelse(get(p$elementVar) == p$stockCode, -1,
                                                          ifelse(get(p$elementVar) == p$foodCode, -1,
                                                                 ifelse(get(p$elementVar) == p$foodProcCode, -1,
                                                                        ifelse(get(p$elementVar) == p$feedCode, -1,
                                                                               ifelse(get(p$elementVar) == p$wasteCode, -1,
                                                                                      ifelse(get(p$elementVar) == p$seedCode, -1,
                                                                                             ifelse(get(p$elementVar) == p$industrialCode, -1,
                                                                                                    ifelse(get(p$elementVar) == p$touristCode, -1,
                                                                                                           ifelse(get(p$elementVar) == p$residualCode, -1, 
                                                                                                                  0))))))))))))),
       by = c(p$mergeKey)]
  
  data = data %>%
    group_by(geographicAreaM49,measuredItemSuaFbs,timePointYears) %>%
    tidyr::complete(measuredElementSuaFbs,nesting(geographicAreaM49,measuredItemSuaFbs,timePointYears))%>%
    fill(type,.direction="up") %>%
    fill(type,.direction="down") %>%
    ungroup()
  
  data = dplyr::left_join(data,data_Stock_Industrial,
                          by=c("geographicAreaM49","measuredItemSuaFbs","timePointYears"))
  
  data$imbalance_Fix[is.na(data$imbalance_Fix)] = 0

  # We will first fix the balance for those feed-only items and then recalculate the imbalance
  
  # work here!
  data$Value_temp = data$Value
  data$Value_temp[is.na(data$Value_temp)] = 0
  data = data %>%
    group_by(geographicAreaM49,measuredItemSuaFbs,timePointYears) %>%
    mutate(Value=ifelse(measuredElementSuaFbs=="feed"&classification=="feedOnly"&imbalance_Fix>=0,
                        Value_temp+imbalance_Fix,Value)) %>%
    ungroup()
  
  data = as.data.table(data)
  
  data[, imbalance_Fix := sum(ifelse(is.na(Value), 0, Value) *
                            ifelse(get(p$elementVar) == p$productionCode, 1,
                                   ifelse(get(p$elementVar) == p$importCode, 1,
                                          ifelse(get(p$elementVar) == p$exportCode, -1,
                                                 ifelse(get(p$elementVar) == p$stockCode, -1,
                                                        ifelse(get(p$elementVar) == p$foodCode, -1,
                                                               ifelse(get(p$elementVar) == p$foodProcCode, -1,
                                                                      ifelse(get(p$elementVar) == p$feedCode, -1,
                                                                             ifelse(get(p$elementVar) == p$wasteCode, -1,
                                                                                    ifelse(get(p$elementVar) == p$seedCode, -1,
                                                                                           ifelse(get(p$elementVar) == p$industrialCode, -1,
                                                                                                  ifelse(get(p$elementVar) == p$touristCode, -1,
                                                                                                         ifelse(get(p$elementVar) == p$residualCode, -1, 
                                                                                                                0))))))))))))),
       by = c(p$mergeKey)] 
  
  data_temp1 = data %>%
    filter(measuredElementSuaFbs=="industrial") %>%
    group_by(measuredItemSuaFbs,measuredElementSuaFbs,geographicAreaM49,timePointYears) %>%
    mutate(industrialValue = round(Value,2)) %>%
    dplyr::select(-Value) %>%
    ungroup()
  # data_temp1$industrialValue[data_temp1$Protected==TRUE&(!is.na(data_temp1$Protected))] = NA
  data_temp2 = data %>%
    filter(measuredElementSuaFbs=="stockChange") %>%
    mutate(StockChangeValue = round(Value,2)) %>%
    dplyr::select(-Value) %>%
    ungroup()
  # data_temp2$StockChangeValue[data_temp2$Protected==TRUE(!is.na(data_temp2$Protected))] = NA
  data = dplyr::left_join(data,dplyr::select(data_temp1,
                                             geographicAreaM49,
                                             measuredItemSuaFbs,
                                             timePointYears,
                                             industrialValue),
                          by=c("geographicAreaM49","measuredItemSuaFbs","timePointYears"))
  data = dplyr::left_join(data,dplyr::select(data_temp2,
                                             geographicAreaM49,
                                             measuredItemSuaFbs,
                                             timePointYears,
                                             StockChangeValue),
                          by=c("geographicAreaM49","measuredItemSuaFbs","timePointYears"))  
  rm(data_temp1)
  rm(data_temp2)
  
  data$industrialValue[is.na(data$industrialValue)] = 0
  # Only activate stocks for those for which existed previously or are primary crops
  #data$StockChangeValue[is.na(data$StockChangeValue)] = 0
  data$StockChangeValue[is.na(data$StockChangeValue)&
                          ((!is.na(data$Median_Value_Stock))|
                          data$type=="CRPR")&(!is.na(data$type))] = 0
  # data$Median_Value_Stock[is.na(data$Median_Value_Stock)] = 0
  data$Median_Value_Stock[(is.na(data$Median_Value_Stock))&data$type=="CRPR"&(!is.na(data$type))] = 0
  data$Median_Value_Industrial[is.na(data$Median_Value_Industrial)] = 0
  
  # work here!
  data = data %>%
    group_by(geographicAreaM49,measuredItemSuaFbs,timePointYears) %>%
    mutate(Value=ifelse(measuredElementSuaFbs=="stockChange"&imbalance_Fix>=0,
                        StockChangeValue+imbalance_Fix*(Median_Value_Stock/(Median_Value_Stock+Median_Value_Industrial)),Value)) %>%
    mutate(Value=ifelse(measuredElementSuaFbs=="industrial"&imbalance_Fix>=0,
                        industrialValue+imbalance_Fix*(Median_Value_Industrial/(Median_Value_Stock+Median_Value_Industrial)),Value)) %>%
    mutate(Value=ifelse((measuredElementSuaFbs=="industrial"&(is.nan(Value)|is.na(Value))&imbalance_Fix>=0),
                        industrialValue+imbalance_Fix,Value)) %>%
    mutate(Value=ifelse((measuredElementSuaFbs=="stockChange"&is.nan(Value)),
                        NA,Value)) %>%
    mutate(Value=ifelse((measuredElementSuaFbs=="stockChange"&imbalance_Fix<0&type=="CRPR"&(!is.na(type))),
                        StockChangeValue+imbalance_Fix,Value)) %>%
    ungroup() %>%
    dplyr::select(-Median_Value_Stock,-Median_Value_Industrial,-industrialValue,-StockChangeValue,-imbalance_Fix,-Diff,-Value_temp)
  
  data = as.data.table(data)
  
  # data[, imbalance := sum(ifelse(is.na(Value), 0, Value) *
  #                               ifelse(get(p$elementVar) == p$productionCode, 1,
  #                                      ifelse(get(p$elementVar) == p$importCode, 1,
  #                                             ifelse(get(p$elementVar) == p$exportCode, -1,
  #                                                    ifelse(get(p$elementVar) == p$stockCode, -1,
  #                                                           ifelse(get(p$elementVar) == p$foodCode, -1,
  #                                                                  ifelse(get(p$elementVar) == p$foodProcCode, -1,
  #                                                                         ifelse(get(p$elementVar) == p$feedCode, -1,
  #                                                                                ifelse(get(p$elementVar) == p$wasteCode, -1,
  #                                                                                       ifelse(get(p$elementVar) == p$seedCode, -1,
  #                                                                                              ifelse(get(p$elementVar) == p$industrialCode, -1,
  #                                                                                                     ifelse(get(p$elementVar) == p$touristCode, -1,
  #                                                                                                            ifelse(get(p$elementVar) == p$residualCode, -1, 
  #                                                                                                                   NA))))))))))))),
  #      by = c(p$mergeKey)] 

  data_type = dplyr::select(data,geographicAreaM49,measuredItemSuaFbs,timePointYears,type)
  data = data %>%
    group_by(geographicAreaM49,measuredItemSuaFbs,timePointYears) %>%
    tidyr::complete(measuredElementSuaFbs,nesting(geographicAreaM49,measuredItemSuaFbs,timePointYears))%>%
    fill(type,.direction="up") %>%
    fill(type,.direction="down") %>%
    ungroup()
  data_type = unique(data_type)
  
  data = dplyr::select(data,-classification)
  data = as.data.table(data)
  
  data=merge(data,foodProc, by="measuredItemSuaFbs", all.x = TRUE)
  
  setnames(data,"foodProcElement","availability")
  
  if(dim(tree)[1]!=0){
    
    # There's only one availability value per group, but we need an aggregation
    # function so we use mean.
    mergeToTree = data[, list(availability = mean(availability)),
                       by = c(p$itemVar)]
    setnames(mergeToTree, p$itemVar, p$parentVar)
    # plotTree = copy(tree)
    tree = merge(tree, mergeToTree, by = p$parentVar, all.x = TRUE)
    # tree[, availability := NULL]
    
    
    tree[get(standParams$childVar) %in% cutItems,
         c(standParams$childVar) := paste0("f???_", get(standParams$childVar))]
    
    
    
    availability = calculateAvailability(tree, p)
    
    tree[, availability := NULL]
    
    tree = collapseEdges(edges = tree, parentName = p$parentVar,
                         childName = p$childVar,
                         extractionName = p$extractVar,
                         keyCols = NULL)
    #####
    #### CRISTINA adding steps to avoiding multiple lines for each 
    # combination fo parent child due to the fact that
    # the same combination can happen because a child can be
    # also a nephew of a commodity
    # I'm taking mean share, mean ER and only one type
    tree[,p$extractVar:=mean(get(p$extractVar),na.rm=TRUE),by=c(p$parentVar,p$childVar)]
    tree[,share:=mean(share,na.rm=TRUE),by=c(p$parentVar,p$childVar)]
    # tree = tree[,c(1:4),with=FALSE]
    tree=unique(tree)
    tree[, target := ifelse(get(p$parentVar) %in% FPCommodities,
                            "F", "B")]
    tree = merge(tree, itemMap, by = "measuredItemParentCPC")
    tree[,standParentID:=NA]
    tree[,weight:=1]
    tree[get(p$childVar) %in% zeroWeight , weight:=0]
    #####
    
    
    tree = merge(tree, availability,
                 by = c(p$childVar, p$parentVar))
    
    tree=calculateShares(data=data, params=p, tree=tree, zeroWeight=zeroWeight)
    
    
    treeShares[get(standParams$childVar) %in% cutItems,
               c(standParams$childVar) := paste0("f???_", get(standParams$childVar))]
    
    # treeShares=treeShares[,c(2,1,3,5,7,4),with=FALSE]
    treeShares=treeShares[,mget(c("measuredItemChildCPC","measuredItemParentCPC","extractionRate",
                                  "target","standParentID","oldShare"))]
    tree=data.table(left_join(tree,treeShares,by=colnames(treeShares)[c(1:5)]))
    tree[, c(params$shareVar) :=
           ifelse(is.na(oldShare), get(params$shareVar), oldShare)]
    tree[, oldShare := NULL]
    
    
    
    
    
    ## STEP 2.1 Compute calories
    if(!is.null(nutrientData)){
      data = merge(data, nutrientData, by = p$itemVar, all.x = TRUE)
      data[rowSums(is.na(data.table(Calories, Proteins, Fats))) > 0, 
           c("Calories", "Proteins", "Fats") := 
             0]
      ## Convert nutrient values into total nutrient info using food
      ## allocation.
      
      ## Please note that we added the multiplicative factor of 10000 because the unit of measurement
      ## of the nutreient componets is 1/100g
      
      
      sapply(nutrientElements, function(nutrient){
        data[, c(nutrient) := (get(nutrient) * Value[get(p$elementVar) == p$foodCode])*10000,
             by = c(p$itemVar)]
      })
    }
    data2keep=copy(data.table(data.frame(data)))
    
    ##################### 
    setnames(data, "measuredItemSuaFbs", "measuredItemFbsSua")
    standData=data.table(data.frame(data))
    standData=standData[,.(geographicAreaM49, measuredElementSuaFbs, measuredItemFbsSua, timePointYears,
                           Value,Calories,Proteins,Fats)]
    standData = calculateFoodAggregates(standData,p,yearVals) 
    
    standData=standData[!is.na(measuredElementSuaFbs)]
    
    #########
    standDatawide = dcast(standData, geographicAreaM49 +timePointYears + measuredItemFbsSua + Calories + Proteins + Fats +
                            DESfoodSupply_kCd  + proteinSupplyQt_gCd + fatSupplyQt_gCd
                          ~ measuredElementSuaFbs,value.var = "Value")
    
    standDataLong = melt(standDatawide,id.vars = c("geographicAreaM49", "timePointYears","measuredItemFbsSua"),
                         variable.name = "measuredElementSuaFbs", value.name = "Value")
    standDataLong = data.table(data.frame(standDataLong))
    standDataLong[,measuredElementSuaFbs:=as.character(measuredElementSuaFbs)]
    standDataLong=data.table(data.frame(standDataLong))
    standDataLong=standDataLong[,.(geographicAreaM49, measuredElementSuaFbs, measuredItemFbsSua, timePointYears,
                                   Value)]
    data2print=data.table(standDataLong[!(measuredElementSuaFbs%in%c(nutrientElements,"proteinSupplyQt_gCd","fatSupplyQt_gCd"))])

    data=data.table(data.frame(data2print))
    setnames(data, "measuredItemFbsSua", "measuredItemSuaFbs")
    #####################
    data2keep2=data.table(data.frame(data))
    setnames(data,"measuredItemSuaFbs","measuredItemFbsSua")
    data=nameData("suafbs","sua_unbalanced",data,except = c("measuredElementSuaFbs","geographicAreaM49","timePointYears"))
    setnames(data,"measuredItemFbsSua","measuredItemSuaFbs")
    
    if(length(printCodes) > 0){
      cat("\nsua_balanced:")
      data = markUpdated(new = data, old = old, standParams = p)
      old = copy(data[,c(params$mergeKey,params$elementVar,"Value"),with=FALSE])
      printSUATableNames(data = data, standParams = p,
                         printCodes = printCodes,
                         nutrientElements = "DESfoodSupply_kCd",
                         printProcessing = TRUE)
    }
    data=data.table(data.frame(data2keep2))
    
    
    #####################
    # Computing Rice
    # To express Rice Paddy in mille equivalent: 
    # 1. Again put the data in wide form
    
    
    
    #####################
    roundNum = function(x){
      if(is.na(x)){
        return(x)
      }
      initialSign = sign(x)
      x = abs(x)
      x=round(x,4)
      x = x * initialSign
      x = prettyNum(x, big.mark = ",", scientific = FALSE)
      return(x)
    }
    ##############################
    printTree = data.table(data.frame(tree))
    printTree[,availability:=ifelse(is.na(availability), "-", sapply(availability, roundNum))]
    printTree[,share:=round(share,2)]
    printTree[,extractionRate:=round(extractionRate,2)]
    
    setnames(printTree,"measuredItemChildCPC","measuredItemFbsSua")
    printTree=nameData("suafbs","sua_unbalanced",printTree)
    setnames(printTree,c("measuredItemFbsSua","measuredItemFbsSua_description"),c("Child","ChildName"))
    setnames(printTree,"measuredItemParentCPC","measuredItemFbsSua")
    printTree=nameData("suafbs","sua_unbalanced",printTree)
    setnames(printTree,c("measuredItemFbsSua","measuredItemFbsSua_description"),c("Parent","ParentName"))
    printTree=printTree[Child %in% printCodes,
                        c("Child","ChildName", "Parent","ParentName", p$extractVar, "availability","share","weight"),
                        with = FALSE]
    printTree[,ChildName:=strtrim(ChildName,20)]
    printTree[,ParentName:=strtrim(ParentName,20)]
    
    if(length(printCodes) > 0){
      cat("\n\nAvailability of parents in terms of their children = FoodProc * eR | Final Shares by child:")
      print(knitr::kable(printTree[Child %in% printCodes,
                                   c("Child","ChildName" ,"Parent", "ParentName",p$extractVar, "availability","share","weight"),
                                   with = FALSE], align = 'r'))
    }
  }    
  
  ### first intermediate SAVE
  message("Attempting to save balanced SUA data...")
  setnames(dataFlags,"measuredItemSuaFbs", "measuredItemFbsSua")
  standData=data.table(left_join(standDataLong,dataFlags,by=colnames(standDataLong)))
  standData[is.na(flagObservationStatus), flagObservationStatus := "I"]
  standData[is.na(flagMethod), flagMethod := "e"]
  ###################
  
  fbs_sua_conversion2 <- data.table(measuredElementSuaFbs=c("Calories", "Fats", "Proteins","DESfoodSupply_kCd","proteinSupplyQt_gCd","fatSupplyQt_gCd", "exports", "feed", "food",
                                                            "foodManufacturing", "imports", "loss", "production",
                                                            "seed", "stockChange", "residual","industrial", "tourist"),
                                    code=c("261", "281", "271","664","674","684","5910", "5520", "5141",
                                           "5023", "5610", "5016", "5510",
                                           "5525", "5071", "5166","5165", "5164"))
  
  
  standData = merge(standData, fbs_sua_conversion2, by = "measuredElementSuaFbs")
  standData = data.table(data.frame(standData))
  standData[,`:=`(measuredElementSuaFbs = NULL)]
  setnames(standData, "code", "measuredElementSuaFbs")
  standData <- standData[!is.na(Value),]
  # standData <- standData[,Value:=round(Value,0)]
  
  
  if(!is.null(debugFile)){
    
    saveFBSItermediateStep(directory=paste0(basedir,"/debugFile/Batch_",batchnumber),
                           fileName=paste0("B",batchnumber,"_02_AfterSuaFilling_BeforeST"),
                           data=standData)
  }
  ###
  data=data.table(data.frame(data2keep))
  ###
  
  
  
  ## STEP 4: Standardize commodities to balancing level
  data = finalStandardizationToPrimary(data = data, tree = tree,
                                       standParams = p, sugarHack = FALSE,
                                       specificTree = FALSE,
                                       cut=cutItems,
                                       additiveElements = nutrientElements)
  # ###################################################
  
  data2keep=copy(data.table(data.frame(data)))
  
  # ###################################################
  
  setnames(data, "measuredItemSuaFbs", "measuredItemFbsSua")
  data=data.table(data.frame(data))
  
  standDatawide = dcast(data, geographicAreaM49 +timePointYears + measuredItemFbsSua
                        ~ measuredElementSuaFbs,value.var = "Value")
  
  standDataLong = melt(standDatawide,id.vars = c("geographicAreaM49", "timePointYears","measuredItemFbsSua",nutrientElements),variable.name = "measuredElementSuaFbs", value.name = "Value")
  standDataLong = data.table(data.frame(standDataLong))
  standDataLong[,measuredElementSuaFbs:=as.character(measuredElementSuaFbs)]
  
  standData=standDataLong[,.(geographicAreaM49, measuredElementSuaFbs, measuredItemFbsSua, timePointYears,
                             Value,Calories,Proteins,Fats)]
  
  standData = calculateFoodAggregates(standData,p,yearVals) ####
  
  standData=standData[!is.na(measuredElementSuaFbs)]
  
  standDatawide = dcast(standData, geographicAreaM49 +timePointYears + measuredItemFbsSua + 
                          Calories + Proteins + Fats +
                          DESfoodSupply_kCd  + proteinSupplyQt_gCd + fatSupplyQt_gCd ~ measuredElementSuaFbs,value.var = "Value")
  
  standDataLong = melt(standDatawide,id.vars = c("geographicAreaM49", "timePointYears","measuredItemFbsSua"),variable.name = "measuredElementSuaFbs", value.name = "Value")
  
  
  #########
  standDataLong=data.table(data.frame(standDataLong))
  standDataLong=standDataLong[,.(geographicAreaM49, measuredElementSuaFbs, measuredItemFbsSua, timePointYears,
                                 Value)]
  
  
  
  
  data2print=data.table(standDataLong[!(measuredElementSuaFbs%in%c(nutrientElements,"proteinSupplyQt_gCd","fatSupplyQt_gCd"))])
  
  data=data.table(data.frame(data2print))
  setnames(data, "measuredItemFbsSua", "measuredItemSuaFbs")
  
  
  
  ####################################################
  setnames(data,"measuredItemSuaFbs","measuredItemFbsSua")
  data=nameData("suafbs","sua_unbalanced",data,except = c("measuredElementSuaFbs","geographicAreaM49","timePointYears"))
  setnames(data,"measuredItemFbsSua","measuredItemSuaFbs")
  
  data0113Print = data[measuredItemSuaFbs!="23161.02"]
  
  if(length(printCodes) > 0){
    # cat("\nSUA table after standardization (BEFORE PROTECTED CORRECTION:)")
    cat("\n\nfbs_standardized")
    data0113Print = markUpdated(new = data0113Print, old = old, standParams = p)
    old = copy(data0113Print[,c(params$mergeKey,params$elementVar,"Value"),with=FALSE])
    printSUATableNames(data = data0113Print, standParams = p,
                       printCodes = printCodes,
                       nutrientElements = "DESfoodSupply_kCd",
                       printProcessing = TRUE)
  }
  
  data23610Print = data[measuredItemSuaFbs!="0113"]
  
  
  if(length(printCodes) > 0 & "0113"%in%printCodes){
    # cat("\nSUA table after standardization (BEFORE PROTECTED CORRECTION:)")
    cat("\n\nfbs_standardized with Rice Quantites expressed in Milled equivalent")
    data23610Print = markUpdated(new = data23610Print, old = old, standParams = p)
    old = copy(data23610Print[,c(params$mergeKey,params$elementVar,"Value"),with=FALSE])
    printSUATableNames(data = data23610Print, standParams = p,
                       printCodes = printCodes,
                       nutrientElements = "DESfoodSupply_kCd",
                       printProcessing = TRUE)
  }
  
  
  #### CRISTINA delete the FoodMAnufacturin rows for the cut items
  # because these have the code with the prefix f???_
  # this generates problems when doing the saving back of the second step.
  
  data=copy(data.table(data.frame(data2keep)))
  data=data[!grepl("f???_",measuredItemSuaFbs)]
  
  
  ############################################################
  
  
  ### Second intermediate Save  
  message("Attempting to save unbalanced FBS data...")
  
  setnames(data, "measuredItemSuaFbs", "measuredItemFbsSua")
  fbs_sua_conversion <- data.table(measuredElementSuaFbs=c("Calories", "Fats", "Proteins", "exports", "feed", "food",
                                                           "foodManufacturing", "imports", "loss", "production",
                                                           "seed", "stockChange", "residual","industrial", "tourist"),
                                   code=c("261", "281", "271","5910", "5520", "5141",
                                          "5023", "5610", "5016", "5510",
                                          "5525", "5071", "5166","5165", "5164"))
  
  # standData = merge(data, fbs_sua_conversion, by = "measuredElementSuaFbs")
  # standData[,`:=`(measuredElementSuaFbs = NULL)]
  # setnames(standData, "code", "measuredElementSuaFbs")
  
  standData=copy(data)
  
  standDatawide = dcast(standData, geographicAreaM49 +timePointYears + measuredItemFbsSua 
                        ~ measuredElementSuaFbs,value.var = "Value")
  
  standData = melt(standDatawide,id.vars = c("geographicAreaM49", "timePointYears","measuredItemFbsSua","Calories", "Proteins","Fats"),variable.name = "measuredElementSuaFbs", value.name = "Value")
  
  standData=data.table(data.frame(standData))
  standData=standData[,.(geographicAreaM49, measuredElementSuaFbs, measuredItemFbsSua, timePointYears,
                         Value,Calories,Proteins,Fats)]
  
  
  standData = calculateFoodAggregates(standData,p,yearVals) ####
  
  
  standDatawide = dcast(standData, geographicAreaM49 +timePointYears + measuredItemFbsSua + 
                          Calories + Proteins + Fats +
                          DESfoodSupply_kCd  + proteinSupplyQt_gCd + fatSupplyQt_gCd ~ measuredElementSuaFbs,value.var = "Value")
  
  

  standDataLong = melt(standDatawide,id.vars = c("geographicAreaM49", "timePointYears","measuredItemFbsSua"),variable.name = "measuredElementSuaFbs", value.name = "Value")
  

  fbs_sua_conversion2 <- data.table(measuredElementSuaFbs=c("Calories", "Fats", "Proteins","DESfoodSupply_kCd","proteinSupplyQt_gCd","fatSupplyQt_gCd", "exports", "feed", "food",
                                                            "foodManufacturing", "imports", "loss", "production",
                                                            "seed", "stockChange", "residual","industrial", "tourist"),
                                    code=c("261", "281", "271","664","674","684","5910", "5520", "5141",
                                           "5023", "5610", "5016", "5510",
                                           "5525", "5071", "5166","5165", "5164"))
  
  standData = merge(standDataLong, fbs_sua_conversion2, by = "measuredElementSuaFbs")

  standData = data.table(data.frame(standData))
  standData[,`:=`(measuredElementSuaFbs = NULL)]
  setnames(standData, "code", "measuredElementSuaFbs")
  
  standData=data.table(data.frame(standData))
  standData=standData[,.(geographicAreaM49, measuredElementSuaFbs, measuredItemFbsSua, timePointYears,
                         Value)]
  standData <- standData[!is.na(Value),]
  
  
  ### Cristina Merge with FbsTree for saving only the PrimaryEquivalent CPC codes
  standData=merge(standData,fbsTree,by.x="measuredItemFbsSua",by.y="measuredItemSuaFbs")
  ###
  
  standData=data.table(data.frame(standData))
  
  standData=standData[,.(geographicAreaM49, measuredElementSuaFbs, measuredItemFbsSua, timePointYears, 
                         Value)]
  standData[, flagObservationStatus := "I"]
  standData[, flagMethod := "s"]
  # standData <- standData[,Value:=round(Value,0)]
  
  
  if(!is.null(debugFile)){
    
    saveFBSItermediateStep(directory=paste0(basedir,"/debugFile/Batch_",batchnumber),
                           fileName=paste0("B",batchnumber,"_03_AfterST_BeforeFBSbal"),
                           data=standData)
  }
  
  setnames(data, "measuredItemFbsSua", "measuredItemSuaFbs")
  ####
  
  coutryT=uniqueLevels[i,geographicAreaM49]
  # save(tree,file=paste0(basedir,"/debugFile/Batch_",batchnumber,"/_tree_",coutryT,"_",yearT,".RData"))
  
  
  ## STEP 5: Balance at the balancing level.
  data = data[get(p$elementVar) %in% c(p$productionCode, p$importCode, p$exportCode,
                                       p$stockCode, p$foodCode, p$feedCode, p$seedCode,
                                       p$touristCode, p$industrialCode, p$wasteCode,
                                       nutrientElements, p$foodProcCode,p$residualCode), ]
  data[, nutrientElement := get(p$elementVar) %in% nutrientElements]
  warning("Not sure how to compute standard deviations!  Currently just 10% ",
          "of value!")
  
  data[, standardDeviation := Value * .1]
  
  ##Production
  # data[get(p$elementVar)==p$productionCode, standardDeviation := Value * .01]
  data[get(p$elementVar)==p$productionCode, standardDeviation := Value * .001] # Batch 119
  ##Import
  data[get(p$elementVar)==p$importCode, standardDeviation := Value * .02]
  ##Export
  data[get(p$elementVar)==p$exportCode, standardDeviation := Value * .02]
  ##Stock
  data[get(p$elementVar)==p$stockCode, standardDeviation := Value * .25]
  ##Food
  data[get(p$elementVar)==p$foodCode, standardDeviation := Value * .025]
  ##Feed
  data[get(p$elementVar)==p$feedCode, standardDeviation := Value * .25]
  ##Seed
  data[get(p$elementVar)==p$seedCode, standardDeviation := Value * .25]
  ##Tourist
  data[get(p$elementVar)==p$touristCode, standardDeviation := Value * .25]
  ##Industrial
  data[get(p$elementVar)==p$industrialCode, standardDeviation := Value * .25]
  ##Waste
  data[get(p$elementVar)==p$wasteCode, standardDeviation := Value * .25]
  ##Food processing
  data[get(p$elementVar)==p$foodProcCode, standardDeviation := Value * .25]
  
  data[!get(p$elementVar) %in% nutrientElements,
       balancedValue := faoswsBalancing::balancing(param1 = sapply(Value, na2zero),
                                                   param2 = sapply(standardDeviation, na2zero),
                                                   sign = ifelse(get(p$elementVar) %in% c(p$residualCode),0,
                                                                 ifelse(get(p$elementVar) %in% c(p$productionCode, p$importCode), 1, -1)),
                                                   
                                                   lbounds = ifelse(get(p$elementVar) %in% c(p$stockCode, p$touristCode), -Inf, 0),
                                                   optimize = "constrOptim", constrTol = 1e-6),
       by = c(p$itemVar)]
  
  ## To adjust calories later, compute the ratio for how much food has been 
  ## adjusted by.  This looks like a "mean", but really we're just using the
  ## mean to select the one non-NA element.
  data[, foodAdjRatio := mean(ifelse(get(p$elementVar) == p$foodCode,
                                     balancedValue / Value, NA),
                              na.rm = TRUE),
       by = c(p$itemVar)]
  ## The balancedValue will be given for all non-nutrient elements.  Update
  ## all these elements with their balanced values.
  # NW Here I have intervened again to ensure that only non-official data is changed
  # The difference is sent to stock change and industrial uses
  data[!(nutrientElement)&!Protected==TRUE, Value := balancedValue]
  data$balancedValue[is.na(data$balancedValue)] = data$Value[is.na(data$balancedValue)]
  data_type = distinct(data_type,geographicAreaM49,timePointYears,measuredItemSuaFbs,.keep_all = TRUE)
  data = dplyr::left_join(data,data_type,by=c("geographicAreaM49",
                                                   "timePointYears",
                                                   "measuredItemSuaFbs"))
  data = data %>%
    group_by(geographicAreaM49,measuredItemSuaFbs,timePointYears) %>%
    fill(type,.direction="up") %>%
    fill(type,.direction="down") %>%
    ungroup()
  
  data = as.data.table(data)
  
  data$Diff = data$Value - data$balancedValue
  data$Diff[is.na(data$Diff)] = 0
  data= as.data.table(data)
  
  data$Diff = data$Value - data$balancedValue
  
  data[, imbalance_Fix := sum(ifelse(is.na(Value), 0, Diff) *
                                ifelse(get(p$elementVar) == p$productionCode, 1,
                                       ifelse(get(p$elementVar) == p$importCode, 1,
                                              ifelse(get(p$elementVar) == p$exportCode, -1,
                                                     ifelse(get(p$elementVar) == p$stockCode, -1,
                                                            ifelse(get(p$elementVar) == p$foodCode, -1,
                                                                   ifelse(get(p$elementVar) == p$foodProcCode, -1,
                                                                          ifelse(get(p$elementVar) == p$feedCode, -1,
                                                                                 ifelse(get(p$elementVar) == p$wasteCode, -1,
                                                                                        ifelse(get(p$elementVar) == p$seedCode, -1,
                                                                                               ifelse(get(p$elementVar) == p$industrialCode, -1,
                                                                                                      ifelse(get(p$elementVar) == p$touristCode, -1,
                                                                                                             ifelse(get(p$elementVar) == p$residualCode, -1, 
                                                                                                                    0))))))))))))),
       by = c(p$mergeKey)]
  
  data = dplyr::left_join(data,data_Stock_Industrial,
                          by=c("geographicAreaM49","measuredItemSuaFbs","timePointYears"))
  
  data$imbalance_Fix[is.na(data$imbalance_Fix)] = 0
  
  # We will first fix the balance for those feed-only items and then recalculate the imbalance
  
  # work here!
  data$Value_temp = data$Value
  data$Value_temp[is.na(data$Value_temp)] = 0
  data = data %>%
    group_by(geographicAreaM49,measuredItemSuaFbs,timePointYears) %>%
    mutate(Value=ifelse(measuredElementSuaFbs=="feed"&classification=="feedOnly"&imbalance_Fix>=0,
                        Value_temp+imbalance_Fix,Value)) %>%
    ungroup()
  
  data$Diff = data$Value - data$balancedValue
  
  data = as.data.table(data)
  
  data[, imbalance_Fix := sum(ifelse(is.na(Value), 0, Diff) *
                                ifelse(get(p$elementVar) == p$productionCode, 1,
                                       ifelse(get(p$elementVar) == p$importCode, 1,
                                              ifelse(get(p$elementVar) == p$exportCode, -1,
                                                     ifelse(get(p$elementVar) == p$stockCode, -1,
                                                            ifelse(get(p$elementVar) == p$foodCode, -1,
                                                                   ifelse(get(p$elementVar) == p$foodProcCode, -1,
                                                                          ifelse(get(p$elementVar) == p$feedCode, -1,
                                                                                 ifelse(get(p$elementVar) == p$wasteCode, -1,
                                                                                        ifelse(get(p$elementVar) == p$seedCode, -1,
                                                                                               ifelse(get(p$elementVar) == p$industrialCode, -1,
                                                                                                      ifelse(get(p$elementVar) == p$touristCode, -1,
                                                                                                             ifelse(get(p$elementVar) == p$residualCode, -1, 
                                                                                                                    0))))))))))))),
       by = c(p$mergeKey)]
  
  # The difference is sent to stock change and industrial uses
  data_temp1 = data %>%
    filter(measuredElementSuaFbs=="industrial") %>%
    mutate(industrialValue = round(Value,2)) %>%
    dplyr::select(-Value)
  data_temp2 = data %>%
    filter(measuredElementSuaFbs=="stockChange") %>%
    mutate(StockChangeValue = round(Value,2)) %>%
    dplyr::select(-Value)
  data = dplyr::left_join(data,dplyr::select(data_temp1,
                                             geographicAreaM49,
                                             measuredItemSuaFbs,
                                             timePointYears,
                                             industrialValue),
                          by=c("geographicAreaM49","measuredItemSuaFbs","timePointYears"))
  data = dplyr::left_join(data,dplyr::select(data_temp2,
                                             geographicAreaM49,
                                             measuredItemSuaFbs,
                                             timePointYears,
                                             StockChangeValue),
                          by=c("geographicAreaM49","measuredItemSuaFbs","timePointYears"))  
  rm(data_temp1)
  rm(data_temp2)
  data$Diff[is.na(data$Diff)] = 0
  data$industrialValue[is.na(data$industrialValue)] = 0
  # Don't create stocks for those commodities that don't have them
  #data$StockChangeValue[is.na(data$StockChangeValue)] = 0
  
  # work here!
  data = data %>%
    group_by(geographicAreaM49,measuredItemSuaFbs,timePointYears) %>%
    mutate(Value=ifelse(measuredElementSuaFbs=="stockChange"&imbalance_Fix>=0,
                        StockChangeValue+imbalance_Fix*(abs(StockChangeValue)/(abs(StockChangeValue)+industrialValue)),Value)) %>%
    mutate(Value=ifelse(measuredElementSuaFbs=="industrial"&imbalance_Fix>=0,
                        industrialValue+imbalance_Fix*(industrialValue/(abs(StockChangeValue)+industrialValue)),Value)) %>%
    mutate(Value=ifelse((measuredElementSuaFbs=="industrial"&(is.na(Value)|is.nan(Value))&imbalance_Fix>=0),
                        industrialValue+imbalance_Fix,Value)) %>%
    mutate(Value=ifelse((measuredElementSuaFbs=="stockChange"&is.nan(Value)),
                        NA,Value)) %>%
    mutate(Value=ifelse((measuredElementSuaFbs=="stockChange"&imbalance_Fix<0&type=="CRPR"&(!is.na(type))),
                        Value+imbalance_Fix,Value)) %>%
    ungroup() %>%
    dplyr::select(-StockChangeValue,-industrialValue,-imbalance_Fix,-Diff)
  
  data = as.data.table(data)
  
  # Recalculate the balances
  data[!get(p$elementVar) %in% nutrientElements,
       balancedValue := faoswsBalancing::balancing(param1 = sapply(balancedValue, na2zero),
                                                   param2 = sapply(standardDeviation, na2zero),
                                                   sign = ifelse(get(p$elementVar) %in% c(p$residualCode),0,
                                                                 ifelse(get(p$elementVar) %in% c(p$productionCode, p$importCode), 1, -1)),
                                                   
                                                   lbounds = ifelse(get(p$elementVar) %in% c(p$stockCode, p$touristCode), -Inf, 0),
                                                   optimize = "constrOptim", constrTol = 1e-6),
       by = c(p$itemVar)]
  
  data = as.data.table(data)
  
  data2keep=data.table(data.frame(data)) ###################!!!!!!!!!!!!!!!!!!!!!!DATA
  
  #############################################################################  
  setnames(data, "measuredItemSuaFbs", "measuredItemFbsSua")
  standData=data.table(data.frame(data))
  
  standData=standData[,.(geographicAreaM49, measuredElementSuaFbs, measuredItemFbsSua, timePointYears, 
                         Value)]
  standDatawide = dcast(standData, geographicAreaM49 +timePointYears + measuredItemFbsSua 
                        ~ measuredElementSuaFbs,value.var = "Value")
  
  standData = melt(standDatawide,id.vars = c("geographicAreaM49", "timePointYears","measuredItemFbsSua","Calories", "Proteins","Fats"),variable.name = "measuredElementSuaFbs", value.name = "Value")
  
  standData=data.table(data.frame(standData))
  standData=standData[,.(geographicAreaM49, measuredElementSuaFbs, measuredItemFbsSua, timePointYears,
                         Value,Calories,Proteins,Fats)]
  
  
  standData = calculateFoodAggregates(standData,p,yearVals) ####
  standData=standData[!is.na(measuredElementSuaFbs)]
  
  standDatawide = dcast(standData, geographicAreaM49 +timePointYears + measuredItemFbsSua + 
                          Calories + Proteins + Fats +
                          DESfoodSupply_kCd  + proteinSupplyQt_gCd + fatSupplyQt_gCd ~ measuredElementSuaFbs,value.var = "Value")
  
  standDataLong = melt(standDatawide,id.vars = c("geographicAreaM49", "timePointYears","measuredItemFbsSua"),variable.name = "measuredElementSuaFbs", value.name = "Value")
  
  
  standDataLong = data.table(data.frame(standDataLong))
  
  standDataLong[,unique(measuredElementSuaFbs)]
  
  data2print=data.table(standDataLong[!(measuredElementSuaFbs%in%c(nutrientElements,"proteinSupplyQt_gCd","fatSupplyQt_gCd"))])
  
  data=data.table(data.frame(data2print))
  
  data=nameData("suafbs","sua_unbalanced",data,except = c("measuredElementSuaFbs","geographicAreaM49","timePointYears"))
  setnames(data,"measuredItemFbsSua","measuredItemSuaFbs")
  
  #######################################################
  
  
  if(length(printCodes) > 0){
    cat("\n\nfbs_balanced:")
    data = markUpdated(new = data, old = old, standParams = p)
    old = copy(data[,c(params$mergeKey,params$elementVar,"Value"),with=FALSE])
    printSUATableNames(data = data, standParams = p, printCodes = printCodes,
                       printProcessing = TRUE,
                       nutrientElements = "DESfoodSupply_kCd")
    data[, updateFlag := NULL]
  }
  
  
  
  ## STEP 6: Update calories of processed products proportionally based on
  ## updated food element values.
  
  data=copy(data.table(data.frame(data2keep)))   ###################!!!!!!!!!!!!!!!!!!!!!!DATA
  
  data[(nutrientElement), Value := ifelse(((!is.na(Value))&is.na(foodAdjRatio)),Value, Value * foodAdjRatio)]
  
  data2keep=data.table(data.frame(data)) ###################!!!!!!!!!!!!!!!!!!!!!!DATA
  
  ####################################################
  ####################################################
  ####################################################
  ####################################################
  # CRISTINA 08/03/2018
  # Express the final value of Rice in Milled equivalent
  
  dataRice=data[measuredItemSuaFbs=="0113"]
  dataRice[,measuredItemSuaFbs:="23161.02"]
  dataRice[!(measuredElementSuaFbs%in%c(nutrientElements)),Value:=Value*0.667]
  
  
  data0113 = copy(data.table(data.frame(data)))
  
  data23161 = copy(data.table(data.frame(dataRice)))
  
  dataBoth=rbind(data0113,data23161)
  
  dataNew=dataBoth[measuredItemSuaFbs!="0113"]
  
  data2keep=copy(data.table(data.frame(dataNew)))
  
  ####################################################
  ####################################################
  ####################################################
  ####################################################
  ###################################################
  ###################################################
  data=data.table(data.frame(copy(dataBoth)))
  
  
  #############################################################################  
  setnames(data, "measuredItemSuaFbs", "measuredItemFbsSua")
  standData=data.table(data.frame(data))
  
  standData=standData[,.(geographicAreaM49, measuredElementSuaFbs, measuredItemFbsSua, timePointYears, 
                         Value)]
  standDatawide = dcast(standData, geographicAreaM49 +timePointYears + measuredItemFbsSua 
                        ~ measuredElementSuaFbs,value.var = "Value")
  
  standData = melt(standDatawide,id.vars = c("geographicAreaM49", "timePointYears","measuredItemFbsSua","Calories", "Proteins","Fats"),variable.name = "measuredElementSuaFbs", value.name = "Value")
  
  standData=data.table(data.frame(standData))
  standData=standData[,.(geographicAreaM49, measuredElementSuaFbs, measuredItemFbsSua, timePointYears,
                         Value,Calories,Proteins,Fats)]
  
  
  standData = calculateFoodAggregates(standData,p,yearVals) ####
  standData=standData[!is.na(measuredElementSuaFbs)]
  
  standDatawide = dcast(standData, geographicAreaM49 +timePointYears + measuredItemFbsSua + 
                          Calories + Proteins + Fats +
                          DESfoodSupply_kCd  + proteinSupplyQt_gCd + fatSupplyQt_gCd ~ measuredElementSuaFbs,value.var = "Value")
  
  standDataLong = melt(standDatawide,id.vars = c("geographicAreaM49", "timePointYears","measuredItemFbsSua"),variable.name = "measuredElementSuaFbs", value.name = "Value")
  
  standDataLong=data.table(data.frame(standDataLong))
  
  data2print=data.table(standDataLong[!(measuredElementSuaFbs%in%c(nutrientElements,"proteinSupplyQt_gCd","fatSupplyQt_gCd"))])
  
  data=data.table(data.frame(data2print))
  setnames(data, "measuredItemFbsSua", "measuredItemSuaFbs")
  #############################################################################  
  setnames(data,"measuredItemSuaFbs","measuredItemFbsSua")
  data=nameData("suafbs","sua_unbalanced",data,except = c("measuredElementSuaFbs","geographicAreaM49","timePointYears"))
  setnames(data,"measuredItemFbsSua","measuredItemSuaFbs")
  
  data0113Print = data[measuredItemSuaFbs!="23161.02"]
  
  if(length(printCodes) > 0){
    
    cat("\n\nfbs_balanced with updated nutrient values:")
    data0113Print = markUpdated(new = data0113Print, old = old, standParams = p)
    old = copy(data0113Print[,c(params$mergeKey,params$elementVar,"Value"),with=FALSE])
    printSUATableNames(data = data0113Print, standParams = p, printCodes = printCodes,
                       printProcessing = TRUE,
                       nutrientElements = "DESfoodSupply_kCd")
    data0113Print[, updateFlag := NULL]
  }
  
  data23610Print = data[measuredItemSuaFbs!="0113"]
  
  if(length(printCodes) > 0 & "0113"%in%printCodes){
    
    cat("\n\nfbs_balanced with updated nutrient values and Rice in Milled equivalent:")
    data23610Print = markUpdated(new = data23610Print, old = old, standParams = p)
    old = copy(data23610Print[,c(params$mergeKey,params$elementVar,"Value"),with=FALSE])
    printSUATableNames(data = data23610Print, standParams = p, printCodes = printCodes,
                       printProcessing = TRUE,
                       nutrientElements = "DESfoodSupply_kCd")
    data23610Print[, updateFlag := NULL]
  }
  
  data=copy(data.table(data.frame(data2keep)))   ###################!!!!!!!!!!!!!!!!!!!!!!DATA
  data[, c("balancedValue", "nutrientElement", "foodAdjRatio") := NULL]
  
  ## STEP 7: Aggregate to FBS Level
  if(is.null(fbsTree)){
    # If no FBS tree, just return SUA-level results
    return(data)
  } else {
    out = computeFbsAggregate(data = data, fbsTree = fbsTree,
                              standParams = p)
    
    if(length(printCodes) > 0){
      printCodeTable = fbsTree[get(p$itemVar) %in% printCodes, ]
      # p$mergeKey[p$mergeKey == p$itemVar] = "fbsID4"
      # p$itemVar = "fbsID4"
      cat("\n\nFBS Table at first level of aggregation (fbs items):\n")
      #############################################################################  
      standData=data.table(data.frame(out[[1]]))
      standData=standData[,.(geographicAreaM49, measuredElementSuaFbs, fbsID4, timePointYears, 
                             Value)]
      standDatawide = dcast(standData, geographicAreaM49 +timePointYears + fbsID4 
                            ~ measuredElementSuaFbs,value.var = "Value")
      
      standData = melt(standDatawide,id.vars = c("geographicAreaM49", "timePointYears","fbsID4","Calories", "Proteins","Fats"),variable.name = "measuredElementSuaFbs", value.name = "Value")
      
      standData=data.table(data.frame(standData))
      standData=standData[,.(geographicAreaM49, measuredElementSuaFbs, fbsID4, timePointYears,
                             Value,Calories,Proteins,Fats)]
      
      setnames(standData,"fbsID4","measuredItemFbsSua")
      standData = calculateFoodAggregates(standData,p,yearVals) ####
      standData=standData[!is.na(measuredElementSuaFbs)]
      setnames(standData,"measuredItemFbsSua","fbsID4")
      
      standDatawide = dcast(standData, geographicAreaM49 +timePointYears + fbsID4 + 
                              Calories + Proteins + Fats +
                              DESfoodSupply_kCd  + proteinSupplyQt_gCd + fatSupplyQt_gCd ~ measuredElementSuaFbs,value.var = "Value")
      
      standDataLong = melt(standDatawide,id.vars = c("geographicAreaM49", "timePointYears","fbsID4"),variable.name = "measuredElementSuaFbs", value.name = "Value")
      
      data2print=data.table(standDataLong[!(measuredElementSuaFbs%in%c(nutrientElements,"proteinSupplyQt_gCd","fatSupplyQt_gCd"))])
      
      data=data.table(data.frame(data2print))
      #############################################################################  
      data2keep2=data.table(data.frame(data))
      # setnames(data,"fbsID4","measuredItemSuaFbs")
      data[,measuredItemFbsSua:=paste0("S",fbsID4)]
      data=nameData("suafbs","fbs_balanced_",data,except = c("measuredElementSuaFbs","geographicAreaM49","timePointYears"))
      data[,measuredItemFbsSua:=NULL]
      setnames(data,"fbsID4","measuredItemSuaFbs")
      
      printSUATableNames(data = data, standParams = p,
                         printCodes = printCodeTable[, fbsID4],
                         printProcessing = TRUE,
                         nutrientElements = "DESfoodSupply_kCd")
      data=data.table(data.frame(data2keep2))
      
      # p$mergeKey[p$mergeKey == p$itemVar] = "fbsID3"
      # p$itemVar = "fbsID3"
      cat("\n\nFBS Table at second level of aggregation (fbs aggregates):\n")
      #############################################################################  
      standData=data.table(data.frame(out[[2]]))
      standData=standData[,.(geographicAreaM49, measuredElementSuaFbs, fbsID3, timePointYears, 
                             Value)]
      standDatawide = dcast(standData, geographicAreaM49 +timePointYears + fbsID3 
                            ~ measuredElementSuaFbs,value.var = "Value")
      
      standData = melt(standDatawide,id.vars = c("geographicAreaM49", "timePointYears","fbsID3","Calories", "Proteins","Fats"),variable.name = "measuredElementSuaFbs", value.name = "Value")
      
      standData=data.table(data.frame(standData))
      standData=standData[,.(geographicAreaM49, measuredElementSuaFbs, fbsID3, timePointYears,
                             Value,Calories,Proteins,Fats)]
      
      setnames(standData,"fbsID3","measuredItemFbsSua")
      standData = calculateFoodAggregates(standData,p,yearVals) ####
      standData=standData[!is.na(measuredElementSuaFbs)]
      setnames(standData,"measuredItemFbsSua","fbsID3")
      
      standDatawide = dcast(standData, geographicAreaM49 +timePointYears + fbsID3 + 
                              Calories + Proteins + Fats +
                              DESfoodSupply_kCd  + proteinSupplyQt_gCd + fatSupplyQt_gCd ~ measuredElementSuaFbs,value.var = "Value")
      
      standDataLong = melt(standDatawide,id.vars = c("geographicAreaM49", "timePointYears","fbsID3"),variable.name = "measuredElementSuaFbs", value.name = "Value")
      
      data2print=data.table(standDataLong[!(measuredElementSuaFbs%in%c(nutrientElements,"proteinSupplyQt_gCd","fatSupplyQt_gCd"))])
      
      data=data.table(data.frame(data2print))
      #############################################################################  
      data2keep2=data.table(data.frame(data))
      # setnames(data,"fbsID4","measuredItemSuaFbs")
      data[,measuredItemFbsSua:=paste0("S",fbsID3)]
      data=nameData("suafbs","fbs_balanced_",data,except = c("measuredElementSuaFbs","geographicAreaM49","timePointYears"))
      data[,measuredItemFbsSua:=NULL]
      setnames(data,"fbsID3","measuredItemSuaFbs")
      
      printSUATableNames(data = data, standParams = p,
                         printCodes = printCodeTable[, fbsID3],
                         printProcessing = TRUE,
                         nutrientElements = "DESfoodSupply_kCd")
      data=data.table(data.frame(data2keep2))
      # p$mergeKey[p$mergeKey == p$itemVar] = "fbsID2"
      # p$itemVar = "fbsID2"
      cat("\n\nFBS Table at third level of aggregation (fbs macro aggregates):\n")
      #############################################################################  
      standData=data.table(data.frame(out[[3]]))
      standData=standData[,.(geographicAreaM49, measuredElementSuaFbs, fbsID2, timePointYears, 
                             Value)]
      standDatawide = dcast(standData, geographicAreaM49 +timePointYears + fbsID2 
                            ~ measuredElementSuaFbs,value.var = "Value")
      
      standData = melt(standDatawide,id.vars = c("geographicAreaM49", "timePointYears","fbsID2","Calories", "Proteins","Fats"),variable.name = "measuredElementSuaFbs", value.name = "Value")
      
      standData=data.table(data.frame(standData))
      standData=standData[,.(geographicAreaM49, measuredElementSuaFbs, fbsID2, timePointYears,
                             Value,Calories,Proteins,Fats)]
      
      setnames(standData,"fbsID2","measuredItemFbsSua")
      standData = calculateFoodAggregates(standData,p,yearVals) ####
      standData=standData[!is.na(measuredElementSuaFbs)]
      setnames(standData,"measuredItemFbsSua","fbsID2")
      
      standDatawide = dcast(standData, geographicAreaM49 +timePointYears + fbsID2 + 
                              Calories + Proteins + Fats +
                              DESfoodSupply_kCd  + proteinSupplyQt_gCd + fatSupplyQt_gCd ~ measuredElementSuaFbs,value.var = "Value")
      
      standDataLong = melt(standDatawide,id.vars = c("geographicAreaM49", "timePointYears","fbsID2"),variable.name = "measuredElementSuaFbs", value.name = "Value")
      
      data2print=data.table(standDataLong[!(measuredElementSuaFbs%in%c(nutrientElements,"proteinSupplyQt_gCd","fatSupplyQt_gCd"))])
      
      data=data.table(data.frame(data2print))
      #############################################################################  
      
      data2keep2=data.table(data.frame(data))
      # setnames(data,"fbsID4","measuredItemSuaFbs")
      data[,measuredItemFbsSua:=paste0("S",fbsID2)]
      data=nameData("suafbs","fbs_balanced_",data,except = c("measuredElementSuaFbs","geographicAreaM49","timePointYears"))
      data[,measuredItemFbsSua:=NULL]
      setnames(data,"fbsID2","measuredItemSuaFbs")
      
      printSUATableNames(data = data, standParams = p,
                         printCodes = printCodeTable[, fbsID2],
                         printProcessing = TRUE,
                         nutrientElements = "DESfoodSupply_kCd")
      data=data.table(data.frame(data2keep2))
      
      # p$mergeKey[p$mergeKey == p$itemVar] = "fbsID1"
      # p$itemVar = "fbsID1"
      cat("\n\nFBS Table at final level of aggregation (Grand Total):\n")
      #############################################################################  
      standData=data.table(data.frame(out[[4]]))
      standData=standData[,.(geographicAreaM49, measuredElementSuaFbs, fbsID1, timePointYears, 
                             Value)]
      standDatawide = dcast(standData, geographicAreaM49 +timePointYears + fbsID1 
                            ~ measuredElementSuaFbs,value.var = "Value")
      
      standData = melt(standDatawide,id.vars = c("geographicAreaM49", "timePointYears","fbsID1","Calories", "Proteins","Fats"),variable.name = "measuredElementSuaFbs", value.name = "Value")
      
      standData=data.table(data.frame(standData))
      standData=standData[,.(geographicAreaM49, measuredElementSuaFbs, fbsID1, timePointYears,
                             Value,Calories,Proteins,Fats)]
      
      setnames(standData,"fbsID1","measuredItemFbsSua")
      standData = calculateFoodAggregates(standData,p,yearVals) ####
      standData=standData[!is.na(measuredElementSuaFbs)]
      setnames(standData,"measuredItemFbsSua","fbsID1")
      
      standDatawide = dcast(standData, geographicAreaM49 +timePointYears + fbsID1 + 
                              Calories + Proteins + Fats +
                              DESfoodSupply_kCd  + proteinSupplyQt_gCd + fatSupplyQt_gCd ~ measuredElementSuaFbs,value.var = "Value")
      
      standDataLong = melt(standDatawide,id.vars = c("geographicAreaM49", "timePointYears","fbsID1"),variable.name = "measuredElementSuaFbs", value.name = "Value")
      
      data2print=data.table(standDataLong[!(measuredElementSuaFbs%in%c(nutrientElements,"proteinSupplyQt_gCd","fatSupplyQt_gCd"))])
      
      data=data.table(data.frame(data2print))
      
      #############################################################################  
      data2keep2=data.table(data.frame(data))
      # setnames(data,"fbsID4","measuredItemSuaFbs")
      data[,measuredItemFbsSua:=paste0("S",fbsID1)]
      data=nameData("suafbs","fbs_balanced_",data,except = c("measuredElementSuaFbs","geographicAreaM49","timePointYears"))
      data[,measuredItemFbsSua:=NULL]
      setnames(data,"fbsID1","measuredItemSuaFbs")
      
      printSUATableNames(data = data, standParams = p,
                         printCodes = printCodeTable[, fbsID1],
                         printProcessing = TRUE,
                         nutrientElements = "DESfoodSupply_kCd")
      data=data.table(data.frame(data2keep2))
    }
    
    
    ##  return(out)
  }
  
  
  outOut=list()  
  
  for (i in seq_len(length(out))) {
    
    
    outOut[[i]]= computeSupplyComponents(data=out[[i]], standParams=p,loop=i)
    
  }
  
  return(outOut)
  
}
