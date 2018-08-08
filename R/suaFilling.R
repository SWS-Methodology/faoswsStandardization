##' This function replaces the old BalanceResidual
##' 
##' This function forces a the filling of empty elements in the pulled SUA
##' by allocating the "imbalance" according to a Ranking of the possible 
##' Uitlizazions for each combination of country/commodity.
##' 
##' 1. PRODUCTION OF DERIVERD
##'    Production of derived commodities is created, when is not estimated via the 
##'    sub-module of derived and when utilization are higher than supplies.
##'    
##' 2. FOOD PROCESSING 
##'    with all productions, food processing is created for all parent commoditeis
##' 
##' 3. ALL OTHER ELEMENTS ARE FILLED, IF NEEDED
##' - if supply > utilization a Multiple filler approach is used: 
##'   * If all the ranked utilizations are present 
##'     these are proportionally incremented
##'   * if one is empty, imbalance goes to this commodity
##'   * if more than one is empty, Utilization are created based on a ranking of possible Utilization 
##'     coming from an external source.
##'     The approach used id the "Inverse Ranking Rule"
##'   
##' - If supply < utilization
##'   * All utilization are reduced by 30%
##'   * if this is not enought to cover the imbalance, production is incremented if is not official,
##'     if is official, utilization are firs reduced of another 30% and then production is reduced.
##' 
##' If OFFICIAL PRODUCTION is CHANGED, changed rows go in an external file
##' 
##' Trade is never touched
##' 
##' @param data The data.table containing the full dataset for standardization.
##' @param p The parameters for standardization.  These parameters 
##'   provide information about the columns of data and tree, specifying (for 
##'   example) which columns should be standardized, which columns represent 
##'   parents/children, etc.
##' @param primaryCommodities Primary level commodities (such as wheat, oranges,
##'   sweet potatoes, etc.) should not be balanced at this step but rather by
##'   the balancing algorithm.  This argument allows the user to specify a
##'   character vector with these primary element codes.
##' @param stockCommodities This list specify if the commodity can be allocated to stock
##' @param utilizationTable is the external utilizataion table
##' @param imbalanceThreshold The size that the imbalance must be in order for 
##'   an adjustment to be made.
##' @param cut these are primary equivalent commodities.
##' @param tree this is the sub tree used in the function.
##' @param loop1 insicates if is the firs or second loop before or after the food Proc calculation
##' @return the Value column of the passed data.table is updated 
##'   

suaFilling = function(data, p = p, tree=tree,
                         primaryCommodities = c(), stockCommodities = c(),
                         debugFile= NULL,
                         utilizationTable=c(), 
                         imbalanceThreshold = 10,loop1=TRUE){
  
  # The commodities that have to be crude balanced are the NON PRIMARY
  
  stopifnot(imbalanceThreshold > 0)
  
  eleToExcludeS = c(p$productionCode,p$exportCode,p$importCode,p$stockCode,p$foodProcCode)
  eleToExclude = c(p$productionCode,p$exportCode,p$importCode,p$foodProcCode)
  
  
  #############################
  # STEP 1: create production for subsequent food processing calculation:
  ############################# 
  
  ## Supply-Utilization = imbalance
  
  data[, imbalance := sum(ifelse(is.na(Value), 0, Value) *
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
                                                                                                                NA))))))))))))),
       by = c(p$mergeKey)]
  
  # we need the sum of Utilizations
  
  data[, sumUtils := sum(ifelse(is.na(Value), 0, Value) *
                           ifelse(get(p$elementVar) == p$productionCode, 0,
                                  ifelse(get(p$elementVar) == p$importCode, 0,
                                         ifelse(get(p$elementVar) == p$exportCode, 0,
                                                ifelse(get(p$elementVar) == p$stockCode, ifelse(is.na(Value),0,ifelse(Value<0,0,1)),
                                                       ifelse(get(p$elementVar) == p$foodCode, 1,
                                                              ifelse(get(p$elementVar) == p$foodProcCode, 0,
                                                                     ifelse(get(p$elementVar) == p$feedCode, 1,
                                                                            ifelse(get(p$elementVar) == p$wasteCode, 1,
                                                                                   ifelse(get(p$elementVar) == p$seedCode, 1,
                                                                                          ifelse(get(p$elementVar) == p$industrialCode, 1,
                                                                                                 ifelse(get(p$elementVar) == p$touristCode, 1,
                                                                                                        ifelse(get(p$elementVar) == p$residualCode, 1, 
                                                                                                               NA))))))))))))),
       by = c(p$mergeKey)]
  
  data[, sumSup := sum(ifelse(is.na(Value), 0, Value) *
                         ifelse(get(p$elementVar) == p$productionCode, 1,
                                ifelse(get(p$elementVar) == p$importCode, 1,
                                       ifelse(get(p$elementVar) == p$exportCode, 0,
                                              ifelse(get(p$elementVar) == p$stockCode, 0,
                                                     ifelse(get(p$elementVar) == p$foodCode, 0,
                                                            ifelse(get(p$elementVar) == p$foodProcCode, 0,
                                                                   ifelse(get(p$elementVar) == p$feedCode, 0,
                                                                          ifelse(get(p$elementVar) == p$wasteCode, 0,
                                                                                 ifelse(get(p$elementVar) == p$seedCode, 0,
                                                                                        ifelse(get(p$elementVar) == p$industrialCode, 0,
                                                                                               ifelse(get(p$elementVar) == p$touristCode, 0,
                                                                                                      ifelse(get(p$elementVar) == p$residualCode, 0,
                                                                                                             NA))))))))))))),
       by = c(p$mergeKey)]
  
  
  data[, sumSupstock := sum(ifelse(is.na(Value), 0, Value) *
                              ifelse(get(p$elementVar) == p$productionCode, 1,
                                     ifelse(get(p$elementVar) == p$importCode, 1,
                                            ifelse(get(p$elementVar) == p$exportCode, 0,
                                                   ifelse(get(p$elementVar) == p$stockCode,ifelse(is.na(Value),0,ifelse(Value<0,-1,0)),
                                                          ifelse(get(p$elementVar) == p$foodCode, 0,
                                                                 ifelse(get(p$elementVar) == p$foodProcCode, 0,
                                                                        ifelse(get(p$elementVar) == p$feedCode, 0,
                                                                               ifelse(get(p$elementVar) == p$wasteCode, 0,
                                                                                      ifelse(get(p$elementVar) == p$seedCode, 0,
                                                                                             ifelse(get(p$elementVar) == p$industrialCode, 0,
                                                                                                    ifelse(get(p$elementVar) == p$touristCode, 0,
                                                                                                           ifelse(get(p$elementVar) == p$residualCode, 0, 
                                                                                                                  NA))))))))))))),
       by = c(p$mergeKey)]
  
  
  # the following line added otherwise some cases were not treated at all 
  data[is.na(ProtectedProd),ProtectedProd:="FALSE"]
  data[is.na(ProtectedFood),ProtectedFood:="FALSE"]
  
  # I serparate the different blocks of data for trating them separately 
  
  dataPrimary = data[(get(p$itemVar) %in% primaryCommodities)]
  dataNoImbP = dataPrimary[imbalance<=imbalanceThreshold&imbalance>=(-imbalanceThreshold)] # never touched
  dataNegImbP = dataPrimary[imbalance < (-imbalanceThreshold)]
  dataPosImbP = dataPrimary[imbalance > imbalanceThreshold]
  
  dataNoPrimary = data[!(get(p$itemVar) %in% primaryCommodities)]
  dataNoImb = dataNoPrimary[imbalance<=imbalanceThreshold&imbalance>=(-imbalanceThreshold)] # never touched
  dataNegImb = dataNoPrimary[imbalance < (-imbalanceThreshold)]
  dataPosImb = dataNoPrimary[imbalance > imbalanceThreshold]
  
  ########################## Supply < utilization (= imbalance < -imbalanceThreshold)
  
  if (loop1==TRUE) {
    # # # if production EXISTS (protected or estimated Via the sub-module), 
    # # # DON'T DO ANYTHING. 
    # # if production do NOT EXISTS or is a NOT protected 0, create it
    # dataNegImb[get(p$elementVar)==p$productionCode & ProtectedProd==FALSE & (is.na(Value)|Value==0),
    #            newValue:=ifelse(is.na(Value),-imbalance,Value-imbalance)]
    #----------------------#
    # This is being changed the 12/04/2018 as per Salar request
    # create OR increase production any time is not sufficient to cover Import-Export
    dataNegImb[get(p$elementVar)==p$productionCode & ProtectedProd==FALSE,
               newValue:=ifelse(is.na(Value),-imbalance,Value-imbalance)]
    
    if("newValue" %in% colnames(dataPosImbP)){
      dataPosImbP[!is.na(newValue),Value:=newValue]
      # dataPosImbP[,newValue:=NULL]
      dataPosImbP=dataPosImbP[,1:18,with=FALSE]
    }
    if("newValue" %in% colnames(dataNegImb)){
      dataNegImb[!is.na(newValue),Value:=newValue]
      # dataPosImbP[,newValue:=NULL]
      dataNegImb=dataNegImb[,1:18,with=FALSE]
    }
    if("newValue" %in% colnames(dataPosImb)){
      dataPosImb[!is.na(newValue),Value:=newValue]
      # dataPosImbP[,newValue:=NULL]
      dataPosImb=dataPosImb[,1:18,with=FALSE]
    }
    data=rbind(dataNoImbP,dataNegImbP,dataNoImb,dataPosImbP,dataNegImb,dataPosImb)
    
  }  ### This refers only to the FIrst LOOP
  
  if (loop1==FALSE) {
    ######################### NEW VERSION 12/06/2017
    ######################### DEFINITIONS
    # First of all try to reduce UTilizations without tuching Food
    
    pTolerance = 0.3
    
    # 1. reduce the present Utilizations
    #    by the 30% of each utilization 
    #    stock here are incremented or reduced of an amount proportional 
    #    to their value in respect to other utilization
    
    # if the reduction of maximum 30% is enought to cover all imbalance
    dataNegImb_ptol=rbind(dataNegImb[abs(imbalance)<=(pTolerance*sumUtils)],
                          dataNegImbP[abs(imbalance)<=(pTolerance*sumUtils)])
    
    
    
    # dataNegImb_ptol=dataNegImb[abs(imbalance)<=(pTolerance*sumUtils)]
    dataNegImb_ptol[,newValue:= ifelse(is.na(Value),NA,
                                       ifelse(get(p$elementVar)%in%eleToExclude,NA,
                                              Value-abs(Value)*(abs(imbalance)/(sumUtils+(sumSupstock-sumSup)))))]  
    # if the all imbalance is NOT covered by the 30% of the all utilization
    # reduce the utilizations by 30% anyway
    
    dataNegImb_Noptol=rbind(dataNegImb[ # Production existing (either officiali or not)
      abs(imbalance)>(pTolerance*sumUtils)],
      dataNegImbP[ # Production existing (either officiali or not)
        abs(imbalance)>(pTolerance*sumUtils)])
    
    # dataNegImb_Noptol=dataNegImb[ # Production existing (either officiali or not)
    #   abs(imbalance)>(pTolerance*sumUtils)]
    # 
    dataNegImb_Noptol[,newValue:= ifelse(is.na(Value),NA,
                                         ifelse(get(p$elementVar)%in%eleToExclude,NA,
                                                Value-(pTolerance*(Value))))]
    dataNegImb_ptol[,Value:=ifelse(!is.na(newValue),newValue,Value)]
    dataNegImb_ptol[,newValue:=NULL]
    dataNegImb_Noptol[,Value:=ifelse(!is.na(newValue),newValue,Value)]
    dataNegImb_Noptol[,newValue:=NULL]
    
    dataNegImbAll=rbind(dataNegImb_ptol,dataNegImb_Noptol)
    dataNegImbComm=dataNegImbAll[,measuredItemSuaFbs]
    
    # dataNegImb=rbind(dataNegImb_ptol,dataNegImb_Noptol)
    # dataNegImbComm=dataNegImb[,measuredItemSuaFbs]
    ############################################################################################
    ############################################################################################
    ############################################################################################
    ############################################################################################
    # Now recalculate all steps for updating those commodities that have been balanced
    
    # First reconstruct the data
    
    data=data[!(measuredItemSuaFbs%in%dataNegImbComm)]
    data=rbind(data,dataNegImbAll)
    
    
    # Then start again
    ## Supply-Utilization = imbalance
    
    data[, imbalance := sum(ifelse(is.na(Value), 0, Value) *
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
                                                                                                                  NA))))))))))))),
         by = c(p$mergeKey)]
    
    # we need the sum of Utilizations
    
    data[, sumUtils := sum(ifelse(is.na(Value), 0, Value) *
                             ifelse(get(p$elementVar) == p$productionCode, 0,
                                    ifelse(get(p$elementVar) == p$importCode, 0,
                                           ifelse(get(p$elementVar) == p$exportCode, 0,
                                                  ifelse(get(p$elementVar) == p$stockCode, ifelse(is.na(Value),0,ifelse(Value<0,0,1)),
                                                         ifelse(get(p$elementVar) == p$foodCode, 1,
                                                                ifelse(get(p$elementVar) == p$foodProcCode, 0,
                                                                       ifelse(get(p$elementVar) == p$feedCode, 1,
                                                                              ifelse(get(p$elementVar) == p$wasteCode, 1,
                                                                                     ifelse(get(p$elementVar) == p$seedCode, 1,
                                                                                            ifelse(get(p$elementVar) == p$industrialCode, 1,
                                                                                                   ifelse(get(p$elementVar) == p$touristCode, 1,
                                                                                                          ifelse(get(p$elementVar) == p$residualCode, 1, 
                                                                                                                 NA))))))))))))),
         by = c(p$mergeKey)]
    
    data[, sumSup := sum(ifelse(is.na(Value), 0, Value) *
                           ifelse(get(p$elementVar) == p$productionCode, 1,
                                  ifelse(get(p$elementVar) == p$importCode, 1,
                                         ifelse(get(p$elementVar) == p$exportCode, 0,
                                                ifelse(get(p$elementVar) == p$stockCode, 0,
                                                       ifelse(get(p$elementVar) == p$foodCode, 0,
                                                              ifelse(get(p$elementVar) == p$foodProcCode, 0,
                                                                     ifelse(get(p$elementVar) == p$feedCode, 0,
                                                                            ifelse(get(p$elementVar) == p$wasteCode, 0,
                                                                                   ifelse(get(p$elementVar) == p$seedCode, 0,
                                                                                          ifelse(get(p$elementVar) == p$industrialCode, 0,
                                                                                                 ifelse(get(p$elementVar) == p$touristCode, 0,
                                                                                                        ifelse(get(p$elementVar) == p$residualCode, 0,
                                                                                                               NA))))))))))))),
         by = c(p$mergeKey)]
    
    
    data[, sumSupstock := sum(ifelse(is.na(Value), 0, Value) *
                                ifelse(get(p$elementVar) == p$productionCode, 1,
                                       ifelse(get(p$elementVar) == p$importCode, 1,
                                              ifelse(get(p$elementVar) == p$exportCode, 0,
                                                     ifelse(get(p$elementVar) == p$stockCode,ifelse(is.na(Value),0,ifelse(Value<0,-1,0)),
                                                            ifelse(get(p$elementVar) == p$foodCode, 0,
                                                                   ifelse(get(p$elementVar) == p$foodProcCode, 0,
                                                                          ifelse(get(p$elementVar) == p$feedCode, 0,
                                                                                 ifelse(get(p$elementVar) == p$wasteCode, 0,
                                                                                        ifelse(get(p$elementVar) == p$seedCode, 0,
                                                                                               ifelse(get(p$elementVar) == p$industrialCode, 0,
                                                                                                      ifelse(get(p$elementVar) == p$touristCode, 0,
                                                                                                             ifelse(get(p$elementVar) == p$residualCode, 0, 
                                                                                                                    NA))))))))))))),
         by = c(p$mergeKey)]
    
    
    # the following line added otherwise some cases were not treated at all 
    data[is.na(ProtectedProd),ProtectedProd:="FALSE"]
    data[is.na(ProtectedFood),ProtectedFood:="FALSE"]
    
    # I serparate the different blocks of data for trating them separately 
    
    dataPrimary = data[(get(p$itemVar) %in% primaryCommodities)]
    dataNoImbP = dataPrimary[imbalance <= imbalanceThreshold&imbalance>=(-imbalanceThreshold)] # never touched
    dataNegImbP = dataPrimary[imbalance < (-imbalanceThreshold)] # never touched
    dataPosImbP = dataPrimary[imbalance > imbalanceThreshold]
    
    dataNoPrimary = data[!(get(p$itemVar) %in% primaryCommodities)]
    dataNoImb = dataNoPrimary[imbalance <= imbalanceThreshold&imbalance>=(-imbalanceThreshold)] # never touched
    dataNegImb = dataNoPrimary[imbalance < (-imbalanceThreshold)]
    dataPosImb = dataNoPrimary[imbalance > imbalanceThreshold]
    
    dataPosImbAll = rbind(dataPosImb,dataPosImbP)
    
    ########################## Supply < utilization (= imbalance < -imbalanceThreshold)
    # if production is not official, create production
    dataNegImb[ProtectedProd=="FALSE" & get(p$elementVar)==p$productionCode,
               newValue:=ifelse(is.na(Value),-imbalance,Value-imbalance)]
    ##########################
    
    
    
    ########################################################  
    # if production is official 
    pTolerance = 0.3
    dataNegImbOffP = dataNegImb[ProtectedProd=="TRUE"]
    # 1. Try to reduce the present Utilizations
    #    only if at least the 70% of each utilization remains
    #    stock here are incremented or reduced of an amount proportional 
    #    to their value in respect to other utilization
    
    dataNegImb[ProtectedProd=="TRUE"&abs(imbalance)<=(pTolerance*sumUtils),
               newValue:= ifelse(is.na(Value),NA,
                                 ifelse(get(p$elementVar)%in%eleToExclude,NA,
                                        Value-abs(Value)*(abs(imbalance)/(sumUtils+(sumSupstock-sumSup)))))]
    
    # 2. If Imbalance is too high 
    # save this data outside 
    
    NoFillable=dataNegImbOffP[(abs(imbalance)>(pTolerance*sumUtils))]
    
    # & force the reduction of utilization (up to 30% of their value) & increase production
    dataNegImb[ProtectedProd=="TRUE"&abs(imbalance)>(pTolerance*sumUtils)&sumUtils>0,
               newValue:= ifelse(get(p$elementVar)%in%eleToExclude[-which(eleToExclude=="production")],NA,
                                 ifelse(get(p$elementVar)==p$productionCode,(ifelse(is.na(Value),0,Value)+abs(imbalance)-pTolerance*sumUtils),
                                        ifelse(is.na(Value),NA,Value-(abs(Value)/(sumUtils+(sumSupstock-sumSup)))
                                               *(sumUtils*pTolerance))))]
    # & force production always (not just if SumUtils==0)
    
    # dataNegImb[ProtectedProd=="TRUE"&abs(imbalance)>(pTolerance*sumUtils)&sumUtils==0,
    dataNegImb[ProtectedProd=="TRUE"&abs(imbalance)>(pTolerance*sumUtils),
               newValue:=ifelse(get(p$elementVar)==p$productionCode,
                                ifelse(is.na(Value),0,Value)+abs(imbalance),NA)]
    # ########################################################  
    
    ## Supply > utilization (= imbalance > imbalanceThreshold)
    
    ### Loop for Non Primary and primary commodity has been unified here (04/08/2018)
    # actualCommodities = dataPosImb[,unique(measuredItemSuaFbs)]
    
    actualCommodities = dataPosImbAll[,unique(measuredItemSuaFbs)]
    
    
    for (i in actualCommodities){
      # If none of the utilization is activable based in the utilization Table
      if(length(dataPosImbAll[measuredItemSuaFbs==i
                              &!(get(p$elementVar)%in%c(eleToExclude,p$stockCode))
                              &!is.na(rank),Value])==0){
        # CRISTINA change made august 2018
        # Save these data outside and send them for manual check and adjustment
        # (before everything was send conventionally on food)
        # dataPosImbAll[measuredItemSuaFbs==i
        #               &!(get(p$elementVar)%in%c(eleToExclude,p$stockCode))&
        #                 get(p$elementVar)==p$foodCode,newValue:=Value]
        
        NoBalanced = dataPosImbAll[measuredItemSuaFbs==i]
        
        setnames(NoBalanced, "measuredItemSuaFbs", "measuredItemFbsSua")
        standData = NoBalanced
        standData=standData[,.(geographicAreaM49, measuredElementSuaFbs, measuredItemFbsSua, timePointYears, 
                               Value)]
        standData <- standData[!is.na(Value),]
        
        standData[, flagObservationStatus := "I"]
        standData[, flagMethod := "x"]
        
        if(!is.null(debugFile)){
          
          saveFBSItermediateStep(directory=paste0(basedir,"/debugFile/Batch_",batchnumber),
                                 fileName=paste0("B",batchnumber,"_04_NotBalancedDerived"),
                                 data=standData)
        }
        
      }else{
        # Se tutti i Value sono popolati
        if(length(dataPosImbAll[measuredItemSuaFbs==i
                                &!(get(p$elementVar)%in%eleToExclude)
                                &!is.na(rank)&(is.na(Value)),Value])==0){
          # distribuisci inbalance proporzionalmente ai value stessi (considerando anche quelli che non hanno 
          # eventualmente ranking)
          # e diminuendo lo stock negativo, se presente
          
          # sumV=sum(dataPosImb[measuredItemSuaFbs==i
          #                     &!(get(p$elementVar)%in%eleToExclude)
          #                     # &!is.na(rank)
          #                     &Value>0,Value],na.rm=TRUE)
          # 
          dataPosImbAll[measuredItemSuaFbs==i
                        &!(get(p$elementVar)%in%eleToExclude)
                        # &!is.na(rank)                              ############### change 5/15/2018
                        &Value!=0,
                        newValue:=ifelse(is.na(Value),NA,
                                         Value+Value*(imbalance/(sumUtils+(sumSupstock-sumSup))))]
        }else{
          #se un valore non 'e popolato e non 'e stock
          if(length(dataPosImbAll[measuredItemSuaFbs==i
                                  &!(get(p$elementVar)%in%c(eleToExclude,p$stockCode))
                                  &!is.na(rank)&(is.na(Value)|Value==0),Value])==1){
            # metti tutto l' imbalance in questo elemento
            
            dataPosImbAll[measuredItemSuaFbs==i
                          &!(get(p$elementVar)%in%c(eleToExclude,p$stockCode))&!is.na(rank)
                          &(is.na(Value)|Value==0),
                          newValue:=imbalance]
            
          }else{
            # se c'e piu' di un elemento non popolato
            if(length(dataPosImbAll[measuredItemSuaFbs==i
                                    &!(get(p$elementVar)%in%c(eleToExclude,p$stockCode))
                                    &!is.na(rank)&(is.na(Value)|Value==0),Value])>1){
              # allora in base alla seguente funzione dei rank e rank inversi: NON PIU'
              
              # allora assegna il valore in base alla percentuale 
              sumPercent = sum(dataPosImbAll[measuredItemSuaFbs==i
                                             &!(get(p$elementVar)%in%c(eleToExclude,p$stockCode))
                                             &!is.na(rank)&(is.na(Value)|Value==0),percent])
              dataPosImbAll[measuredItemSuaFbs==i
                            &!(get(p$elementVar)%in%c(eleToExclude,p$stockCode))
                            &!is.na(rank)&(is.na(Value)|Value==0),newValue:=imbalance*(percent/sumPercent)]
              
              # sumRank = sum(dataPosImb[measuredItemSuaFbs==i
              #                          &!(get(p$elementVar)%in%eleToExclude)
              #                          &!is.na(rank)&(is.na(Value)|Value==0),rankInv])
              # dataPosImb[measuredItemSuaFbs==i
              #            &!(get(p$elementVar)%in%eleToExclude)
              #            &!is.na(rank)&(is.na(Value)|Value==0),newValue:=imbalance*(rankInv/sumRank)]
            }
          }
        }
        
      }
      
    }
    ############ End loop no primary
    ############
    ### Loop for primary
    # 04/08/2018 all the following part has been deleted because primaries with positive imbalanec are now balanced
    
    # 
    # # the loop is reduced to the commodities for which Food is NOT PROTECTED
    # actualCommoditiesP = dataPosImbP[measuredElementSuaFbs=="food"&ProtectedFood=="FALSE",unique(measuredItemSuaFbs)]
    # # actualCommoditiesP = dataPosImbP[,unique(measuredItemSuaFbs)]
    # 
    # for (i in actualCommoditiesP){
    #   # If none of the utilization is activable based in the utilization Table
    #   if(length(dataPosImbP[measuredItemSuaFbs==i
    #                         &!(get(p$elementVar)%in%c(eleToExclude,p$stockCode))
    #                         &!is.na(rank),Value])==0){
    #     # conventionally put all on food (As was in the previous version of the new module)
    #     # this a very rare case but can happen
    #     # ONLY IF FOOD IS NOT PROTECTED
    #     #   if(dataPosImbP[measuredItemSuaFbs==i
    #     #                  &!(get(p$elementVar)%in%c(eleToExclude,p$stockCode))&
    #     #                  get(p$elementVar)==p$foodCode,ProtectedFood]=="FALSE"){
    #     #          dataPosImbP[measuredItemSuaFbs==i
    #     #              &!(get(p$elementVar)%in%c(eleToExclude,p$stockCode))&
    #     #                get(p$elementVar)==p$foodCode,newValue:=imbalance]
    #     #     } # IF FOOD IS PROTECTED THE LINE WILL REMAIN IMBALANCED
    #   }else{
    #     #   # Se tutti i Value sono popolati
    #     if(length(dataPosImbP[measuredItemSuaFbs==i
    #                           &!(get(p$elementVar)%in%eleToExclude)
    #                           &!is.na(rank)&(is.na(Value)),Value])==0){
    #       # AS NOW WE ARE CONSIDERING PRIMARIES, IF ALL THE VALUES ARE POPULATED
    #       # DON'T DO ANYTHING
    #       dataPosImbP[measuredItemSuaFbs==i
    #                   &!(get(p$elementVar)%in%eleToExclude)
    #                   # &!is.na(rank)
    #                   &Value>0,
    #                   newValue:=Value]
    #     }else{
    #       #se un valore non 'e popolato e non e' stock ED E' FOOD 
    #       if(length(dataPosImbP[measuredItemSuaFbs==i
    #                             &!(get(p$elementVar)%in%c(eleToExclude,p$stockCode))
    #                             &!is.na(rank)&(is.na(Value)|Value==0),Value])==1){
    #         # metti tutto l'imbalance in questo elemento
    #         # ONLY IF IS FOOD 
    #         # what we are tring to do is not to necessary balance primary, but only create food 
    #         # if this should be there and is not
    #         dataPosImbP[measuredItemSuaFbs==i
    #                     &!(get(p$elementVar)%in%c(eleToExclude,p$stockCode))&!is.na(rank)
    #                     &(is.na(Value)|Value==0)
    #                     &get(p$elementVar)==p$foodCode&ProtectedFood=="FALSE",
    #                     newValue:=imbalance]
    #         
    #       }else{
    #         # se c'e piu' di un elemento non popolato e food é fra questi elementi
    #         if(length(dataPosImbP[measuredItemSuaFbs==i
    #                               &!(get(p$elementVar)%in%eleToExclude)
    #                               &!is.na(rank)&(is.na(Value)|Value==0),Value])>1
    #            &(p$foodCode %in% dataPosImbP[measuredItemSuaFbs==i
    #                                          &!(get(p$elementVar)%in%eleToExclude)
    #                                          &!is.na(rank)&(is.na(Value)|Value==0),get(p$elementVar)])
    #         ){
    #           # allora in base alla seguente funzione dei rank e rank inversi:
    #           sumRank = sum(dataPosImbP[measuredItemSuaFbs==i
    #                                     &!(get(p$elementVar)%in%eleToExclude)
    #                                     &!is.na(rank)&(is.na(Value)|Value==0),rankInv])
    #           dataPosImbP[measuredItemSuaFbs==i
    #                       &!(get(p$elementVar)%in%eleToExclude)
    #                       &!is.na(rank)&(is.na(Value)|Value==0),newValue:=imbalance*(rankInv/sumRank)]
    #         }
    #       }
    #     }
    #     
    #   }
    #   
    # }
    # ############ End loop primary
    
    
    
    ###    ###    ###    ###    
    ### EXTERNAL SAVING OF FORCED INFORMATION
    
    if(dim(dataNegImb[ProtectedProd=="TRUE"&abs(imbalance)>(pTolerance*sumUtils)])[1]>0){
      
      NoFillable= dataNegImb[ProtectedProd=="TRUE"&abs(imbalance)>(pTolerance*sumUtils)]
      
      if("newValue" %in% colnames(NoFillable)){
        NoFillable[!is.na(newValue),Value:=newValue]
        NoFillable=NoFillable[,c(3,2,1,4,5,6,7,16),with=FALSE]
      }
      setnames(NoFillable, "measuredItemSuaFbs", "measuredItemFbsSua")
      standData = NoFillable
      standData=standData[,.(geographicAreaM49, measuredElementSuaFbs, measuredItemFbsSua, timePointYears, 
                             Value)]
      standData <- standData[!is.na(Value),]
      
      standData[, flagObservationStatus := "I"]
      standData[, flagMethod := "x"]
      
      if(!is.null(debugFile)){
        
        saveFBSItermediateStep(directory=paste0(basedir,"/debugFile/Batch_",batchnumber),
                               fileName=paste0("B",batchnumber,"_10_ForcedProduction"),
                               data=standData)
      }
      
    }
    
    
    # if("newValue" %in% colnames(dataPosImbP)){
    #   dataPosImbP[!is.na(newValue),Value:=newValue]
    #   # dataPosImbP[,newValue:=NULL]
    #   dataPosImbP=dataPosImbP[,1:17,with=FALSE]
    # }
    if("newValue" %in% colnames(dataNegImb)){
      dataNegImb[!is.na(newValue),Value:=newValue]
      # dataPosImbP[,newValue:=NULL]
      dataNegImb=dataNegImb[,1:18,with=FALSE]
    }
    if("newValue" %in% colnames(dataPosImbAll)){
      dataPosImbAll[!is.na(newValue),Value:=newValue]
      dataPosImbAll=dataPosImbAll[,1:18,with=FALSE]
    }
    
    
    # data=rbind(dataNoImbP,dataNegImbP,dataNoImb,dataPosImbP,dataNegImb,dataPosImb)
    data=rbind(dataNoImbP,dataNegImbP,dataNoImb,dataNegImb,dataPosImbAll)
    
  }  #this brackets refer only at the second loop
  
  data[, c("imbalance","sumUtils","sumSup") := NULL]   
  
}
