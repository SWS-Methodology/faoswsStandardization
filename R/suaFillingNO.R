##' This function replaces the old BalanceResidual
##' 
##' This function forces a the filling of empty elements in the pulled SUA
##' by allocating the "imbalance" according to a Ranking of the possible 
##' Uitlizazions for each combination of country/commodity
##' 
##' - If supply < utilization
##'   * the imbalance is assigned to production, if it is not official
##'   * if is official it is checked if the commodity is a stock commodity
##'     i.e. if it does make sense to allocate stock. If it is so, stock is created (negative)
##'   * if production is official and is not a stocj comodity, 
##'     Utilizations are proportionally reduced,if the difference S-U <=10% Utilizations
##'   * if none of these operations is possible, a manual check of the data is needed
##'     (for the moment production is increased anyway and an external file is produced for check)
##' - if supply > utilization a Multiple filler approach is used: 
##'   * If all the ranked utilizations are present 
##'     these are proportionally incremented
##'   * if one is empty, imbalance goes to this commodity
##'   * if more than one is empty, Utilization are created based on a ranking of possible Utilization 
##'     coming from an external source.
##'     The approach used is of the Inverse Ranking. 
##'     (CRISTINA: for the moment we are trying with the Approach2: using old SUA)
##' 
##' Trade and Food Processing is never created in this function:
##' - trade is supposed to be "protected", 
##' - food processing is created outside this function 
##'   using a specific function and is not replaced here
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

suaFillingNO = function(data, p = p, tree=tree,
                      primaryCommodities = c(), stockCommodities = c(),
                      debugFile= NULL,
                      utilizationTable=c(), 
                      imbalanceThreshold = 10,loop1=TRUE){

    # The commodities that have to be crude balanced are the NON PRIMARY
    
    stopifnot(imbalanceThreshold > 0)

    eleToExclude = c(p$productionCode,p$exportCode,p$importCode,p$stockCode)
 
  
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
                         ifelse(get(p$elementVar) == p$foodProcCode, 1,
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
  data[is.na(officialProd),officialProd:="FALSE"]
  data[is.na(ProtectedFood),ProtectedFood:="FALSE"]
  
  # I serparate the different blocks of data for trating them separately 
  
  # if(loop1==FALSE){
  #   primaryCommodities = c()
  # }
  #   dataPrimary = data[(get(p$itemVar) %in% primaryCommodities)]
  #   dataNoPrimary = data[!(get(p$itemVar) %in% primaryCommodities)]
  #   dataNoImb = dataNoPrimary[imbalance<=imbalanceThreshold&imbalance>=(-imbalanceThreshold)]
  #   dataNegImb = dataNoPrimary[imbalance < (-imbalanceThreshold)]
  #   dataPosImb = dataNoPrimary[imbalance > imbalanceThreshold]

    dataPrimary = data[(get(p$itemVar) %in% primaryCommodities)]
    dataNoImbP = dataPrimary[imbalance<=imbalanceThreshold&imbalance>=(-imbalanceThreshold)] # never touched
    dataNegImbP = dataPrimary[imbalance < (-imbalanceThreshold)] # never touched
    dataPosImbP = dataPrimary[imbalance > imbalanceThreshold]
    
    dataNoPrimary = data[!(get(p$itemVar) %in% primaryCommodities)]
    dataNoImb = dataNoPrimary[imbalance<=imbalanceThreshold&imbalance>=(-imbalanceThreshold)] # never touched
    dataNegImb = dataNoPrimary[imbalance < (-imbalanceThreshold)]
    dataPosImb = dataNoPrimary[imbalance > imbalanceThreshold]
    
    

  ## Supply < utilization (= imbalance < -imbalanceThreshold)
  
  # if production is not official, create production
  dataNegImb[officialProd=="FALSE" & get(p$elementVar)==p$productionCode,
             newValue:=ifelse(is.na(Value),-imbalance,Value-imbalance)]


  ########################################################  
  # if production is official 
  pTolerance = 0.3
  
  dataNegImbOffP = dataNegImb[officialProd=="TRUE"]
  
  # 1. Try to reduce the present Utilizations
  #    only if at least the 70% of each utilization remains
  #    stock here are incremented or reduced of an amount proportional 
  #    to their value in respect to other utilization

  dataNegImb[officialProd=="TRUE"&abs(imbalance)<=(pTolerance*sumUtils),
             newValue:= ifelse(is.na(Value),NA,
             ifelse(get(p$elementVar)%in%eleToExclude,NA,
             Value-abs(Value)*(abs(imbalance)/(sumUtils+(sumSupstock-sumSup)))))]
  
  # 2. If Imbalance is too high 
  # save this data outside 
  
  NoFillable=dataNegImbOffP[(abs(imbalance)>(pTolerance*sumUtils))]

  # & force the reduction of utilization (up to 30% of their value) & increase production
  dataNegImb[officialProd=="TRUE"&abs(imbalance)>(pTolerance*sumUtils)&sumUtils>0,
             newValue:= ifelse(get(p$elementVar)%in%eleToExclude[-which(eleToExclude=="production")],NA,
                                ifelse(get(p$elementVar)==p$productionCode,(ifelse(is.na(Value),0,Value)+abs(imbalance)-pTolerance*sumUtils),
                                       ifelse(is.na(Value),NA,Value-(abs(Value)/(sumUtils+(sumSupstock-sumSup)))
                                              *(sumUtils*pTolerance))))]
 # & force production
  
  dataNegImb[officialProd=="TRUE"&abs(imbalance)>(pTolerance*sumUtils)&sumUtils==0,
            newValue:=ifelse(get(p$elementVar)==p$productionCode,
                                ifelse(is.na(Value),0,Value)+abs(imbalance),NA)]
              
              
  ########################################################  
  
 if(loop1==FALSE) {
  ## Supply > utilization (= imbalance > imbalanceThreshold)

############
### Loop fot Non Primary
  actualCommodities = dataPosImb[,unique(measuredItemSuaFbs)]
  
  for (i in actualCommodities){
    # If none of the utilization is activable based in the utilization Table
    if(length(dataPosImb[measuredItemSuaFbs==i
                         &!(get(p$elementVar)%in%c(eleToExclude,p$stockCode))
                         &!is.na(rank),Value])==0){
    # conventionally put all on food (As was in the previous version of the new module)
    # this a very rare case but can happen
      dataPosImb[measuredItemSuaFbs==i
                 &!(get(p$elementVar)%in%c(eleToExclude,p$stockCode))&
                 get(p$elementVar)==p$foodCode,newValue:=imbalance]
    }else{
      # Se tutti i Value sono popolati
    if(length(dataPosImb[measuredItemSuaFbs==i
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
      dataPosImb[measuredItemSuaFbs==i
                 &!(get(p$elementVar)%in%eleToExclude)
                 # &!is.na(rank)
                 &Value>0,
                 newValue:=ifelse(is.na(Value),NA,
                           Value+abs(Value)*(abs(imbalance)/(sumUtils+(sumSupstock-sumSup))))]
    }else{
      #se un valore non 'e popolato e non 'e stock
      if(length(dataPosImb[measuredItemSuaFbs==i
                           &!(get(p$elementVar)%in%c(eleToExclude,p$stockCode))
                           &!is.na(rank)&(is.na(Value)|Value==0),Value])==1){
        # metti tutto l' imbalance in questo elemento
        
        dataPosImb[measuredItemSuaFbs==i
                   &!(get(p$elementVar)%in%c(eleToExclude,p$stockCode))&!is.na(rank)
                   &(is.na(Value)|Value==0),
                   newValue:=imbalance]
        
      }else{
        # se c'e piu' di un elemento non popolato
        if(length(dataPosImb[measuredItemSuaFbs==i
                             &!(get(p$elementVar)%in%eleToExclude)
                             &!is.na(rank)&(is.na(Value)|Value==0),Value])>1){
          # allora in base alla seguente funzione dei rank e rank inversi:
          sumRank = sum(dataPosImb[measuredItemSuaFbs==i
                                   &!(get(p$elementVar)%in%eleToExclude)
                                   &!is.na(rank)&(is.na(Value)|Value==0),rankInv])
          dataPosImb[measuredItemSuaFbs==i
                     &!(get(p$elementVar)%in%eleToExclude)
                     &!is.na(rank)&(is.na(Value)|Value==0),newValue:=imbalance*(rankInv/sumRank)]
        }
      }
    }
    
  }
  
}
############ End loop no primary
############
### Loop for primary
  # the loop is reduced to the commodities for which Food is NOT PROTECTED
  actualCommoditiesP = dataPosImbP[measuredElementSuaFbs=="food"&ProtectedFood=="FALSE",unique(measuredItemSuaFbs)]
  # actualCommoditiesP = dataPosImbP[,unique(measuredItemSuaFbs)]
  
  for (i in actualCommoditiesP){
    # If none of the utilization is activable based in the utilization Table
    if(length(dataPosImbP[measuredItemSuaFbs==i
                         &!(get(p$elementVar)%in%c(eleToExclude,p$stockCode))
                         &!is.na(rank),Value])==0){
      # conventionally put all on food (As was in the previous version of the new module)
      # this a very rare case but can happen
      # ONLY IF FOOD IS NOT PROTECTED
    #   if(dataPosImbP[measuredItemSuaFbs==i
    #                  &!(get(p$elementVar)%in%c(eleToExclude,p$stockCode))&
    #                  get(p$elementVar)==p$foodCode,ProtectedFood]=="FALSE"){
    #          dataPosImbP[measuredItemSuaFbs==i
    #              &!(get(p$elementVar)%in%c(eleToExclude,p$stockCode))&
    #                get(p$elementVar)==p$foodCode,newValue:=imbalance]
    #     } # IF FOOD IS PROTECTED THE LINE WILL REMAIN IMBALANCED
    }else{
    #   # Se tutti i Value sono popolati
      if(length(dataPosImbP[measuredItemSuaFbs==i
                           &!(get(p$elementVar)%in%eleToExclude)
                           &!is.na(rank)&(is.na(Value)),Value])==0){
        # AS NOW WE ARE CONSIDERING PRIMARIES, IF ALL THE VALUES ARE POPULATED
        # ONLY CHANGE THE NON PROTECTED VALUES
        #SO
        # IF VALUES ARE ALL PROTECTED COPY THEM
        
        if(length(dataPosImbP[measuredItemSuaFbs==i
                              &!(get(p$elementVar)%in%eleToExclude)
                              &!is.na(rank)&(!is.na(Value))&Protected=="FALSE",Value])==0){
        dataPosImbP[measuredItemSuaFbs==i
                   &!(get(p$elementVar)%in%eleToExclude)
                   # &!is.na(rank)
                   &Value>0,
                   newValue:=Value]
        }else{
          # if there is some non protected value, increment them proportionally to ranks
          sumRank = sum(dataPosImbP[measuredItemSuaFbs==i
                                    &!(get(p$elementVar)%in%eleToExclude)
                                    &!is.na(rank)&(!is.na(Value))&Protected=="FALSE",rankInv])
          dataPosImbP[measuredItemSuaFbs==i
                                  &!(get(p$elementVar)%in%eleToExclude)
                                  &!is.na(rank)&(!is.na(Value))&Protected=="FALSE",newValue:=Value+imbalance*(rankInv/sumRank)]
        }
#here ends the case in which all value are populated
      }else{
        #se un valore non 'e popolato e non e' stock ED E' FOOD 
        if(length(dataPosImbP[measuredItemSuaFbs==i
                             &!(get(p$elementVar)%in%c(eleToExclude,p$stockCode))
                             &!is.na(rank)&(is.na(Value)|Value==0),Value])==1){
          # metti tutto l'imbalance in questo elemento
          # ONLY IF IS FOOD 
          # what we are tring to do is not to necessary balance primary, but only create food 
          # if this should be there and is not
          dataPosImbP[measuredItemSuaFbs==i
                     &!(get(p$elementVar)%in%c(eleToExclude,p$stockCode))&!is.na(rank)
                     &(is.na(Value)|Value==0)
                     &get(p$elementVar)==p$foodCode&ProtectedFood=="FALSE",
                     newValue:=imbalance]
          
        }else{
          # se c'e piu' di un elemento non popolato e food é fra questi elementi
          if(length(dataPosImbP[measuredItemSuaFbs==i
                               &!(get(p$elementVar)%in%eleToExclude)
                               &!is.na(rank)&(is.na(Value)|Value==0),Value])>1
             &(p$foodCode %in% dataPosImbP[measuredItemSuaFbs==i
                          &!(get(p$elementVar)%in%eleToExclude)
                          &!is.na(rank)&(is.na(Value)|Value==0),get(p$elementVar)])
             ){
            # allora in base alla seguente funzione dei rank e rank inversi:
            sumRank = sum(dataPosImbP[measuredItemSuaFbs==i
                                     &!(get(p$elementVar)%in%eleToExclude)
                                     &!is.na(rank)&(is.na(Value)|Value==0),rankInv])
            dataPosImbP[measuredItemSuaFbs==i
                       &!(get(p$elementVar)%in%eleToExclude)
                       &!is.na(rank)&(is.na(Value)|Value==0),newValue:=imbalance*(rankInv/sumRank)]
          }
        }
      }
      
    }
  }   

  ############ End loop primary
  
}  #this brackets refer only at the second loop
  
  ###    ###    ###    ###    
  ### EXTERNAL SAVING OF FORCED INFORMATION

  if(dim(NoFillable)[1]>0){
    # message(paste0("There are ",dim(NoFillable)[1]," row with supply<Utilization where Official Production has been incremented"))
  
    NoFillable[,noF:=NULL]
    setnames(NoFillable, "measuredItemSuaFbs", "measuredItemFbsSua")
    # fbs_sua_conversion <- data.table(measuredElementSuaFbs=c("Calories", "Fats", "Proteins", "exports", "feed", "food", 
    #                                                          "foodManufacturing", "imports", "loss", "production", 
    #                                                          "seed", "stockChange", "residual","industrial", "tourist"),
    #                                  code=c("261", "281", "271", "5910", "5520", "5141", 
    #                                         "5023", "5610", "5016", "5510",
    #                                         "5525", "5071", "5166","5165", "5164"))
    # 
    # standData = merge(NoFillable, fbs_sua_conversion, by = "measuredElementSuaFbs")
    # standData[,`:=`(measuredElementSuaFbs = NULL)]
    # setnames(standData, "code", "measuredElementSuaFbs")
    standData = NoFillable
    standData=standData[,.(geographicAreaM49, measuredElementSuaFbs, measuredItemFbsSua, timePointYears, 
                           Value)]
    standData <- standData[!is.na(Value),]
    
    standData[, flagObservationStatus := "I"]
    standData[, flagMethod := "x"]
    
    if(!is.null(debugFile)){
      
      saveFBSItermediateStep(directory=paste0("debugFile/Batch_",batchnumber),
                             fileName=paste0("B",batchnumber,"_10_ForcedProduction"),
                             data=standData)
    }

  }

  
  if("newValue" %in% colnames(dataPosImbP)){
    dataPosImbP[!is.na(newValue),Value:=newValue]
    dataPosImbP=dataPosImbP[,1:17,with=FALSE]
  }
  if("newValue" %in% colnames(dataNegImb)){
    dataNegImb[!is.na(newValue),Value:=newValue]
    dataNegImb=dataNegImb[,1:17,with=FALSE]
  }
  if("newValue" %in% colnames(dataPosImb)){
    dataPosImb[!is.na(newValue),Value:=newValue]
    dataPosImb=dataPosImb[,1:17,with=FALSE]
  }
  data=rbind(dataNoImbP,dataNegImbP,dataNoImb,dataPosImbP,dataNegImb,dataPosImb)
  
  data[, c("imbalance","sumUtils","sumSup") := NULL]   
  
}



   
