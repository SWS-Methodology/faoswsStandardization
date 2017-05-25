##' Balance Residual
##' 
##' This function forces a balance in the passed elements by allocating the 
##' imbalance to one element.
##' 
##' If supply < utilization, the imbalance is always assigned to production (as 
##' trade is generally assumed to be fixed and stock changes are usually 0 and 
##' hence also fixed).
##' 
##' If supply > utilization, we must choose where to allocate the imbalance. The
##' default variable is food, but for some commodities we could instead use 
##' feed, food processing, or industrial.
##' 
##' @param data The data.table containing the full dataset for standardization.
##' @param standParams The parameters for standardization.  These parameters 
##'   provide information about the columns of data and tree, specifying (for 
##'   example) which columns should be standardized, which columns represent 
##'   parents/children, etc.
##' @param primaryCommodities Primary level commodities (such as wheat, oranges,
##'   sweet potatoes, etc.) should not be balanced at this step but rather by
##'   the balancing algorithm.  This argument allows the user to specify a
##'   character vector with these primary element codes.
##' @param feedCommodities Sometimes excess supply will need to be allocated to 
##'   some processed product.  The default is to place it into food, but this 
##'   list specifies which elements should allocate such a difference to feed.
##' @param indCommodities Same as feedCommodities, but for commodities where we 
##'   allocate the difference to industrial utilization.
##' @param foodCommodities if we use the foodCommodities from the Crude Balancing matrix old faostat.
##' @param seedCommodities This list specify which commodities should allocate imbalance to seed.
##' @param stockCommodities This list specify which commodities should allocate imbalance to stock
##' @param lossCommodities This list specify which commodities should allocate imbalance to loss. 
##' @param foodProcessCommodities Same as feedCommodities, but for commodities 
##'   where we allocate the difference to food processing.
##' @param imbalanceThreshold The size that the imbalance must be in order for 
##'   an adjustment to be made.
##' @param cut these are primary equivalent commodities.
##' @param tree this is the sub tree used in the function.
##' @return Nothing is returned, but the Value column of the passed data.table 
##'   is updated.
##'   

balanceResidual = function(data, standParams, feedCommodities = c(), tree=tree,
                           indCommodities = c(), primaryCommodities = c(), stockCommodities = c(),
                           seedCommodities = c(), lossCommodities = c(),foodCommodities =c(),
                           foodProcessCommodities = c(), imbalanceThreshold = 10,cut=c()){
    p = standParams
    
    ## imbalance calculates, for each commodity, the residual of the FBS equation and
    ## assigns this amount to each row for that commodity.
    
    knownCodes <- unlist(p[c("productionCode", "importCode", "exportCode", "stockCode", 
                             "foodCode", "foodProcCode", "feedCode", "wasteCode", 
                             "seedCode", "industrialCode", "touristCode", "residualCode")])
    
    ##!! NOTE: It's unclear here why NAs are showing up here and if they should
    ## be allowable.
    unknownCodes <- setdiff(na.omit(unique(data[, get(standParams$elementVar)])), knownCodes)
    
    if(length(unknownCodes) > 0){
      stop(sprintf("Unknown code(s): %s", paste(unknownCodes, collapse = ", ")))
    }
    
    stopifnot(imbalanceThreshold > 0)
    
   data[, imbalance := sum(ifelse(is.na(Value), 0, Value) *
           ifelse(get(standParams$elementVar) == p$productionCode, 1,
           ifelse(get(standParams$elementVar) == p$importCode, 1,
           ifelse(get(standParams$elementVar) == p$exportCode, -1,
           ifelse(get(standParams$elementVar) == p$stockCode, -1,
           ifelse(get(standParams$elementVar) == p$foodCode, -1,
           ifelse(get(standParams$elementVar) == p$foodProcCode, 0,
           ifelse(get(standParams$elementVar) == p$feedCode, -1,
           ifelse(get(standParams$elementVar) == p$wasteCode, -1,
           ifelse(get(standParams$elementVar) == p$seedCode, -1,
           ifelse(get(standParams$elementVar) == p$industrialCode, -1,
           ifelse(get(standParams$elementVar) == p$touristCode, -1,
           ifelse(get(standParams$elementVar) == p$residualCode, -1, 
                  NA))))))))))))),
        by = c(standParams$mergeKey)]
    
   ##data[, imbalance := sum(ifelse(is.na(Value), 0, Value) *
   ##                           ifelse(get(p$elementVar) == p$productionCode, 1,
   ##                           ifelse(get(p$elementVar) == p$importCode, 1,
   ##                           ifelse(get(p$elementVar) == p$exportCode, -1,
   ##                           ifelse(get(p$elementVar) == p$seedCode, -1,0))))),
   ##      by = c(p$mergeKey)]
    
    ##data[!is.na(foodProc) & imbalance>0, imbalance:=imbalance-foodProc]
    
    
    data[imbalance>0 & !is.na(foodProcElement), imbalance:=imbalance-foodProcElement]
    
    
    data[, newValue := ifelse(is.na(Value), 0, Value) + imbalance]
    # I'm a little confused about why flags aren't used here, but it looks like 
    # items where there's a positive values are marked as official
    data[, officialProd := any(get(standParams$elementVar) == standParams$productionCode &
                                   !is.na(Value) & Value > 0),
         by = c(standParams$itemVar)]

    
    data[, officialFood := any(get(standParams$elementVar) == standParams$foodCode &
                                 !is.na(Value) & Value > 0),
        by = c(standParams$itemVar)]
    
   TobeBalancedCommodity=list()
   primaryCommoditiesChildren=list()
   for(i in seq_len(length(unique(primaryCommodities))) )
   { 
     
     
     primaryCommoditiesChildren[[i]]=(getChildren( commodityTree = tree,
                                                     parentColname =params$parentVar,
                                                     childColname = params$childVar,
                                                     topNodes =primaryCommodities[i] ))
     
     primaryCommoditiesChildren[[i]]=primaryCommoditiesChildren[[i]][primaryCommoditiesChildren[[i]]!=primaryCommodities[i]]
     if(nrow(data[get(standParams$itemVar) %in% primaryCommoditiesChildren[[i]] , ])==0 |
        sum(data[get(standParams$itemVar) %in% primaryCommoditiesChildren[[i]] , Value], na.rm = TRUE)==0 )
        {
       
       TobeBalancedCommodity[[i]]= data.table(primaryCommodities[i])
       
     }
     
     TobeBalancedCommodity[[i]]=data.table(NA)
   }
   
   TobeBalanced= rbindlist(TobeBalancedCommodity)
   TobeBalanced=TobeBalanced[!is.na(TobeBalanced[,V1])]
   
   
   NotTobeBalanced=primaryCommodities[!primaryCommodities %in% TobeBalanced[,V1]]
   
    
    ## Supply > Utilization: assign difference to food, feed, etc.  Or, if 
    ## production is official, force a balance by adjusting food, feed, etc.
    data[(imbalance > imbalanceThreshold & officialProd )
         & (!get(standParams$itemVar) %in% NotTobeBalanced) ,
         ## Remember, data is currently in long format.  This condition is ugly, but the idea is
         ## that Value should be replaced with newValue if the particular row of interest
         ## corresponds to the food variable and if the commodity should have it's residual
         ## allocated to food.  Likewise, if the row corresponds to feed and if the commodity
         ## should have it's residual allocated to feed, then Value is updated with newValue.
         Value := ifelse(
            (get(standParams$elementVar) == p$feedCode & get(p$itemVar) %in% feedCommodities) |
            (get(standParams$elementVar) == p$foodProcCode & get(p$itemVar) %in% foodProcessCommodities) |
            (get(standParams$elementVar) == p$industrialCode & get(p$itemVar) %in% indCommodities) |
            (get(standParams$elementVar) == p$stockCode & get(p$itemVar) %in% stockCommodities) |
            (get(standParams$elementVar) == p$wasteCode & get(p$itemVar) %in% lossCommodities) |
            (get(standParams$elementVar) == p$seedCode & get(p$itemVar) %in% seedCommodities) |
            #food have been defined outside the Balance Residual function (Cristina)
            (get(standParams$elementVar) == p$foodCode & get(p$itemVar) %in% foodCommodities),
            newValue, Value)]
    
  ##  data[(imbalance > imbalanceThreshold & officialProd )
  ##       & (!get(standParams$itemVar) %in% primaryCommodities) ,
  ##       ## Remember, data is currently in long format.  This condition is ugly, but the idea is
  ##       ## that Value should be replaced with newValue if the particular row of interest
  ##       ## corresponds to the food variable and if the commodity should have it's residual
  ##       ## allocated to food.  Likewise, if the row corresponds to feed and if the commodity
  ##       ## should have it's residual allocated to feed, then Value is updated with newValue.
  ##       Value := ifelse(get(standParams$elementVar) == p$foodProcCode,newValue, Value)]
    
    
    
    
    
    ## Supply < Utilization
    data[imbalance < -imbalanceThreshold & !officialProd &
             (!get(standParams$itemVar) %in% primaryCommodities) &
             get(standParams$elementVar) == p$productionCode ,
             Value := -newValue]
    data[, c("imbalance", "newValue") := NULL]
}
