##' Process Forward
##' 
##' A few edges in the commodity trees are labeled with an 'F' to indicate that 
##' processing is 'forward'.  The parent commodities in these edges are 
##' immediately converted into the corresponding child and then they are removed
##' from the tree (as we will standardize to the children instead).  This is a 
##' rare scenario; an example commodity is sugar.
##' 
##' Note: when commodities are processed forward like this, the final flag is
##' assigned ARBITRARILY as the first flag observed (for lack of a better
##' approach).  This should be corrected.
##' 
##' @param data The data.table containing the full dataset for standardization.
##' @param tree The commodity tree which provides the edge structure.
##' @param standParams The parameters for standardization.  These parameters 
##'   provide information about the columns of data and tree, specifying (for 
##'   example) which columns should be standardized, which columns represent 
##'   parents/children, etc.
##'   
##' @return A list of names 'data' and 'tree'.  Both objects must be returned, 
##'   as the tree is updated by pruning off some edges.
##'   

processForward = function(data, tree, standParams){
  
  ## If no forward processing edge, than don't do anything:
  if(all(tree[, get(standParams$targetVar) != "F"])){
    return(list(data = data, tree = tree))
  }
  
  cnames = colnames(data)
  
  subTree = tree[get(standParams$targetVar) == "F", ]
  level = getCommodityLevel(subTree, parentColname = standParams$parentVar,
                            childColname = standParams$childVar)
  setnames(level, c(standParams$parentVar, "level"))
  if(length(unique(tree[get(standParams$parentVar) %in%
                        subTree[, get(standParams$parentVar)], target])) > 1){
    warning("Some parents have one edge set to be forward processed and ",
            "another edge not.  How to handle such a case is not clear, ",
            "and this may cause strange behavior.")
  }
  subTree = merge(subTree, level, by = standParams$parentVar)
  setnames(subTree, standParams$parentVar, standParams$itemVar)
  
  for(currentLevel in subTree[, sort(unique(level))]){
    dataToUpdate = merge(data, subTree[level == currentLevel, ],
                         by = standParams$itemVar, allow.cartesian = TRUE)
    ## Process the node down by first computing the availability of the
    ## parent as the balance
    dataToUpdate = dataToUpdate[, list(parentAvail = sum(Value *
                                                           ifelse(get(standParams$elementVar) %in%
                                                                    c(standParams$exportCode, standParams$stockCode,
                                                                      standParams$foodCode, standParams$foodProcCode,
                                                                      standParams$feedCode, standParams$wasteCode,
                                                                      standParams$seedCode, standParams$industrialCode,
                                                                      standParams$touristCode, standParams$residualCode), -1,
                                                                  ifelse(get(standParams$elementVar) %in% c(standParams$importCode,
                                                                                                            standParams$productionCode), 1, 0)),
                                                         na.rm = TRUE),
                                       parentAvailSd = sqrt(sum(standardDeviation^2 *
                                                                  ifelse(get(standParams$elementVar) %in% c(standParams$exportCode, standParams$stockCode,
                                                                                                            standParams$foodCode, standParams$foodProcCode,
                                                                                                            standParams$feedCode, standParams$wasteCode,
                                                                                                            standParams$seedCode, standParams$industrialCode,
                                                                                                            standParams$touristCode, standParams$residualCode,
                                                                                                            standParams$importCode, standParams$productionCode), 1, 0),
                                                                na.rm = TRUE))),
                                by = c(standParams$mergeKey, standParams$childVar,
                                       standParams$extractVar)]
    dataToUpdate[, c(standParams$itemVar) := get(standParams$childVar)]
    dataToUpdate[, Value := get(standParams$extractVar) * parentAvail]
    dataToUpdate[, standardDeviation := get(standParams$extractVar) * parentAvailSd]
    dataToUpdate[, c(standParams$elementVar) := standParams$productionCode]
    dataToUpdate = dataToUpdate[, c(standParams$mergeKey, standParams$elementVar,
                                    "Value", "standardDeviation"), with = FALSE]
    ## Aggregate dataToUpdate in case there are multiple parents going into
    ## one child.
    dataToUpdate = dataToUpdate[, list(Value = sum(Value),
                                       standardDeviation = sqrt(sum(standardDeviation^2))),
                                by = c(standParams$mergeKey, standParams$elementVar)]
    
    ## Add in the new data values
    data = merge(data, dataToUpdate, by = c(standParams$mergeKey,
                                            standParams$elementVar),
                 all = TRUE, suffixes = c("", ".new"))
    data[is.na(Value), c("Value", "standardDeviation") :=
           list(Value.new, standardDeviation.new)]
    data[, c("Value.new", "standardDeviation.new") := NULL]
    
    ## Remove the values processed forward from the original data
    data = data[!get(standParams$itemVar) %in%
                  subTree[level == currentLevel, get(standParams$itemVar)]]
  }
  tree = tree[!get(standParams$targetVar) == "F", ]
  
  #     ## This function may create some new commodities, and only the production of
  #     ## these items will be in the data.frame.  To prevent future issues, fill in
  #     ## all other elements as well.
  
  return(list(data = data, tree = tree))
}