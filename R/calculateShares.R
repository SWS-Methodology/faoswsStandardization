##' calculateShares
##' 
##' 
##' 
##' @param data The data.table containing the full dataset for standardization. 
##'   It should have columns corresponding to year, country, element, commodity,
##'   and value.  The specific names of these columns should be in params.
##' @param tree The commodity tree which details how elements can be processed 
##'   into other elements.  It does not, however, specify which elements get 
##'   aggregated into others.  This data.table should have columns parent, 
##'   child, extraction rate, share, and target (target specifying if an element
##'   is processed forward or not).  Names of these columns should be provided
##'   in params.
##'
##' @param params The parameters for standardization.  These parameters 
##'   provide information about the columns of data and tree, specifying (for 
##'   example) which columns should be standardized, which columns represent 
##'   parents/children, etc.
##' @param availability Avalaibility as previously calculated
##' @param zeroWeight as previously defined

##' @return A data.table containing the final balanced and standardized SUA 
##'   data.  Additionally, this table will have new elements in it if 
##'   nutrientData was provided.

##' 
##' @export
##' 


calculateShares=function(data=data, params=p, tree=tree,zeroWeight= zeroWeight)

  
{

  # tree = merge(tree, availability,
  #              by = c(params$childVar, params$parentVar))
  tree = tree[, list(share = sum(share),
                     availability = max(availability)),
              by = c(params$childVar, params$parentVar, params$extractVar, 
                     params$targetVar, params$standParentVar)]
  setnames(tree, "share", params$shareVar)
  ## Calculate the share using proportions of availability, but default to the
  ## old value if no "by-availability" shares are available.
  tree[, newShare := availability / sum(availability, na.rm = TRUE),
       by = c(params$childVar)]
  tree[, c(params$shareVar) :=
         ifelse(is.na(newShare), get(params$shareVar), newShare)]
  tree[, newShare := NULL]
  
  
  # weight
  
  tree[,weight:=1]
  tree[measuredItemChildCPC %in% zeroWeight, weight:=0]
  
  
  freqChild= data.table(table(tree[, get(params$childVar)]))
  setnames(freqChild, c("V1","N"), c(params$childVar, "freq"))
  tree=merge(tree, freqChild , by=params$childVar)
  tree[availability<=0|is.na(availability), negShare:=1/freq]
  # tree[availability<=0, availability:=0]
  tree[,sumPositiveAvail:=sum(availability*ifelse(availability>0,1,0),na.rm=TRUE),by = c(params$childVar)]
  tree[,tempAvailability:=ifelse(availability<=0|is.na(availability),negShare*sumPositiveAvail,availability)]
  
  tree[, newShare := ifelse(tempAvailability==0,negShare, tempAvailability / sum(tempAvailability, na.rm = TRUE)),
       by = c(params$childVar)]
  
  tree[,availability:=tempAvailability]
  
  tree[,c("freq","tempAvailability","sumPositiveAvail","negShare"):=NULL]
  tree[, c(params$shareVar) :=
         ifelse(is.na(newShare), get(params$shareVar), newShare)]
  tree[, newShare := NULL]
  
  
  return(tree)
}
