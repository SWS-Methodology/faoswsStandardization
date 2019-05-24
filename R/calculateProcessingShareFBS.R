##' Function to compute Processing sharing: share that identify the quantity of primary availability 
##' that it is allocated in different productive processes.
##'
##' Duplication used in Used in faoswsStandardization 
##'
##' @param data data table containing all the columns to compute processingSharing
##' @param params defaultProcessedItamParams parameters, object which contains the parameters  
##' @param printSharesGraterThan1 logic parameter
##' 
##' @export
##'

# Modified by NW to include shares greater than one
calculateProcessingShareFBS_NW=function(data, printSharesGraterThan1=TRUE, param){
                                  
    
##Check that data contains all the necessary columns    
stopifnot(c(param$geoVar, param$yearVar, param$childVar, param$parentVar,
            param$extractVar, param$shareDownUp ,params$value, param$availVar) %in% colnames(data))

##data[, processingShare:=(((get(param$value)/get(param$extractVar))*get(param$shareDownUp))/get( param$availVar ))]
data[,param$processingShare:= (( get(params$value)/get (param$extractVar) )* get(param$shareDownUp) )/get((param$availVar))] 

# intervene here ***

## This merge eventually used the processing tree (the tree extracte dby Cristina from)
##data=merge(data,processingTree, by=c("geographicAreaM49", "measuredItemChildCPC", "timePointYears", "measuredItemParentCPC", "extractionRate"))

data[processingShare>1.1,PG1:=1 ]
data[processingShare==Inf,PG1:=1 ]
data[is.na(PG1),PG1:=0 ]

##data[flagObservationStatus=="E" & flagMethod=="h" & get(param$processingShare)>1.02, processingShare:=1]
##if processing share are Inf, it means that the availability is 0, so the share must be zero as well.
data[processingShare==Inf,processingShare:=0 ]


## Force the processing share not exceeding 1 

# data[processingShare>1,processingShare:=1 ]

##Attempt to use shares coming from the old system, with shares here we mean the share down up (processing shares)
##data[!is.na(pShare), processingShare:=pShare]

##-------------------------------------------------------------------------------------------------------
##Here we should perform an imputation of the imputation on 
processingShareParamenters=defaultImputationParameters()
processingShareParamenters$imputationValueColumn="processingShare"
processingShareParamenters$imputationFlagColumn="processingShareFlagObservationStatus"
processingShareParamenters$imputationMethodColumn="processingShareFlagMethod"
#processingShareParamenters$byKey=c("geographicAreaM49", "measuredItemChildCPC", "measuredItemParentCPC")
processingShareParamenters$byKey=c("geographicAreaM49", "measuredItemParentCPC", "measuredItemChildCPC")
processingShareParamenters$estimateNoData=FALSE


processingShareParamenters$ensembleModels$defaultExp=NULL
##processingShareParamenters$ensembleModels$defaultLogistic=NULL
processingShareParamenters$ensembleModels$defaultLoess=NULL
processingShareParamenters$ensembleModels$defaultSpline=NULL
processingShareParamenters$ensembleModels$defaultMars=NULL
processingShareParamenters$ensembleModels$defaultMixedModel=NULL
##processingShareParamenters$ensembleModels$defaultMovingAverage=NULL
##processingShareParamenters$ensembleModels$defaultArima=NULL

##dataInpute=copy(data)
data[,processingShareFlagObservationStatus:="M"]
data[,processingShareFlagMethod:="u"]

## I am flagging the just computed flags with a protected flag combination
## in ordert to use them as training set to produce imputations
data[!is.na(processingShare),processingShareFlagObservationStatus:="T"]
data[!is.na(processingShare),processingShareFlagMethod:="-"]
data[processingShare=="NaN", processingShare:=NA_real_]

##Remove series with no data
counts = data[, sum(!is.na(processingShare)),
               by = c(processingShareParamenters$byKey)]
counts=counts[V1!=0]
counts=counts[,c(param$geoVar, param$childVar, param$parentVar), with=FALSE]
data=data[counts, ,on=c(param$geoVar, param$childVar, param$parentVar)]

## impute processingSharing
# NOTE: on 20190523 it was decided to replace the ensemble imputation with a
# 3-year moving average (actually it was decided before, never implemented).
# This may be temporaty or not, thus the structure of this function still
# keeps the ensemble framework, though the actual imputation is replaced
# (carried out by imputeVariable() is commented. New code goes from
# "STARTSMOVAV" to until "ENDMOVAV", so that if can be changed back easily
# to ensemble if required.
#
#data=arrange(data,geographicAreaM49,measuredItemParentCPC,measuredItemChildCPC,timePointYears)
#data=imputeVariable(as.data.table(data),processingShareParamenters)
#
# STARTSMOVAV
my_movav <- function(x, order = 3) {
  # order should be > 2
  stopifnot(order >= 3)

  non_missing <- sum(!is.na(x))

  # For cases that have just two non-missing observations
  order <- ifelse(order > 2 & non_missing == 2, 2, order)

  if (non_missing == 1) {
    x[is.na(x)] <- na.omit(x)[1]
  } else if (non_missing >= order) {
    n <- 1
    while(any(is.na(x)) & n <= 10) {
      movav <- suppressWarnings(RcppRoll::roll_mean(x, order, fill = 'extend', align = 'right'))
      movav <- data.table::shift(movav)
      x[is.na(x)] <- movav[is.na(x)]
      n <- n + 1
    }

    x <- zoo::na.fill(x, 'extend')
  }

  return(x)
}

data <-
  data.table(data)[
    order(geographicAreaM49, measuredItemParentCPC, measuredItemChildCPC, timePointYears)
  ][,
    processingShare_avg := my_movav(processingShare, order = 3),
    .(geographicAreaM49, measuredItemParentCPC, measuredItemChildCPC)
  ]

data[is.na(processingShare), `:=`(processingShare = processingShare_avg, processingShareFlagObservationStatus = "I", processingShareFlagMethod = "e")]

data[, processingShare_avg := NULL]
# ENDMOVAV

## CARLO MODIFICATION check if sum of processing share by productive process is less than 1
# to do this i need the zero weight
zeros <- as.character(ReadDatatable('zero_weight')[,item_code]) 
data=data[,weight:=ifelse(measuredItemChildCPC %in% zeros,0,1)]


data=data[,processingShareByProcess:=sum(processingShare*weight), by=c("geographicAreaM49","timePointYears","measuredItemParentCPC")]
data=data[,excProcShare:=ifelse(processingShareByProcess>1,processingShareByProcess-1,0)]
data=data[,allocExcShare:=processingShare/sum(processingShare), by=c("geographicAreaM49","timePointYears","measuredItemParentCPC")]
data=data[,processingShare:=processingShare-excProcShare*allocExcShare]
data=data[,processingShareByProcess_new:=sum(processingShare*weight), by=c("geographicAreaM49","timePointYears","measuredItemParentCPC")]





## Force the processing share not exceeding 1 
data[processingShare>1,processingShare:=1 ]
##-------------------------------------------------------------------------------------------------------


return(data)

}

