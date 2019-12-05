##' @title Validate ExtractionRates of Commodity Trees
##'
##' @description  This function perform Flag Validation for the commodity Tree
##' Validation. 
##' The possible flags are (decided with Data Team): 
##'
##' Extraction Rates:
##' (T,-) = validated up do 2013 - protected
##' (E,t) = copied from 2014 onwards - not protected but that do not change during standardization process
##' (E,f) = manually changed by the user - protected
##'
##' Shares: 
##' (E,-) = coming from old methodology - NOT protected. These values willbe overwritten
##' at any run of the module, except the values of oils, which are kept, unless manually changed
##' (E,f) = manually changed by the user - protected
##' (I,i) = calculated by the module - not protected
##' 
##' @param tree the commodity tree to check
##'   
##' @param min.er the lower limit of the range of acceptable values for Extraction Rates.
##' default to 0
##' 
##' @param max.er the upper limit of the range of acceptable values for Extraction Rates.
##' default to 7
##'   
##' @return a list of two elements: 
##' messageER, is a message with the information of validity of flags and 
##' invalidER is a data.table which contains invalid Flags or invalid Values or is empty, if everything is valid
##' 
##' @export
##' 

validateTreeExtractionRates = function(tree = NULL, min.er = 0, max.er = 10){
    ## Data Quality Checks
    if(!exists("swsContext.datasets")){
        stop("No swsContext.datasets object defined.  Thus, you probably ",
             "won't be able to read from the SWS and so this function won't ",
             "work.")
    }
    # Checks for data
    stopifnot(c("geographicAreaM49", "measuredElementSuaFbs", "measuredItemParentCPC", 
                "measuredItemChildCPC", "timePointYears", "Value", "flagObservationStatus", 
                "flagMethod") %in% colnames(tree))
    
    if("5423"%in%tree[,measuredElementSuaFbs]){
        stop("Elements have to be expressed in names: extractionRate, share")
    }
    # 5423 = Extraction Rate [hg/t]
    # 5431 = Share of utilization [%]
    
    tree=tree[,mget(c("geographicAreaM49", "measuredElementSuaFbs", "measuredItemParentCPC", 
                      "measuredItemChildCPC", "timePointYears", "Value", "flagObservationStatus", 
                      "flagMethod"))]
    
    ##create column for check
    ##(this column will be deleted)
    tree[,checkFlags:=paste0("(",flagObservationStatus,",",flagMethod,")")]
    
    ##############################################################
    #################### CHECK FLAG VALIDITY  ####################
    ##############################################################
    
    validERflags=c("(T,-)","(E,t)","(E,f)")
    
    # if There is some invalid Flag Combination OR any value outside Ranges
    if(any(!(tree[measuredElementSuaFbs=="extractionRate",unique(checkFlags)]%in%validERflags))|
       any(tree[measuredElementSuaFbs=="extractionRate",unique(Value)]<min.er)|
       any(tree[measuredElementSuaFbs=="extractionRate",unique(Value)]>max.er)){
        # check if Flags are invalid
        if(any(!(tree[measuredElementSuaFbs=="extractionRate",unique(checkFlags)]%in%validERflags))){
            # if ALSO Values are invalid
            if(any(tree[measuredElementSuaFbs=="extractionRate",unique(Value)]<min.er)|
               any(tree[measuredElementSuaFbs=="extractionRate",unique(Value)]>max.er)){
                #  select invalid Flags rows
                invalidERf=tree[measuredElementSuaFbs=="extractionRate"&(!checkFlags%in%validERflags)]
                # select also invalid values     
                invalidERv=tree[measuredElementSuaFbs=="extractionRate"&(Value>max.er|Value<min.er)]
                invalidER=rbind(invalidERf,invalidERv)
                invalidER[,checkFlags:=NULL]
                messageER = "Invalid Flags and Figures"
            }else{
                # if ONLY Flags are invalid
                invalidER=tree[measuredElementSuaFbs=="extractionRate"&(!checkFlags%in%validERflags)]
                # then the file will contain only Flags
                invalidER[,checkFlags:=NULL]
                messageER = "Invalid Flags and Figures"
            }
        }else{
            # if ONLY VALUES are INVALID
            if(any(tree[measuredElementSuaFbs=="extractionRate",unique(Value)]<min.er)|
               any(tree[measuredElementSuaFbs=="extractionRate",unique(Value)]>max.er)){
                invalidER=tree[measuredElementSuaFbs=="extractionRate"&(Value>max.er|Value<min.er)]
                invalidER[,checkFlags:=NULL]
                messageER = "Invalid Flags and Figures"
            }
        }
    }else{
        # the following Tree in empy
        invalidER = tree[measuredElementSuaFbs=="extractionRate"&(Value>max.er|Value<min.er)]
        messageER = "Extraction Rates Valid for the selected Tree"
    }
    
    return(list(messageER=messageER,invalidER=invalidER))
    
}

