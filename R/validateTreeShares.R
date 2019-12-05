##' @title Validate shares of Commodity Trees
##' 
##' This function perform Flag Validation for the commodity Tree
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
##' @return a list of two elements: 
##' messageSH, is a message with the information of validity of flags and 
##' invalidSH is a data.table which contains invalid Flags or invalid Values or is empty, if everything is valid
##' 
##' @export
##' 

validateTreeShares = function(tree = NULL){
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
    
    validSHflags=c("(E,-)","(E,f)","(I,i)")
    
    # if There is some invalid Flag Combination OR any value outside Ranges
    if(any(!(tree[measuredElementSuaFbs=="share",unique(checkFlags)]%in%validSHflags))|
       any(tree[measuredElementSuaFbs=="share",unique(Value)]<0)|
       any(tree[measuredElementSuaFbs=="share",unique(Value)]>1)){
        # check if Flags are invalid
        if(any(!(tree[measuredElementSuaFbs=="share",unique(checkFlags)]%in%validSHflags))){
            # if ALSO Values are invalid
            if(any(tree[measuredElementSuaFbs=="share",unique(Value)]<0)|
               any(tree[measuredElementSuaFbs=="share",unique(Value)]>1)){
                #  select invalid Flags rows
                invalidSHf=tree[measuredElementSuaFbs=="share"&(!checkFlags%in%validSHflags)]
                # select also invalid values     
                invalidSHv=tree[measuredElementSuaFbs=="share"&(Value>1|Value<0)]
                invalidSH=rbind(invalidSHf,invalidSHv)
                invalidSH[,checkFlags:=NULL]
                messageSH = "Invalid Flags and Figures"
            }else{
                # if ONLY Flags are invalid
                invalidSH=tree[measuredElementSuaFbs=="share"&(!checkFlags%in%validSHflags)]
                # then the file will contain only Flags
                invalidSH[,checkFlags:=NULL]
                messageSH = "Invalid Flags and Figures"
            }
        }else{
            # if ONLY VALUES are INVALID
            if(any(tree[measuredElementSuaFbs=="share",unique(Value)]<0)|
               any(tree[measuredElementSuaFbs=="share",unique(Value)]>1)){
                invalidSH=tree[measuredElementSuaFbs=="share"&(Value>1|Value<0)]
                invalidSH[,checkFlags:=NULL]
                messageSH = "Invalid Flags and Figures"
            }
        }
    }else{
        # the following Tree in empy
        invalidSH = tree[measuredElementSuaFbs=="share"&(Value>1|Value<0)]
        messageSH = "Shares Valid for the selected Tree"
    }
    
    return(list(messageSH=messageSH,invalidSH=invalidSH))
    
}

