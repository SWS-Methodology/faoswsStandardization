##' @title Validate ExtractionRates of Commodity Trees
##' 
##' @description This function perform Flag Validation for the commodity Tree
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
##' @param validateShares if to check also shares flags and figures or only Extraction Rates.
##' Default set to TRUE
##' 
##' @return A message with indication of validation complete or a file with values to correct
##' an email if on the system.
##' 
##' @export
##' 

validateTree = function(tree= NULL, min.er = 0, max.er = 10, validateShares = TRUE){
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
    
    vER=validateTreeExtractionRates(tree = tree, min.er = min.er, max.er = max.er)
    messageER=vER$messageER
    invalidER=vER$invalidER
    
    if(validateShares){
        vSH=validateTreeShares(tree = tree)
        messageSH=vER$messageSH
        invalidSH=vER$invalidSH
        invalidTree=rbind(invalidER,invalidSH)
        messageTree=paste(messageER,messageSH,sep='\n')
    }else{
        invalidTree=invalidER
        messageTree=messageER
    }
    
    if(dim(invalidTree)[1]>0){
        
        if(!CheckDebug()){
            # Create the body of the message
            
            FILETYPE = ".csv"
            CONFIG <- faosws::GetDatasetConfig(swsContext.datasets[[1]]@domain, swsContext.datasets[[1]]@dataset)
            sessionid <- ifelse(length(swsContext.datasets[[1]]@sessionId), 
                                swsContext.datasets[[1]]@sessionId,
                                "core")
            
            basename <- sprintf("%s_%s",
                                "invalidTreeRows",
                                sessionid)
            basedir <- tempfile()
            dir.create(basedir)
            destfile <- file.path(basedir, paste0(basename, FILETYPE))
            
            # create the csv in a temporary foldes   
            write.csv(invalidTree, destfile, row.names = FALSE)  
            # define on exit strategy
            on.exit(file.remove(destfile))    
            zipfile <- paste0(destfile, ".zip")
            withCallingHandlers(zip(zipfile, destfile, flags = "-j9X"),
                                warning = function(w){
                                    if(grepl("system call failed", w$message)){
                                        stop("The system ran out of memory trying to zip up your data. Consider splitting your request into chunks")
                                    }
                                })
            
            on.exit(file.remove(zipfile), add = TRUE)
            body = paste("Standardization stopped because of:",
                         messageTree,
                         " ",
                         paste0("check Share = ",validateShares),
                         " ",
                         "Look attached file for details",
                         " ",
                         "The file can be modified and uploaded in the Commodity Tree",
                         " ",
                         "=================================================",
                         "Valid Flags for Commodity Tree are the following",
                         " ",
                         "Extraction rates:",
                         "(T,-) = validated up do 2013 - protected",
                         "(E,t) = copied from 2014 onwards - not protected but that do not change during standardization process",
                         "(E,f) = manually changed by the user - protected",
                         " ",
                         "Shares:",
                         "(E,-) = coming from old methodology - NOT protected. These values willbe overwritten",
                         "any run of the module, except the values of oils, which are kept, unless manually changed",
                         "(E,f) = manually changed by the user - protected",
                         "(I,i) = calculated by the module - not protected",
                         " ",
                         "=================================================",
                         paste0("Valid Figures for Extraction Rates are in the range: ",min.er," to ", max.er)
                         ,sep='\n')
            
            sendmailR::sendmail(from = "sws@fao.org",
                                to = swsContext.userEmail,
                                subject = sprintf("Standardization stopped for invalid Flags and/or Figures"),
                                msg = list(strsplit(body,"\n")[[1]], 
                                           sendmailR::mime_part(destfile, 
                                                                name = paste0(basename, FILETYPE)
                                           )
                                )
            )
            stop("Process Stopped")
            paste0("Email sent to ", swsContext.userEmail)
        }else{
            if(file.exists(paste0("debugFile/Batch_",batchnumber,"/B",batchnumber,"__0_invalidTreeRows.csv"))){
                file.remove(paste0("debugFile/Batch_",batchnumber,"/B",batchnumber,"__0_invalidTreeRows.csv"))
            }
            dir.create(paste0(PARAMS$debugFolder,"/Batch_",batchnumber), showWarnings = FALSE,recursive=TRUE)
            write.csv(invalidTree, paste0(PARAMS$debugFolder,"/Batch_",batchnumber,"/B",batchnumber,"__0_invalidTreeRows.csv"), row.names = FALSE)  
            return(paste0("Process Stopped for Invalid rows in Commodity Tree. Csv file saved in: ", paste0(PARAMS$debugFolder,"/Batch_",batchnumber,"/B",batchnumber,"__0_invalidTreeRows.csv")))
            
        }
        
    }
    # else{  # End of case for which flags and/or figures are invalid
    #     if(!CheckDebug()){
    #         body = paste("The Commodity Tree is valid",
    #                      " ",
    #                      "If no check has been performed on Shares, be aware that shares might be invalid",
    #                      sep='\n')
    #         sendmailR::sendmail(from = "sws@fao.org",
    #                             to = swsContext.userEmail,
    #                             subject = sprintf("tree successfully downloaded and Checked"),
    #                             msg = strsplit(body,"\n")[[1]])
    #         
    #     }
    #     message("Commodity Tree Extraction Rates are valid")
    # }
}
