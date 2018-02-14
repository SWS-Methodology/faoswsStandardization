##' Send email after having checked for shares 
##' 
##' This function simply snd email to the user, 
##' with the result of the check on shares
##' 
##' @param tree2Subset single subset of the Tree, which means, a subset having all the parent of a single child
##' where the child is an official one. The subset has the characteristis of having shares not summing at 1
##' 
##' @return The function doesn't return anything, but send mail 
##' 
##' @export

sendMail4shares=function(tree2change){
  
  if(dim(tree2change)[1]>0){
    
    if(any(tree2change[,share]==0)){
      messageSH=paste("Some manually changed shares might have been recaluclated",
                      "because of inconsistencies",
                      " ",
                      "There are shares = 0",
                      "Consider deleting ER for deleting connections",
                      sep='\n')
    }else{
      messageSH=paste("Some manually changed shares might have been recaluclated",
                      "because of inconsistencies",
                      sep='\n')
    }
    
    if(!CheckDebug()){
      # Create the body of the message
      
      FILETYPE = ".csv"
      CONFIG <- faosws::GetDatasetConfig(swsContext.datasets[[1]]@domain, swsContext.datasets[[1]]@dataset)
      sessionid <- ifelse(length(swsContext.datasets[[1]]@sessionId), 
                          swsContext.datasets[[1]]@sessionId,
                          "core")
      
      basename <- sprintf("%s_%s",
                          "ShareCorrections",
                          sessionid)
      basedir <- tempfile()
      dir.create(basedir)
      destfile <- file.path(basedir, paste0(basename, FILETYPE))
      
      # create the csv in a temporary foldes   
      write.csv(tree2change, destfile, row.names = FALSE)  
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
      body = paste("Shares have been checked and corrected",
                   " ",
                   messageSH,
                   " ",
                   "Changed Shares are highlighted in your commodity Tree Session",
                   ,sep='\n')
      
      sendmailR::sendmail(from = "sws@fao.org",
                          to = swsContext.userEmail,
                          subject = sprintf("Outcome of checks on shares"),
                          msg = list(strsplit(body,"\n")[[1]], 
                                     sendmailR::mime_part(destfile, 
                                                          name = paste0(basename, FILETYPE)
                                     )
                          )
      )
    }else{
      if(file.exists(paste0("debugFile/Batch_",batchnumber,"/B",batchnumber,"__0_shareCorrections.csv"))){
        file.remove(paste0("debugFile/Batch_",batchnumber,"/B",batchnumber,"__0_shareCorrections.csv"))
      }
      dir.create(paste0(PARAMS$debugFolder,"/Batch_",batchnumber), showWarnings = FALSE,recursive=TRUE)
      write.csv(ShareCorrections, paste0(PARAMS$debugFolder,"/Batch_",batchnumber,"/B",batchnumber,"__0_shareCorrections.csv"), row.names = FALSE)  
      return(tree2change)
    }
    
  }else{  # End of case for which flags and/or figures are invalid
    if(!CheckDebug()){
      body = paste("No changes in shares")
      sendmailR::sendmail(from = "sws@fao.org",
                          to = swsContext.userEmail,
                          subject = sprintf("Share successfully checked and saved in the session"),
                          msg = strsplit(body,"\n")[[1]])
      
    }
    message("Valid Shares")
  }

}
