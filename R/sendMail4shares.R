##' Send email after having checked for shares 
##' 
##' This function simply snd email to the user, 
##' with the result of the check on shares
##' 
##' @param tree single subset of the Tree, which means, a subset having all the parent of a single child
##' where the child is an official one. The subset has the characteristis of having shares not summing at 1
##' 
##' @return message with the status of shares
##' 
##' @export
##' 

sendMail4shares=function(tree){
  
  if(dim(tree)[1]>0){
    
    if(any(tree[,share]==0)){
      messageSH=paste("Some manually changed shares might have been recaluclated",
                      "because of inconsistencies",
                      " ",
                      "Moreover:",
                      "There are shares = 0",
                      "Consider deleting ER for deleting connections",
                      " ",
                      "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!",
                      "THE ACCHACHED FILE IS JUST FOR EASY CONSULTATION",
                      "DO NOT TRY TO UPLOAD IT IN THE SWS",
                      "GO TO YOUR SESSION AND MAKE CHANGES THERE",
                      "YOU WILL FIND IN THE SESSION ALL THE VALUE CHANGED",
                      "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!",
                      sep='\n')
    }else{
      messageSH=paste("Some manually changed shares might have been recaluclated",
                      "because of inconsistencies",
                      " ",
                      "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!",
                      "THE ACCHACHED FILE IS JUST FOR EASY CONSULTATION",
                      "DO NOT TRY TO UPLOAD IT IN THE SWS",
                      "GO TO YOUR SESSION AND MAKE CHANGES THERE",
                      "YOU WILL FIND IN THE SESSION ALL THE VALUE CHANGED",
                      "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!",
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
      write.csv(tree, destfile, row.names = FALSE)  
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
                   "Changed Shares are highlighted in your commodity Tree Session"
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
      write.csv(tree, paste0(PARAMS$debugFolder,"/Batch_",batchnumber,"/B",batchnumber,"__0_shareCorrections.csv"), row.names = FALSE)  
      return(message(paste0(dim(tree[severity>0])[1]," shares changed")))
    }
    
  }
  # else{  # End of case for which flags and/or figures are invalid
  #   if(!CheckDebug()){
  #     body = paste("Valid shares")
  #     sendmailR::sendmail(from = "sws@fao.org",
  #                         to = swsContext.userEmail,
  #                         subject = sprintf("Share successfully checked and saved in the session"),
  #                         msg = strsplit(body,"\n")[[1]])
  #     
  #   }
  #   return(message("Valid Shares"))
  # }

}
