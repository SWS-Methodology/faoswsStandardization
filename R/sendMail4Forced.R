##' Send email after having checked if some protected figure of production has been changed 
##' 
##' This function simply snd email to the user, 
##' with the result of the check on shares
##' 
##' @param forcedData If existing, are the SUA of the commodity for which
##' the change of the production figure was required in order to Fill the SUA
##' 
##' @return The function doesn't return anything, but send mail 
##' 
##' @export

sendMail4forced=function(forcedData){
  if(dim(forcedData)[1]>0){
    if(!CheckDebug()){
      # Create the body of the message
      
      FILETYPE = ".csv"
      CONFIG <- faosws::GetDatasetConfig(swsContext.datasets[[1]]@domain, swsContext.datasets[[1]]@dataset)
      sessionid <- ifelse(length(swsContext.datasets[[1]]@sessionId), 
                          swsContext.datasets[[1]]@sessionId,
                          "core")
      
      basename <- sprintf("%s_%s",
                          "forcedProduction",
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
      body = paste("You are receiving this email because some protected figure of production",
                   "has been changed for balancing at SUA level.",
                   "This should not be possible and you might have to check why the corresponding SUA",
                   "is so heavily unbalanced to require such a non-feasible change",
                   "Check it on the attached file",
                   sep='\n')
      
      sendmailR::sendmail(from = "sws@fao.org",
                          to = swsContext.userEmail,
                          subject = sprintf("Some Protected production figure changed"),
                          msg = list(strsplit(body,"\n")[[1]], 
                                     sendmailR::mime_part(destfile, 
                                                          name = paste0(basename, FILETYPE)
                                     )
                          )
      )
    }
  }
}
