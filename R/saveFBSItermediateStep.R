##' Save Intermediate steps of Standardization process
##' 
##' This function has been created in order to locally save some intermediate
##' steps of the Standardization module. This exigency arose for validation
##' purposes.
##' 
##' @param directory Where does the intermediate step have to be saved back to?
##' @param fileName Name of the file we are creating
##' @param data data.table to be saved
##'   
##' @export

saveFBSItermediateStep=function(directory, fileName, data){
  
  ## Check if the directory specifed in the arguments of the function exists
  
  
  if(!dir.exists(directory)){
    dir.create(directory, recursive = TRUE)
               
  } 

 
  write.table(data, paste0(directory, "/", fileName,".csv"), sep=";", append=T, col.names = F ,row.names = F)
  
}