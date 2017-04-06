## This function hs been created in order to locally save some intermedate steps
## of the Standardization module. This exigency arose for validation purposes.
##' @param directory Where does the intermediate step has to be saved back?
##' @param fileName Name of the file we are creating
##' @param data data.table that has been saved into the 
##' 
##' @export

saveFBSItermediateStep=function(directory, fileName, data){
  
  ## Check if the directory specifed in the arguments of the function exists
  
  
  if(!dir.exists(directory)){
    dir.create(directory, recursive = TRUE)
               
  } 

 
  write.table(data, paste0(directory, "/", fileName,".csv"), sep=";", append=T, col.names = F ,row.names = F)
  
}