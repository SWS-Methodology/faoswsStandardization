##' NA to zero
##' 
##' Helper function to convert NA's to 0 when summing.
##' 
##' @param x A numeric value.
##'   
##' @return The value of x if x has a value and 0 if x is na or has length 0. 
##'   If x has length >1, an error is thrown.
##' 
##' @export
##' 

na2zero = function(x){
    if(length(x) == 0) # No data for variable, return 0
        return(0)
    if(is.na(x))
        return(0) # Missing, return 0
    if(length(x) > 1)
        stop("Length of x should be 1 or 0")
    return(x)
}
