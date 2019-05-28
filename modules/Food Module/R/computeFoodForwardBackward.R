##' Calculate Food Forward and Backward
##' 
##' The function compute the estimates for food consumption in the time t based 
##' on the food consumption in the t-1.
##' 
##' @param food The food consumption at time t.
##' @param pop Population at time t.
##' @param elas The elasticity of the commodity.  This will vary by country and 
##'   commodity.
##' @param gdp_pc Per person GDP (gross domestic product) at time t.
##' @param netTrade The Net Trade (imports - exports) at time t. 
##' @param functionalForm Currently one of 0, 1, 2, or 3.  Specifies if a log-log,
##'   semi-log, or inverse-log food demand model should be used.

##' @param timePointYears A numeric vector with the years.
##' @param protected A logical value indicating whether the value is proetcted or not.
##' @param type A character string indicating the levels of the food classification of the commodity.
##' @param referenceYear Year of reference to start to compute the food estimates.
##' 
##' @return A numeric vector of the estimated food consumption.
##' 
##' @export
##' 

computeFoodForwardBackward <- function(food, pop, elas, gdp, netTrade, functionalForm,
                                       timePointYears, protected, type, referenceYear){
    
    result <- NA_real_
    N = length(unique(timePointYears))
    first = referenceYear - min(timePointYears) + 1
    result[first] = food[first]
    if (referenceYear > min(timePointYears)) {
        
        # backward
        for (i in (first-1):1){
            if(protected[i] == TRUE){
                result[i] = food[i]
            } else if(type[i] == "Food Residual" & netTrade[i] > 0){
                result[i] = netTrade[i]
            } else if(type[i] == "Food Residual" & netTrade[i] <= 0){
                result[i] = 0
            } else if(protected[i] == FALSE & type[i] != "Food Residual"){ 
                food_t0 = as.numeric(result[i+1])
                pop_t0 = as.numeric(pop[i+1])
                pop_t1 = as.numeric(pop[i])
                elas_t = as.numeric(elas[i])
                gdp_pc_t0 = as.numeric(gdp[i+1])
                gdp_pc_t1 = as.numeric(gdp[i])
                form = functionalForm[i]
                
                result[i] = center(form, food_t0, elas_t, gdp_pc_t0, gdp_pc_t1, pop_t0, pop_t1)
            } else {
                stop("This is not currently implemented.")
            }
        }        
    }
    
    if (referenceYear < max(timePointYears)) {
        # forward
        for (i in (first+1):N){
            
            if(protected[i] == TRUE){
                result[i] = food[i]
            } else if(type[i] == "Food Residual" & netTrade[i] > 0){
                result[i] = netTrade[i]
            } else if(type[i] == "Food Residual" & netTrade[i] <= 0){
                result[i] = 0
            } else if(protected[i] == FALSE & type[i] != "Food Residual"){ 
                food_t0 = as.numeric(result[i-1])
                pop_t0 = as.numeric(pop[i-1])
                pop_t1 = as.numeric(pop[i])
                elas_t = as.numeric(elas[i])
                gdp_pc_t0 = as.numeric(gdp[i-1])
                gdp_pc_t1 = as.numeric(gdp[i])
                form = functionalForm[i]
                
                result[i] = center(form, food_t0, elas_t, gdp_pc_t0, gdp_pc_t1, pop_t0, pop_t1)
            } else {
                stop("This is not currently implemented.")
            }
        }

    }
    return(result)
}