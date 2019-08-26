# test for github
start_time <- Sys.time()

print("NEWBAL: start")

# Always source files in R/ (useful for local runs).
# Your WD should be in faoswsStandardization/
sapply(dir("R", full.names = TRUE), source)

# Flags set by this plugin. These are temporary: once the testing phase is over,
# most of these should be changed to I,e, E,e, I,i.
#
# E,i: Food processing generated
# E,c: Balancing: production modified to compensate net trade
# E,s: Balancing: stocks modified
# E,h: Balancing: imbalance to food, given food is only utilization
# E,-: Balancing: utilization modified
# E,u: Stocks variation generated
# I,c: Derived production generated
# E,e: Outlier replaced
# T,c: Opening stocks updated
# I,i: Residual item (identity)
# M,q: Cases for which flags were not set.
# T,i: Calories per capita created
# T,p: USDA stocks data
# I,e: Module imputation
# E,b: Final balancing (industrial, Feed)
#
# M,q should not happen, but added so to check.


print("NEWBAL: define functions")

######### FUNCTIONS: at some point, they will be moved out of this file. ##

# The fmax function is used when fixing the processingShare of coproducts.
# If TRUE it means that "+" or "or" cases are involved.
fmax <- function(child, main, share, plusor = FALSE) {
  main <- unique(main)

   if (plusor) {
     found <- sum(sapply(child, function(x) grepl(x, main), USE.NAMES = FALSE))

     if (found == 0) {
       return(max(share, na.rm = TRUE))
     } else if (found == 1) {
       return(share[(1:length(child))[sapply(child, function(x) grepl(x, main), USE.NAMES = FALSE)]])
     } else { # should be 2
       return(max(share[(1:length(child))[sapply(child, function(x) grepl(x, main), USE.NAMES = FALSE)]], na.rm = TRUE))
     }
  } else {
    if (sum(grepl(main, child)) > 0) {
      share[child == main]
    } else {
      max(share, na.rm = TRUE)
    }
  }
}

# Function that calculates imbalance as supply - utilizations (both calculated
# inside the function, dropped if keep_(supply|utilizations) set to FALSE).
calculateImbalance <- function(data,
                               supply_add = c("production", "imports"),
                               supply_subtract = c("exports", "stockChange"),
                               supply_all = union(supply_add, supply_subtract),
                               item_name = "measuredItemSuaFbs",
                               bygroup = c("geographicAreaM49", "timePointYears", item_name),
                               keep_supply = TRUE,
                               keep_utilizations = TRUE) {
  
  stopifnot(is.data.table(data))
  
  data[,
       `:=`(
         supply =
           sum(Value[measuredElementSuaFbs %chin% supply_add],
               - Value[measuredElementSuaFbs %chin% supply_subtract],
               na.rm = TRUE),
         # All elements that are NOT supply elements
         utilizations =
           sum(Value[!(measuredElementSuaFbs %chin% supply_all)],
               na.rm = TRUE)
       ),
       by = bygroup
       ][,
         imbalance := supply - utilizations
         ]
  
  if (keep_supply == FALSE) {
    data[, supply := NULL]
  }
  
  if (keep_utilizations == FALSE) {
    data[, utilizations := NULL]
  }
  
}

outside <- function(x, lower = NA, upper = NA) {
  x < lower | x > upper
}


send_mail <- function(from = NA, to = NA, subject = NA,
                      body = NA, remove = FALSE) {
  
  if (missing(from)) from <- 'no-reply@fao.org'
  
  if (missing(to)) {
    if (exists('swsContext.userEmail')) {
      to <- swsContext.userEmail
    }
  }
  
  if (is.null(to)) {
    stop('No valid email in `to` parameter.')
  }
  
  if (missing(subject)) stop('Missing `subject`.')
  
  if (missing(body)) stop('Missing `body`.')
  
  if (length(body) > 1) {
    body <-
      sapply(
        body,
        function(x) {
          if (file.exists(x)) {
            # https://en.wikipedia.org/wiki/Media_type 
            file_type <-
              switch(
                tolower(sub('.*\\.([^.]+)$', '\\1', basename(x))),
                txt  = 'text/plain',
                csv  = 'text/csv',
                png  = 'image/png',
                jpeg = 'image/jpeg',
                jpg  = 'image/jpeg',
                gif  = 'image/gif',
                xls  = 'application/vnd.ms-excel',
                xlsx = 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
                doc  = 'application/msword',
                docx = 'application/vnd.openxmlformats-officedocument.wordprocessingml.document',
                pdf  = 'application/pdf',
                zip  = 'application/zip',
                # https://stackoverflow.com/questions/24725593/mime-type-for-serialized-r-objects
                rds  = 'application/octet-stream'
              )
            
            if (is.null(file_type)) {
              stop(paste(tolower(sub('.*\\.([^.]+)$', '\\1', basename(x))),
                         'is not a supported file type.'))
            } else {
              return(sendmailR:::.file_attachment(x, basename(x), type = file_type))
            }
            
            if (remove) {
              unlink(x)
            }
          } else {
            return(x)
          }
        }
      )
  } else if (!is.character(body)) {
    stop('`body` should be either a string or a list.')
  }
  
  sendmailR::sendmail(from, to, subject, as.list(body))
}





################### optim stuff ###########################

my_fun <- function(s) {
  
  zero_if_na <- function(x) {
    ifelse(length(x) == 0, 0, ifelse(is.na(x), 0, x))
  }
  
  prod_i <- x[, measuredElementSuaFbs == 'production']
  impo_i <- x[, measuredElementSuaFbs == 'imports']
  expo_i <- x[, measuredElementSuaFbs == 'exports']
  stoc_i <- x[, measuredElementSuaFbs == 'stockChange']
  food_i <- x[, measuredElementSuaFbs == 'food']
  feed_i <- x[, measuredElementSuaFbs == 'feed']
  seed_i <- x[, measuredElementSuaFbs == 'seed']
  proc_i <- x[, measuredElementSuaFbs == 'foodManufacturing']
  indu_i <- x[, measuredElementSuaFbs == 'industrial']
  loss_i <- x[, measuredElementSuaFbs == 'loss']
  tour_i <- x[, measuredElementSuaFbs == 'tourist']
  resi_i <- x[, measuredElementSuaFbs == 'residual']
  
  prod_v <- zero_if_na(x$Value[prod_i])
  impo_v <- zero_if_na(x$Value[impo_i])
  expo_v <- zero_if_na(x$Value[expo_i])
  stoc_v <- zero_if_na(x$Value[stoc_i])
  food_v <- zero_if_na(x$Value[food_i])
  feed_v <- zero_if_na(x$Value[feed_i])
  seed_v <- zero_if_na(x$Value[seed_i])
  proc_v <- zero_if_na(x$Value[proc_i])
  indu_v <- zero_if_na(x$Value[indu_i])
  loss_v <- zero_if_na(x$Value[loss_i])
  tour_v <- zero_if_na(x$Value[tour_i])
  resi_v <- zero_if_na(x$Value[resi_i])
  
  #  prod_p <- x$Protected[prod_i] ; prod_p <- ifelse(length(prod_p) == 0, 0, ifelse(is.na(prod_p), 0, prod_p))
  #  impo_p <- x$Protected[impo_i] ; impo_p <- ifelse(length(impo_p) == 0, 0, ifelse(is.na(impo_p), 0, impo_p))
  #  expo_p <- x$Protected[expo_i] ; expo_p <- ifelse(length(expo_p) == 0, 0, ifelse(is.na(expo_p), 0, expo_p))
  #  stoc_p <- x$Protected[stoc_i] ; stoc_p <- ifelse(length(stoc_p) == 0, 0, ifelse(is.na(stoc_p), 0, stoc_p))
  #  food_p <- x$Protected[food_i] ; food_p <- ifelse(length(food_p) == 0, 0, ifelse(is.na(food_p), 0, food_p))
  #  feed_p <- x$Protected[feed_i] ; feed_p <- ifelse(length(feed_p) == 0, 0, ifelse(is.na(feed_p), 0, feed_p))
  #  seed_p <- x$Protected[seed_i] ; seed_p <- ifelse(length(seed_p) == 0, 0, ifelse(is.na(seed_p), 0, seed_p))
  #  proc_p <- x$Protected[proc_i] ; proc_p <- ifelse(length(proc_p) == 0, 0, ifelse(is.na(proc_p), 0, proc_p))
  #  indu_p <- x$Protected[indu_i] ; indu_p <- ifelse(length(indu_p) == 0, 0, ifelse(is.na(indu_p), 0, indu_p))
  #  loss_p <- x$Protected[loss_i] ; loss_p <- ifelse(length(loss_p) == 0, 0, ifelse(is.na(loss_p), 0, loss_p))
  #  tour_p <- x$Protected[tour_i] ; tour_p <- ifelse(length(tour_p) == 0, 0, ifelse(is.na(tour_p), 0, tour_p))
  #  resi_p <- x$Protected[resi_i] ; resi_p <- ifelse(length(resi_p) == 0, 0, ifelse(is.na(resi_p), 0, resi_p))
  
  supply <- prod_v + impo_v - expo_v - stoc_v
  
  #  utilizations <-
  #    (food_v * !food_p) * s[1] +
  #    (feed_v * !feed_p) * s[2] +
  #    (seed_v * !seed_p) * s[3] +
  #    (proc_v * !proc_p) * s[4] +
  #    (indu_v * !indu_p) * s[5] +
  #    (loss_v * !loss_p) * s[6] +
  #    (tour_v * !tour_p) * s[7] +
  #    (resi_v * !resi_p) * s[8]
  
  utilizations <-
    food_v * s[1] +
    feed_v * s[2] +
    seed_v * s[3] +
    proc_v * s[4] +
    indu_v * s[5] +
    loss_v * s[6] +
    tour_v * s[7] +
    resi_v * s[8]
  
  abs(supply - utilizations)
}


do_optim <- function(d) {
  
  elem_names <- c('food', 'feed', 'seed', 'foodManufacturing',
                  'industrial', 'loss', 'tourist', 'residual')
  
  my_lower <- d$min_adj
  my_upper <- d$max_adj
  
  my_protected <- d$Protected
  
  names(my_lower) <- names(my_upper) <- names(my_protected) <- d$measuredElementSuaFbs
  
  my_lower <- my_lower[elem_names]
  my_upper <- my_upper[elem_names]
  my_protected <- my_protected[elem_names]
  
  names(my_lower) <- names(my_upper) <- names(my_protected) <- elem_names
  
  my_lower[is.na(my_lower) | my_protected == TRUE] <- 0.999999
  my_upper[is.na(my_upper) | my_protected == TRUE] <- 1.000001
  
  # XXX this used to be &, but we want now all 1s so it chan change
  initial <- (my_lower | my_upper ) * 1.0
  
  #initial[initial < 1] <- 0.00000001
  
  opt <- optim(initial, my_fun, method = "L-BFGS-B",
               lower = my_lower, upper = my_upper,
               control = list(pgtol = 0.0001))
  
  opt$par <- opt$par * !dplyr::near(opt$par, 0, 0.0000001)
  
  res <- reshape2::melt(as.list(opt$par))
  
  setDT(res)
  
  setnames(res, c("value", "L1"), c("adj", "measuredElementSuaFbs"))
  
  # Round to keep things actually different from 1.
  res[, adj := ifelse(dplyr::near(adj, 1, tol = 0.0001), 1, adj)]
  
  return(res)
}


balance_optimization <- function(d) {
  myres_optim <- do_optim(d)
  
  res <- myres_optim[d[, list(measuredElementSuaFbs, Value)],
                     on = c('measuredElementSuaFbs')]
  
  res[, adj := adj * Value]
  
  res[dplyr::near(adj, 0) | dplyr::near(adj, 1), adj := NA_real_]
  
  return(res$adj)
}

################### / optim stuff ###########################


balance_proportional <- function(data) {
  
  x <- copy(data)
  
  x <-
    x[
      Protected == FALSE &
        !measuredElementSuaFbs %in%
        c("production", "imports", "exports", "stockChange", "foodManufacturing"),
      list(measuredElementSuaFbs, measuredItemSuaFbs, Value,
           mov_share, imbalance, min_threshold, max_threshold)
      ]
  
  # Recalculate mov_share, as we are excluding protected values
  x[, mov_share := mov_share / sum(mov_share, na.rm = TRUE)]
  x = as.data.frame(x)
  x$Value_temp = x$Value
  x$Value_temp[is.na(x$Value_temp)] = 0
  x = as.data.table(x)
  
  # x[, adjusted_value := ifelse(Value + mov_share * imbalance>=0,Value + mov_share * imbalance,0)]
  x[, adjusted_value := ifelse(Value_temp + mov_share * imbalance>=0,Value_temp + mov_share * imbalance,0)]
  
  # x[adjusted_value > Value & adjusted_value > max_threshold, adjusted_value := max_threshold]
  x[adjusted_value > Value_temp & adjusted_value > max_threshold, adjusted_value := max_threshold]
  
  # x[adjusted_value < Value & adjusted_value < min_threshold, adjusted_value := min_threshold]
  x[adjusted_value < Value_temp & adjusted_value < min_threshold, adjusted_value := min_threshold]
  x = as.data.frame(x)
  x = dplyr::select(x,-Value_temp)
  x = as.data.table(x)
  
  x <-
    merge(
      data,
      x[, list(measuredElementSuaFbs, measuredItemSuaFbs, adjusted_value)],
      by = c("measuredElementSuaFbs", "measuredItemSuaFbs"),
      all.x = TRUE
    )
  
  return(x$adjusted_value)
}





# rollavg() is a rolling average function that uses computed averages
# to generate new values if there are missing values (and FOCB/LOCF).
# I.e.:
# vec <- c(NA, 2, 3, 2.5, 4, 3, NA, NA, NA)
#
#> RcppRoll::roll_mean(myvec, 3, fill = 'extend', align = 'right')
#[1]       NA       NA       NA 2.500000 3.166667 3.166667       NA       NA       NA 
#
#> rollavg(myvec)
#[1] 2.000000 2.000000 3.000000 2.500000 4.000000 3.000000 3.166667 3.388889 3.185185

rollavg <- function(x, order = 3) {
  # order should be > 2
  stopifnot(order >= 3)
  
  non_missing <- sum(!is.na(x))
  
  # For cases that have just two non-missing observations
  order <- ifelse(order > 2 & non_missing == 2, 2, order)
  
  if (non_missing == 1) {
    x[is.na(x)] <- na.omit(x)[1]
  } else if (non_missing >= order) {
    n <- 1
    while(any(is.na(x)) & n <= 10) { # 10 is max tries
      movav <- suppressWarnings(RcppRoll::roll_mean(x, order, fill = 'extend', align = 'right'))
      movav <- data.table::shift(movav)
      x[is.na(x)] <- movav[is.na(x)]
      n <- n + 1
    }
    
    x <- zoo::na.fill(x, 'extend')
  }
  
  return(x)
}









newBalancing <- function(data, tree, utilizationTable, Utilization_Table, zeroWeight) {
  
  # Contains a variable that indicates whether stocks changed
  data[, change_stocks := NA_integer_]
  
  if (nrow(tree) > 0) {
    level <- findProcessingLevel(tree, from = p$parentVar,
                                 to = p$childVar, aupusParam = p)
    
    # TODO check if there are multiple levels and how to handle that
    
    data <- merge(data, level, by = 'measuredItemSuaFbs', all.x = TRUE)
    
    # TODO: balance also these: data[is.na(processingLevel)]
    # XXX: correct?
    data[is.na(processingLevel), processingLevel := 0]
    
    # XXX: na.rm = TRUE?
    data[,
         min_processingLevel := min(processingLevel),
         by = c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs")
         ]
    
    
    # XXX define "food residual" those items for which the only utilization
    # is food. Food processing can also be another possible utilization and
    # if there is that does not change its food-residualness, this is why
    # the check is done before assigning food processing.
    # NW: I have commented the conditions out
    # Now it only checks to make sure that food is the only utilization
    # We noticed that some of the food items were missing from the utilization table
    # This is still different from the previous approach of assigning all of the imbalance to 
    # food when "none of the other utilizations are activable"
  if (NEW_FOOD_RESIDUAL==TRUE){
    data[,
         food_resid :=
           # It's a food item & ...
           #(measuredItemSuaFbs %in% Utilization_Table[food_item == 'X', cpc_code] |
           # food exists & ...
           # !is.na(Value[measuredElementSuaFbs == 'food']) &
           Food_Median > 0 & !is.na(Food_Median) &
           # ... is the only utilization
           all(is.na(Value[!(measuredElementSuaFbs %in%
                               c('food', 'production', 'imports', 'exports', 'stockChange','foodManufacturing'))])),
         by = c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs")
         ]
  }else{
    data[,
         food_resid :=
           # It's a food item & ...
           measuredItemSuaFbs %in% Utilization_Table[food_item == 'X', cpc_code] &
           # food exists & ...
           !is.na(Value[measuredElementSuaFbs == 'food']) &
           # ... is the only utilization
           all(is.na(Value[!(measuredElementSuaFbs %in%
                               c('food', 'production', 'imports', 'exports', 'stockChange'))])),
         by = c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs")
         ]   
  }
    
    #Checking if the commodity has past value before assigning the residual imbalance at the end
    #of the balancing procees
    data[,
         `:=`(feed_resid =
                # It's a feed item or have past value & ...
                #(measuredItemSuaFbs %in% Utilization_Table[feed == 'X', cpc_code] |
                (Feed_Median > 0 & !is.na(Feed_Median)) &
                #feed is the only utilization....
                all(is.na(Value[!(measuredElementSuaFbs %in%
                                    c('feed', 'production', 'imports', 'exports', 'stockChange','foodManufacturing'))])),
              # It's a industrial item or have past value & ...
              industrial_resid=Industrial_Median > 0 & !is.na(Industrial_Median)),
         by = c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs")
         ] 
     
    # Commodities with missing processing level => they are not in the tree.
    # This should be investigated, they will be balanced separately.
    
    for (lev in rev(unique(level$processingLevel))) {
      
      # Food processing that come from lower levels (i.e., with higher numbers)
      
      if (exists('food_proc_table')) {
        data <-
          merge(
            data,
            food_proc_table,
            by = c('geographicAreaM49', 'timePointYears', 'measuredItemSuaFbs'),
            all.x = TRUE
          )
        
        data[
          measuredElementSuaFbs == "foodManufacturing" &
            Protected == FALSE & !is.na(food_proc),
          `:=`(
            Value = ifelse(is.na(Value), 0, Value) + food_proc,
            flagObservationStatus = "E",
            flagMethod = "i",
            Protected = ifelse(lev == min_processingLevel, TRUE, FALSE)
          )
          ][,
            food_proc := NULL
            ]
        
        rm(food_proc_table)
      }
      
      data_level <- data[processingLevel == lev]
      
      # This is used only to check whether production needs to be generated,
      # hence no stocks are considered.
      
      data_level[,
                 supply :=
                   sum(
                     Value[measuredElementSuaFbs %chin% c('production', 'imports')],
                     - Value[measuredElementSuaFbs %chin% c('exports')],
                     # XXX: should stocks be included?
                     #- Value[measuredElementSuaFbs %chin% c('exports', 'stockChange')],
                     na.rm = TRUE
                   ),
                 # year is quite unnecessary, but let's use it in any case
                 by = c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs")
                 ]
      
      
      # When production needs to be created
      
      data_level[
        Protected == FALSE &
          # Only primary
          measuredItemSuaFbs %chin% Utilization_Table[primary_item == "X"]$cpc_code &
          measuredElementSuaFbs == 'production' &
          supply < 0 &
          stockable == FALSE,
        `:=`(
          Value = ifelse(is.na(Value), 0, Value) - supply,
          flagObservationStatus = "E",
          flagMethod = "c"
        )
        ]
      
      calculateImbalance(data_level)
      
      if (NEW_STOCKS_POSITION==FALSE){   
        # Try to assign the maximum of imbalance to stocks
        
        data_level[,
                   Value_0 := ifelse(is.na(Value), 0, Value)
                   ][
                     outside(imbalance, -1, 1) & measuredElementSuaFbs == "stockChange" & stockable == TRUE,
                     change_stocks :=
                       # The numbers indicate the case. Assignmnet (value and flags) will be done below
                       case_when(
                         # case 1: we don't want stocks to change sign.
                         sign(Value_0) * sign(Value_0 + imbalance) == -1                                        ~ 1L,
                         # case 2: if value + imbalance takes LESS than opening stock, take all from stocks
                         Value_0 <= 0 & (Value_0 + imbalance <= 0) & abs(Value_0 + imbalance) <= opening_stocks ~ 2L,
                         # case 3: if value + imbalance takes MORE than opening stock, take max opening stocks
                         Value_0 <= 0 & (Value_0 + imbalance <= 0) & abs(Value_0 + imbalance) > opening_stocks  ~ 3L,
                         # case 4: if value + imbalance send LESS than 20% of supply, send all
                         Value_0 >= 0 & (Value_0 + imbalance >= 0) & (opening_stocks+Value_0 + imbalance <= supply * 0.2)      ~ 4L,
                         # case 5: if value + imbalance send MORE than 20% of supply, send 20% of supply
                         Value_0 >= 0 & (Value_0 + imbalance >= 0) & (opening_stocks+Value_0 + imbalance > supply * 0.2)       ~ 5L
                       )
                     ]
        
        data_level[change_stocks == 1L, Value := 0]
        data_level[change_stocks == 2L, Value := Value_0 + imbalance]
        data_level[change_stocks == 3L, Value := - opening_stocks]
        data_level[change_stocks == 4L, Value := Value_0 + imbalance]
        data_level[change_stocks == 5L, Value := supply * 0.2 ]
        
        data_level[change_stocks %in% 1L:5L, `:=`(flagObservationStatus = "E", flagMethod = "s")]
        
        data_level[, Value_0 := NULL]
      } 
      
      # Recalculate imbalance
      calculateImbalance(data_level)
      # Assign imbalance to food if food "only" (not "residual") item
      if(NEW_FOOD_RESIDUAL==TRUE){
      data_level[
        Protected == FALSE & food_resid == TRUE & outside(imbalance, -1, 1) & measuredElementSuaFbs == "food",
        `:=`(
          Value = ifelse(is.na(Value) & imbalance>0,imbalance,ifelse(Value + imbalance >= 0, Value + imbalance, 0)),
          flagObservationStatus = "E",
          flagMethod = "h"
        )
        ]
      }else{
        
        data_level[
          Protected == FALSE & food_resid == TRUE & outside(imbalance, -100, 100) & measuredElementSuaFbs == "food",
          `:=`(
            Value = ifelse(Value + imbalance >= 0, Value + imbalance, 0),
            flagObservationStatus = "E",
            flagMethod = "h"
          )
          ] 
        
      } 
        
          
      for (j in 1:10) {

        # Recalculate imbalance
        calculateImbalance(data_level)
        
        ###    data_level[,
        ###      mov_share_rebased := mov_share / sum(mov_share[Protected == FALSE], na.rm = TRUE),
        ###      by = list(geographicAreaM49, timePointYears, measuredItemSuaFbs)
        ###    ]
        
        ###    # Assign remaining imbalance proportionally, using rebased moving shares.
        ###    data_level[
        ###      Protected == FALSE & food_resid == FALSE & outside(imbalance, -100, 100) & !(measuredElementSuaFbs %chin% c('production', 'imports', 'exports', 'stockChange')),
        ###      `:=`(
        ###        Value = Value + mov_share_rebased * imbalance,
        ###        flagObservationStatus = "E",
        ###        flagMethod = "u"
        ###      )
        ###    ]
        
        
        if (nrow(data_level[outside(imbalance, -1, 1)]) > 0) {
          
          data_level_no_imbalance <- data_level[data.table::between(imbalance, -1, 1)]
          data_level_with_imbalance <- data_level[outside(imbalance, -1, 1)]
          
          levels_to_optimize <- unique(data_level_with_imbalance[, .(geographicAreaM49, timePointYears, measuredItemSuaFbs)])
          
          D_adj <- list()
          
          for (i in 1:nrow(levels_to_optimize)) {
            #print(i) ; flush.console()
            # FIXME: remove this (ugly) global assignment
            x <<- data_level_with_imbalance[levels_to_optimize[i], on = c('geographicAreaM49', 'timePointYears', 'measuredItemSuaFbs')]
            
            if (BALANCING_METHOD == "proportional") {
              x[, adjusted_value := balance_proportional(x)]
            } else if (BALANCING_METHOD == "optimization") {
              x[, adjusted_value := balance_optimization(x)]
            } else {
              stop("Invalid balancing method.")
            }
            
            x[
              !is.na(adjusted_value) & adjusted_value != Value,
              `:=`(
                Value = adjusted_value,
                flagObservationStatus = "E",
                flagMethod = "-"
              )
              ]
            
            D_adj[[i]] <- x
            
            rm(x)
          }
          
          data_level_with_imbalance <- rbindlist(D_adj)
          
          data_level_with_imbalance[, adjusted_value := NULL]
          
          data_level <- rbind(data_level_with_imbalance, data_level_no_imbalance)
          
        }
      }
      
      
      # At this point the imbalance (in the best case scenario) be zero, the following re-calculation is useful only for debugging
      
      calculateImbalance(data_level)
      
      
  if (NEW_STOCKS_POSITION==TRUE){   
      # Try to assign the maximum of imbalance to stocks
      
      data_level[,
                 Value_0 := ifelse(is.na(Value), 0, Value)
                 ][
                   outside(imbalance, -1, 1) & measuredElementSuaFbs == "stockChange" & stockable == TRUE,
                   change_stocks :=
                     # The numbers indicate the case. Assignmnet (value and flags) will be done below
                     case_when(
                       # case 1: we don't want stocks to change sign.
                       sign(Value_0) * sign(Value_0 + imbalance) == -1                                        ~ 1L,
                       # case 2: if value + imbalance takes LESS than opening stock, take all from stocks
                       Value_0 <= 0 & (Value_0 + imbalance <= 0) & abs(Value_0 + imbalance) <=opening_stocks ~ 2L,
                       # case 3: if value + imbalance takes MORE than opening stock, take max opening stocks
                       Value_0 <= 0 & (Value_0 + imbalance <= 0) & abs(Value_0 + imbalance) > opening_stocks  ~ 3L,
                       # case 4: if value + imbalance send LESS than 20% of supply, send all
                       Value_0 >= 0 & (Value_0 + imbalance >= 0) & (Value_0 + imbalance + opening_stocks <= supply * 0.2)      ~ 4L,
                       # case 5: if value + imbalance send MORE than 20% of supply, send 20% of supply
                       Value_0 >= 0 & (Value_0 + imbalance >= 0) & (Value_0 + imbalance + opening_stocks > supply * 0.2)       ~ 5L
                     )
                   ]
      
      data_level[change_stocks == 1L, Value := 0]
      data_level[change_stocks == 2L, Value := Value_0 + imbalance]
      data_level[change_stocks == 3L, Value := -opening_stocks]
      data_level[change_stocks == 4L, Value := Value_0 + imbalance]
      data_level[change_stocks == 5L, Value := ifelse(opening_stocks < supply * 0.2,supply * 0.2-opening_stocks,0)]
 
      data_level[change_stocks %in% 1L:5L, `:=`(flagObservationStatus = "E", flagMethod = "s")]
        
      data_level[, Value_0 := NULL]
  }     
      # Recalculate imbalance
      calculateImbalance(data_level)
      # Assign imbalance to food if food "only" (not "residual") item
      if(NEW_FOOD_RESIDUAL==TRUE){
        data_level[
          Protected == FALSE & food_resid == TRUE & outside(imbalance, -1, 1) & measuredElementSuaFbs == "food",
          `:=`(
            Value = ifelse(is.na(Value) & imbalance>0,imbalance,ifelse(Value + imbalance >= 0, Value + imbalance, 0)),
            flagObservationStatus = "E",
            flagMethod = "h"
          )
          ]
      }else{
        
        data_level[
          Protected == FALSE & food_resid == TRUE & outside(imbalance, -100, 100) & measuredElementSuaFbs == "food",
          `:=`(
            Value = ifelse(Value + imbalance >= 0, Value + imbalance, 0),
            flagObservationStatus = "E",
            flagMethod = "h"
          )
          ] 
        
      } 
      
      
      # Recalculate imbalance
      calculateImbalance(data_level)
      
      if(RESIDUAL_BALANCING==TRUE){
      
      #Assign the residual imbalance to industrial if the conditions are met
      data_level[
        Protected == FALSE & industrial_resid == TRUE & outside(imbalance, -1, 1) & measuredElementSuaFbs == "industrial",
        `:=`(
          Value = ifelse(is.na(Value)&imbalance>0,imbalance,ifelse(Value + imbalance >= 0, Value + imbalance, Value)),
          flagObservationStatus = "E",
          flagMethod = "b"
        )
        ]
      # Recalculate imbalance
      calculateImbalance(data_level)
      
      #Assign the residual imbalance to feed if the conditions are met
      data_level[
        Protected == FALSE & feed_resid == TRUE & outside(imbalance, -1, 1) & measuredElementSuaFbs == "feed",
        `:=`(
          Value = ifelse(is.na(Value)&imbalance>0,imbalance,ifelse(Value + imbalance >= 0, Value + imbalance, Value)),
          flagObservationStatus = "E",
          flagMethod = "b"
        )
        ]
     
      calculateImbalance(data_level)
      } 
      
      
      # Now, let's calculate food processing
      # TODO: change name of `x_to_food`
      x_to_food <-
        data_level[
          measuredElementSuaFbs == 'production',
          list(geographicAreaM49, timePointYears, measuredItemSuaFbs, Value)
          ][
            computed_shares,
            on = c('geographicAreaM49' = 'geographicAreaM49',
                   'timePointYears' = 'timePointYears',
                   'measuredItemSuaFbs' = 'measuredItemChildCPC'),
            nomatch = 0
            ]
      
      food_proc_table <-
        tree[
          measuredElementSuaFbs == 'extractionRate',
          list(measuredItemParentCPC, measuredItemSuaFbs = measuredItemChildCPC, extractionRate = Value)
          ][
            x_to_food,
            on = c('measuredItemParentCPC', 'measuredItemSuaFbs')
            ][,
              zero_weight := measuredItemSuaFbs %in% zeroWeight
              ][,
                food_proc_i := Value / extractionRate * shareDownUp * !zero_weight
                ][,
                  list(food_proc = sum(food_proc_i)),
                  by = list(geographicAreaM49, timePointYears, measuredItemSuaFbs = measuredItemParentCPC)
                  ]
      
      # XXX ??? (Reported by Irina, Angola 0111 (it was assigned as zero in level 1, but it should have been in level 0))
      food_proc_table <- food_proc_table[food_proc > 0]
      
      
      data_level[, c("supply", "utilizations", "imbalance", "mov_share_rebased") := NULL]
      
      data <- rbind(data[processingLevel != lev], data_level)
      
    }
    
    data[, c("processingLevel", "min_processingLevel") := NULL]
  } else {
    primaryEl <- c()
  }
  
  return(data)
}




############################## / FUNCTIONS #################################











print("NEWBAL: end functions")





















library(faosws)
library(faoswsUtil)
library(faoswsBalancing)
library(faoswsStandardization)
library(dplyr)
library(data.table)
library(tidyr)


#  32 = Argentina
#  76 = Brazil
# 124 = Canada
# 144 = Sri Lanka
# 324 = Guinea
# 454 = Malawi
# 643 = Russia
# 710 = south africa

if (CheckDebug()) {
  COUNTRY <- "840"
} else {
  COUNTRY <- swsContext.datasets[[1]]@dimensions$geographicAreaM49@keys
  #COUNTRY <- swsContext.computationParams$country
}

print("NEWBAL: parameters")

if (CheckDebug()) {
  mydir <- "modules/newBalancing_test"
  
  SETTINGS <- faoswsModules::ReadSettings(file.path(mydir, "sws.yml"))
  
  SetClientFiles(SETTINGS[["certdir"]])
  
  GetTestEnvironment(baseUrl = SETTINGS[["server"]], token = SETTINGS[["token"]])
  R_SWS_SHARE_PATH = "//hqlprsws1.hq.un.fao.org/sws_r_share"
}

STOP_AFTER_DERIVED <- as.logical(swsContext.computationParams$stop_after_derived)

#BALANCING_METHOD <- swsContext.computationParams$balancing_method
BALANCING_METHOD <- "proportional"

#THRESHOLD_METHOD <- swsContext.computationParams$threshold_method
THRESHOLD_METHOD <- 'share'

#FIX_OUTLIERS <- as.logical(swsContext.computationParams$fix_outliers)
FIX_OUTLIERS <- TRUE

NEW_THRESHOLDS <- as.logical(swsContext.computationParams$new_thresholds)
#NEW_THRESHOLDS<-TRUE

#NEW_STOCKS_POSITION <- as.logical(swsContext.computationParams$new_stocks_position)
NEW_STOCKS_POSITION <- TRUE

#NEW_FOOD_RESIDUAL <- as.logical(swsContext.computationParams$new_food_residual)
NEW_FOOD_RESIDUAL <- TRUE

#FILL_EXTRACTION_RATES<-as.logical(swsContext.computationParams$fill_extraction_rates)
FILL_EXTRACTION_RATES <- TRUE

#RESIDUAL_BALANCING <- as.logical(swsContext.computationParams$residual_balancing)
RESIDUAL_BALANCING <- TRUE

YEARS <- as.character(2000:2017)

R_SWS_SHARE_PATH <- Sys.getenv("R_SWS_SHARE_PATH")
if (CheckDebug()) {
  R_SWS_SHARE_PATH = "//hqlprsws1.hq.un.fao.org/sws_r_share"
}

params <- defaultStandardizationParameters()

params$itemVar <- "measuredItemSuaFbs"
params$mergeKey[params$mergeKey == "measuredItemCPC"] <- "measuredItemSuaFbs"
params$elementVar <- "measuredElementSuaFbs"
params$childVar <- "measuredItemChildCPC"
params$parentVar <- "measuredItemParentCPC"
params$createIntermetiateFile <- "TRUE"
params$protected <- "Protected"
params$official <- "Official"

p <- params




#####################################  TREE #################################

print("NEWBAL: download tree")

tree <- getCommodityTreeNewMethod(COUNTRY, YEARS)

stopifnot(nrow(tree) > 0)

tree <- tree[geographicAreaM49 %chin% COUNTRY]

validateTree(tree)

## NA ExtractionRates are recorded in the sws dataset as 0
## for the standardization, we nee them to be treated as NA
## therefore here we are re-changing it

tree[Value == 0, Value := NA]

tree_to_send <- tree[is.na(Value) & measuredElementSuaFbs=="extractionRate"]

if (FILL_EXTRACTION_RATES == TRUE) {

  expanded_tree <-
    merge(
      data.table(
        geographicAreaM49 = unique(tree$geographicAreaM49),
        timePointYears = as.character(min(tree$timePointYears):max(tree$timePointYears))
      ),
      unique(tree[, .(geographicAreaM49, measuredElementSuaFbs,
                      measuredItemParentCPC, measuredItemChildCPC)]),
      by = "geographicAreaM49",
      all = TRUE,
      allow.cartesian = TRUE
    )

  tree <- tree[expanded_tree, on = colnames(expanded_tree)]

  tree <-
    tree %>%
    group_by(geographicAreaM49, measuredElementSuaFbs, measuredItemParentCPC, measuredItemChildCPC) %>%
    arrange(geographicAreaM49, measuredElementSuaFbs, measuredItemParentCPC, measuredItemChildCPC) %>%
    tidyr::fill(Value, flagObservationStatus, flagMethod, .direction = "up") %>%
    tidyr::fill(Value, flagObservationStatus, flagMethod, .direction = "down") %>%
    setDT()
}

# TODO: add explicit columns names in joins
tree_to_send <-
  tree_to_send %>% 
  dplyr::anti_join(tree[is.na(Value) & measuredElementSuaFbs=="extractionRate"]) %>%
  dplyr::select(-Value) %>%
  dplyr::left_join(tree) %>%
  setDT()

tree_to_send <-
  tree_to_send[,
    .(geographicAreaM49, measuredElementSuaFbs, measuredItemParentCPC,
      measuredItemChildCPC, timePointYears, Value, flagObservationStatus, flagMethod)]

setnames(
  tree_to_send,
  c("measuredItemParentCPC", "measuredItemChildCPC"),
  c("measuredItemParentCPC_tree", "measuredItemChildCPC_tree")
)

tree_to_send <-
  nameData("suafbs", "ess_fbs_commodity_tree2", tree_to_send, except = c('measuredElementSuaFbs', 'timePointYears'))

tree_to_send[,
  `:=`(
    measuredItemParentCPC_tree = paste0("'", measuredItemParentCPC_tree),
    measuredItemChildCPC_tree = paste0("'", measuredItemChildCPC_tree))
]

tmp_file_name_extr <- tempfile(pattern = paste0("FILLED_ER_", COUNTRY, "_"), fileext = '.csv')

write.csv(tree_to_send, tmp_file_name_extr)

# XXX remove NAs
tree <- tree[!is.na(Value)]

### Update tree by setting some edges to "F"
#FPCommodities <- c("01499.06", "01921.01")
#
## These commodities are forwards processed instead of backwards processed:
##        code             description type
## 3: 01499.06      Kapokseed in shell CRNP
## 4: 01921.01   Seed cotton, unginned CRPR
#
#tree[, target := ifelse(measuredItemParentCPC %in% FPCommodities, "F", "B")]

uniqueLevels <- unique(tree[, list(geographicAreaM49, timePointYears)])

levels <- list()

treeLevels <- list()

for (i in seq_len(nrow(uniqueLevels))) {
  filter <- uniqueLevels[i, ]
  
  treeCurrent <- tree[filter, on = c("geographicAreaM49", "timePointYears")]
  
  levels <- findProcessingLevel(treeCurrent, "measuredItemParentCPC", "measuredItemChildCPC")
  setnames(levels, "temp", "measuredItemParentCPC")
  
  treeLevels[[i]] <- merge(treeCurrent, levels, by = c("measuredItemParentCPC"), all.x = TRUE)
}

tree <- rbindlist(treeLevels)

# XXX there are no different process levels, but check it
tree[,
     processingLevel := max(processingLevel, na.rm = TRUE),
     by = c("geographicAreaM49", "timePointYears",
            "measuredElementSuaFbs", "measuredItemChildCPC")
     ]



# XXX Check if this one is still good or it can be obtained within the dataset
processed_item_datatable <- ReadDatatable("processed_item")
processedCPC <- processed_item_datatable[, measured_item_cpc]


# XXX what is this for?
itemMap <- GetCodeList(domain = "agriculture", dataset = "aproduction", "measuredItemCPC")
itemMap <- itemMap[, list(measuredItemSuaFbs = code, type)]

##################################### / TREE ################################











############################ POPULATION #########################################

key <-
  DatasetKey(
    domain = "population",
    dataset = "population_unpd",
    dimensions =
      list(
        geographicAreaM49 = Dimension(name = "geographicAreaM49", keys = COUNTRY),
        measuredElementSuaFbs = Dimension(name = "measuredElement", keys = "511"), # 511 = Total population
        timePointYears = Dimension(name = "timePointYears", keys = as.character(2000:2017))
      )
  )

print("NEWBAL: download population")

popSWS <- GetData(key)

stopifnot(nrow(popSWS) > 0)

popSWS[geographicAreaM49 == "156", geographicAreaM49 := "1248"]


############################ / POPULATION #########################################























############################################################### DATA ############################################################




# 5510 Production[t]
# 5610 Import Quantity [t]
# 5071 Stock Variation [t]
# 5023 Export Quantity [t]
# 5910 Loss [t]
# 5016 Industrial uses [t]
# 5165 Feed [t]
# 5520 Seed [t]
# 5525 Tourist Consumption [t]
# 5164 Residual other uses [t]
# 5141 Food [t]
# 664 Food Supply (/capita/day) [Kcal]

elemKeys <- c("5510", "5610", "5071", "5113", "5023", "5910", "5016",
              "5165", "5520", "5525", "5164", "5166", "5141")

itemKeys <- GetCodeList(domain = "suafbs", dataset = "sua_unbalanced", "measuredItemFbsSua")
itemKeys <- itemKeys$code


# NOTE: the definition of "food_resid" changed (see inside newBalancing)
# (so, the tables below are not used anymore)

#food_classification_country_specific <-
#  ReadDatatable("food_classification_country_specific",
#                where = paste0("geographic_area_m49 IN ('", COUNTRY, "')"))
#
#food_classification_country_specific <-
#  food_classification_country_specific[geographic_area_m49 == COUNTRY]
#
#food_only_items <- food_classification_country_specific[food_classification == 'Food Residual', measured_item_cpc]


Utilization_Table <- ReadDatatable("utilization_table_2018")

stockable_items <- Utilization_Table[stock == 'X', cpc_code]

zeroWeight <- ReadDatatable("zero_weight")[, item_code]

flagValidTable <- ReadDatatable("valid_flags")

# XXX: we decided to unprotect E,e (replaced outliers)
flagValidTable[flagObservationStatus == 'E' & flagMethod == 'e', Protected := FALSE]

# utilizationTable=ReadDatatable("utilization_table")
utilizationTable <-
  ReadDatatable(
    "utilization_table_percent",
    where = paste0("area_code IN (", paste(shQuote(COUNTRY, type = "sh"), collapse = ", "), ")")
  )

setnames(utilizationTable, colnames(utilizationTable), c("geographicAreaM49", "measuredElementSuaFbs", "measuredItemSuaFbs",
                                                         "percent", "rank", "rankInv"))
nutrientData <-
  getNutritiveFactors(
    measuredElement = "1001", # "1001" is code for calories per 100 grams
    timePointYears = as.character(2014:2017),
    geographicAreaM49 = COUNTRY
  )



if (CheckDebug()) {
  key <-
    DatasetKey(
      domain = "suafbs",
      dataset = "sua_unbalanced",
      dimensions =
        list(
          geographicAreaM49 = Dimension(name = "geographicAreaM49", keys = COUNTRY),
          measuredElementSuaFbs = Dimension(name = "measuredElementSuaFbs", keys = elemKeys),
          measuredItemFbsSua = Dimension(name = "measuredItemFbsSua", keys = itemKeys),
          timePointYears = Dimension(name = "timePointYears", keys = YEARS)
        )
    )
} else {
  key <- swsContext.datasets[[2]]
  
  key@dimensions$timePointYears@keys <- YEARS
  key@dimensions$measuredItemFbsSua@keys <- itemKeys
  key@dimensions$measuredElementSuaFbs@keys <- elemKeys
  key@dimensions$geographicAreaM49@keys <- COUNTRY
}

#       # If need to read from session:
#       GetTestEnvironment(baseUrl = SETTINGS[["server"]], token = 'edfe83e1-47d8-499d-88fa-35e6a9dca850')
#
#       key <- swsContext.datasets[[2]]
#
#       key@dimensions$timePointYears@keys <- YEARS
#       key@dimensions$measuredItemFbsSua@keys <- itemKeys
#       key@dimensions$measuredElementSuaFbs@keys <- elemKeys
#       key@dimensions$geographicAreaM49@keys <- COUNTRY



print("NEWBAL: download data")


data <- GetData(key)
# LOAD
# data <- readRDS(paste0('c:/Users/mongeau.FAODOMAIN/tmp/new_balancing/data_', COUNTRY, '.rds'))









data <- merge(data, flagValidTable, by = c("flagObservationStatus", "flagMethod"), all.x = TRUE)

data[flagObservationStatus %in% c("", "T"), `:=`(Official = TRUE, Protected = TRUE)]
data[is.na(Official), Official := FALSE]
data[is.na(Protected), Protected := FALSE]


# FIXME: elementCodesToNames do not set the "opening_stocks" variable name for element 5113
data[,
     `:=`(
       opening_stocks = Value[measuredElementSuaFbs == "5113"],
       opening_stocks_protected = Protected[measuredElementSuaFbs == "5113"],
       opening_stocks_official = Official[measuredElementSuaFbs == "5113"]
     ),
     by = c("geographicAreaM49", "timePointYears", "measuredItemFbsSua")
     ]

data <- data[measuredElementSuaFbs != "5113"]

print("NEWBAL: elementToCodeNames")

data <-
  elementCodesToNames(
    data,
    itemCol = "measuredItemFbsSua",
    elementCol = "measuredElementSuaFbs"
  )


# XXX: there are some NAs here, but probably there shouldn't
data <- data[!is.na(measuredElementSuaFbs)]

setnames(data, "measuredItemFbsSua", "measuredItemSuaFbs")

print("NEWBAL: convert sugar")

# XXX
##############################################################
######### SUGAR RAW CODES TO BE CONVERTED IN 2351F ###########
##############################################################
data <- convertSugarCodes(data)

## XXX: check for max processing level
#level <- findProcessingLevel(tree, from = params$parentVar,
#                            to = params$childVar, aupusParam = params)


#primaryEl <- level[processingLevel == 0, get(params$itemVar)]

## XXX what is this?
#data[
#  !(get(params$protected) == TRUE | (flagObservationStatus == "I" & flagMethod %in% c("i", "e"))) &
#    get(params$elementVar) == params$productionCode &
#    !(get(params$itemVar) %chin% primaryEl),
#  Value_xxx := NA
#]


#data[,
#  availability :=
#    sum(
#      Value[get(p$elementVar) %in% c(p$productionCode, p$importCode)],
#      - Value[get(p$elementVar) %in% p$exportCode],
#      na.rm = TRUE
#    ),
#  by = c(p$mergeKey)
#]




treeRestricted <- tree[, .(measuredItemParentCPC, measuredItemChildCPC, processingLevel)]
treeRestricted <- unique(treeRestricted[order(measuredItemChildCPC)])

primaryInvolved <- faoswsProduction::getPrimary(processedCPC, treeRestricted, params)

print("NEWBAL: primary involved descendents")

# XXX: check here, 0111 is in results
primaryInvolvedDescendents <-
  getChildren(
    commodityTree = treeRestricted,
    parentColname = "measuredItemParentCPC",
    childColname = "measuredItemChildCPC",
    topNodes = primaryInvolved
  )


# stocks need to be generated for all (XXX if stockable)

# FIXME: when it exists, we will not have stocks backwards...
# see, e.g, data[geographicAreaM49 == 124 & measuredItemSuaFbs == '01445' & timePointYears == 2004]

items_to_generate_stocks <-
  setdiff(
    # all ...
    unique(intersect(c(data$measuredItemSuaFbs, primaryInvolvedDescendents), stockable_items)),
    # ... except those for which already available
    unique(data[measuredElementSuaFbs == 'stock_change' & flagObservationStatus == "T" & flagMethod == "p"]$measuredItemSuaFbs)
  )

stock <-
  CJ(
    measuredItemSuaFbs    = items_to_generate_stocks,
    measuredElementSuaFbs = 'stock_change',
    geographicAreaM49     = unique(data$geographicAreaM49),
    timePointYears        = unique(data$timePointYears)
  )

# rbind with anti_join
data <-
  rbind(
    data,
    stock[!data, on = c('measuredItemSuaFbs', 'measuredElementSuaFbs',
                        'geographicAreaM49', 'timePointYears')],
    fill = TRUE
  )

# XXX what is primaryInvolvedDescendents ?????????
#deriv <- CJ(measuredItemSuaFbs = primaryInvolvedDescendents, measuredElementSuaFbs = 'production', geographicAreaM49 = unique(data$geographicAreaM49), timePointYears = unique(data$timePointYears))
deriv <-
  CJ(
    measuredItemSuaFbs    = unique(tree$measuredItemChildCPC),
    measuredElementSuaFbs = 'production',
    geographicAreaM49     = unique(data$geographicAreaM49),
    timePointYears        = unique(data$timePointYears)
  )

# rbind with anti_join
data <-
  rbind(
    data,
    deriv[!data, on = c('measuredItemSuaFbs', 'measuredElementSuaFbs',
                        'geographicAreaM49', 'timePointYears')],
    fill = TRUE
  )



data[is.na(Official), Official := FALSE]
data[is.na(Protected), Protected := FALSE]


# XXX When `x` enters here it should be already ordered by year
fix_stocks <- function(x) {
  # The following fix works only in cases like:
  #   geographicAreaM49 timePointYears measuredItemSuaFbs delta opening_stocks
  #1:               643           2014           01919.93     0      133800000
  #2:               643           2015           01919.93    NA             NA
  #3:               643           2016           01919.93    NA             NA
  #4:               643           2017           01919.93    NA             NA
  #
  # In more general cases, it's not valid
  x$delta[is.na(x$delta)] <- 0
  x$opening_stocks[is.na(x$opening_stocks)] <- 0
  
  x$delta_updated <- x$delta
  
  if (nrow(x) > 1) {
    for (i in 2:nrow(x)) {
      
      x$opening_stocks[i] <- x$opening_stocks[i-1] + x$delta_updated[i-1]
      
      if (dplyr::near(x$opening_stocks[i], 0) & x$delta_updated[i] < 0) {
        x$delta_updated[i] <- 0
      } else if (x$opening_stocks[i] < 0) {
        x$opening_stocks[i] <- 0
        x$delta_updated[i] <- x$opening_stocks[i-1]
      }
    }
  }
  
  return(x)
}


data[, stockable := measuredItemSuaFbs %chin% stockable_items]


# XXX: Remove items for which no production, imports or exports exist after 2013.
non_existing_for_imputation <-
  setdiff(
    data[measuredElementSuaFbs %in% c('production', 'imports', 'exports') & timePointYears <= 2013, unique(measuredItemSuaFbs)],
    data[measuredElementSuaFbs %in% c('production', 'imports', 'exports') & timePointYears >= 2014, unique(measuredItemSuaFbs)]
  )

data <- data[!(measuredItemSuaFbs %in% non_existing_for_imputation)]


############################################### Create derived production ###################################


# Tables that will be required by the co-product issue (fix processingShare)

coproduct_table <- ReadDatatable('zeroweight_coproducts')

stopifnot(nrow(coproduct_table) > 0)

coproduct_table <- coproduct_table[, .(measured_item_child_cpc, branch)]

setnames(coproduct_table, "measured_item_child_cpc", "measuredItemChildCPC")

# Can't do anything if this information if missing, so remove these cases
coproduct_table <- coproduct_table[!is.na(branch)]

### XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
### XXX removing these cases as '22242.01' appears as zero-weight XXXX
### XXX for main product = '22110.04'                             XXXX
### XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
coproduct_table <- coproduct_table [branch != '22242.01 + 22110.04']


coproduct_table_easy <- coproduct_table[!grepl('\\+|or', branch)]

coproduct_table_easy <-
  rbind(
    coproduct_table_easy,
    coproduct_table_easy[, .(measuredItemChildCPC = branch, branch)]
  )

coproduct_table_easy <- unique(coproduct_table_easy)

coproduct_table_plus <- coproduct_table[grepl('\\+', branch)]

coproduct_table_plus <-
  rbind(
    coproduct_table_plus,
    tidyr::separate(coproduct_table_plus, branch, into = c('main1', 'main2'), remove = FALSE, sep = ' *\\+ *')[, .(measuredItemChildCPC = main1, branch)],
    tidyr::separate(coproduct_table_plus, branch, into = c('main1', 'main2'), remove = FALSE, sep = ' *\\+ *')[, .(measuredItemChildCPC = main2, branch)]
  )

coproduct_table_plus <- unique(coproduct_table_plus)


coproduct_table_or <- coproduct_table[grepl('or', branch)]

coproduct_table_or <-
  rbind(
    coproduct_table_or,
    tidyr::separate(coproduct_table_or, branch, into = c('main1', 'main2'), remove = FALSE, sep = ' *or *')[, .(measuredItemChildCPC = main1, branch)],
    tidyr::separate(coproduct_table_or, branch, into = c('main1', 'main2'), remove = FALSE, sep = ' *or *')[, .(measuredItemChildCPC = main2, branch)]
  )

coproduct_table_or <- unique(coproduct_table_or)

# / Tables that will be required by the co-product issue (fix processingShare)



# Keep processingShare and shareDownUp
computed_shares <- list()
computed_shares_send <- list()
# Keep negative availability
negative_availability <- list()

fixed_proc_shares <- list()

data_stock_1 <- vector(mode = "list", length = length(unique(tree$processingLevel)))

print("NEWBAL: starting derived production loop")

if (length(primaryInvolvedDescendents) == 0) {
  message("No primary commodity involved in this country")
} else {
  for (lev in sort(unique(tree$processingLevel))) {
    
    treeCurrentLevel <-
      tree[
        !is.na(Value) &
          processingLevel == lev &
          measuredElementSuaFbs == 'extractionRate'
        ][,
          list(
            measuredItemParentCPC,
            geographicAreaM49,
            measuredItemChildCPC,
            timePointYears,
            extractionRate = Value
          )
          ]
    
    # Stocks will be generated also for those cases for which we have data
    condition_for_stocks <-
      data$stockable == TRUE &
      data$measuredItemSuaFbs %chin% items_to_generate_stocks &
      data$measuredItemSuaFbs %chin% treeCurrentLevel$measuredItemParentCPC &
      data$measuredElementSuaFbs %chin% c('production', 'imports', 'exports')
    
    if (any(condition_for_stocks)) {
      
      # XXX These are fake opening stocks
      fake_opening_stocks <-
        data[
          condition_for_stocks == TRUE,
          list(timePointYears, measuredElementSuaFbs, Value),
          by = c('geographicAreaM49', 'measuredItemSuaFbs')
          ][,
            list(
              supply =
                sum(
                  Value[measuredElementSuaFbs %in% c('production', 'imports')],
                  -Value[measuredElementSuaFbs == 'exports'],
                  na.rm = TRUE
                )
            ),
            by = c('geographicAreaM49', 'measuredItemSuaFbs', 'timePointYears')
            ][
              # XXX check if this is right (these cases shouldn't happen)
              # In this case opening_stocks will be zero and in the condition of negative delta being less than
              supply < 0,
              supply := 0
              ][
                order(geographicAreaM49, measuredItemSuaFbs, timePointYears)
                ][,
                  min_year := timePointYears == min(timePointYears),
                  by = c("geographicAreaM49", "measuredItemSuaFbs")
                  ][
                    min_year == TRUE, opening_stocks := ifelse(!is.na(supply), supply, 0)
                    ]
      
      
      # generate stocks for parents
      data_for_stocks <-
        data[
          condition_for_stocks == TRUE
          ][,
            list(
              supply =
                sum(
                  Value[measuredElementSuaFbs %in% c('production', 'imports')],
                  - Value[measuredElementSuaFbs == 'exports'],
                  na.rm = TRUE
                )
            ),
            by = c("measuredItemSuaFbs", "geographicAreaM49", "timePointYears")
            ][
              order(measuredItemSuaFbs, geographicAreaM49, timePointYears)
              ][,
                delta := supply - RcppRoll::roll_mean(supply, 2, fill = 'extend', align = 'right')
                ][,
                  supply := NULL
                  ]
      
      data_for_stocks <-
        merge(
          data_for_stocks,
          fake_opening_stocks,
          by = c('geographicAreaM49', 'measuredItemSuaFbs', 'timePointYears'),
          all = TRUE
        )
      
      data_for_stocks[,
                      min_year := timePointYears == min(timePointYears),
                      by = c("geographicAreaM49", "measuredItemSuaFbs")
                      ]
      
      # To avoid degenerate cases, we will suppose that in the first year delta stocks are zero
      data_for_stocks[min_year == TRUE, delta := 0]
      
      # If generated delta is greater than supply, set it to 20% of supply
      data_for_stocks[delta > 0 & delta > 0.2 * supply, delta := 0.2 * supply]
      
      # Fix stocks
      data_for_stocks <-
        plyr::ddply(
          data_for_stocks,
          .variables = c('geographicAreaM49', 'measuredItemSuaFbs'),
          .fun = function(x) fix_stocks(x)
        )
      
      setDT(data_for_stocks)
      
      data_for_stocks <-
        data_for_stocks[,
                        list(
                          geographicAreaM49,
                          measuredItemSuaFbs,
                          timePointYears,
                          opening_stocks,
                          delta = delta_updated)
                        ]
      
      data <-
        merge(
          data,
          data_for_stocks,
          by = c('geographicAreaM49', 'timePointYears', 'measuredItemSuaFbs'),
          all.x = TRUE
        )
      
      # This should always happen, but just in case
      if ("opening_stocks.x" %in% names(data)) {
        data[, opening_stocks := ifelse(!is.na(opening_stocks.x), opening_stocks.x, opening_stocks.y)]
        data[, c("opening_stocks.x", "opening_stocks.y") := NULL]
      }
      
      # Assign if non-protected
      # XXX I was told to save only for 2014 onwards (non-fozen data)
      data[
        timePointYears >= 2014 &
          Protected == FALSE &
          measuredElementSuaFbs == 'stock_change' &
          measuredItemSuaFbs %in% data_for_stocks$measuredItemSuaFbs,
        `:=`(
          Value = delta,
          flagObservationStatus = "E",
          flagMethod = "u"
        )
        ][,
          delta := NULL
          ]
      
    }
    
    data_stock_1[[lev +1]] <- copy(data)
    
    
    
    dataMergeTree <- data[measuredElementSuaFbs %chin% c('production', 'imports', 'exports', 'stock_change')]
    
    setnames(dataMergeTree, "measuredItemSuaFbs", "measuredItemParentCPC")
    
    dataMergeTree <-
      merge(
        dataMergeTree,
        treeCurrentLevel,
        by = c(params$parentVar, params$geoVar, params$yearVar),
        allow.cartesian = TRUE
      )
    
    dataMergeTree[,
                  availability :=
                    sum(
                      Value[get(p$elementVar) %in% c(p$productionCode, p$importCode)],
                      # XXX p$stockCode is "stockChange", not "stock_change"
                      - Value[get(p$elementVar) %in% c(p$exportCode, "stock_change")],
                      na.rm = TRUE
                    ),
                  by = c(params$geoVar, params$yearVar, params$parentVar, params$childVar)
                  ]
    
    negative_availability[[as.character(lev)]] <-
      unique(
        dataMergeTree[
          availability < -100,
          list(
            country            = geographicAreaM49,
            year               = timePointYears,
            measuredItemFbsSua = measuredItemParentCPC,
            element            = measuredElementSuaFbs,
            Value,
            flagObservationStatus,
            flagMethod,
            availability
          )
          ]
      )
    
    # XXX: Replace negative availability, so we get zero production, instead of negative. This, , however, should be fixed in advance, somehow.
    dataMergeTree[availability < 0, availability := 0]
    
    dataMergeTree[,
                  availabilitieChildEquivalent := availability * extractionRate #,
                  #by = c(params$geoVar, params$yearVar, params$parentVar, params$childVar, "measuredElementSuaFbs")
                  ]
    
    # XXX I commented out measuredElementSua because it makes sense only if
    # the number of elements by parent for an item is the same. For instance,
    # if a parent 1 has 3 elements and parent 2 has 4 elements (the same 3
    # as parent 1 plus stocks) it will give a different sum of utilizations
    # for parent 2 and fourth element
    # 
    # It was with a [[1]]
    #dataMergeTree[, sumAvail := sum(sort(unique(availabilitieChildEquivalent), na.last = TRUE)[[1]]), by = c(params$childVar, params$yearVar, params$geoVar)] #, "measuredElementSuaFbs")]
    # Fixed:
    #dataMergeTree[, sumAvail := sum(sort(unique(availabilitieChildEquivalent), na.last = TRUE)), by = c(params$childVar, params$yearVar, params$geoVar)] #, "measuredElementSuaFbs")]
    # Better (set to NA when zero?):
    dataMergeTree[,
                  sumAvail := sum(unique(na.omit(availabilitieChildEquivalent))),
                  by = c(params$childVar, params$yearVar, params$geoVar)  #, "measuredElementSuaFbs"
                  ]
    
    dataMergeTree[, shareDownUp := availabilitieChildEquivalent / sumAvail]
    
    ## XXX check these cases
    #dataMergeTree[shareDownUp > 1]
    #dataMergeTree[shareDownUp < 0]
    #dataMergeTree[is.nan(shareDownUp)]
    
    # XXX for now:
    dataMergeTree[shareDownUp > 1, shareDownUp := 1]
    dataMergeTree[shareDownUp < 0, shareDownUp := 0]
    dataMergeTree[is.nan(shareDownUp), shareDownUp := 0]
    # 
    # #correcting ShareDownUp
    # dataMergeTree[,Nparent:=uniqueN(measuredItemParentCPC),
    #               by = c('geographicAreaM49', 'timePointYears', 'measuredItemChildCPC')][,`:=`(shareDownUp=ifelse(Nparent==1,1,shareDownUp),
    #                                                                                            Nparent=NULL)]
    # Key here was implicitly set by a previous order()
    setkey(dataMergeTree, NULL)
    
    dataMergeTree <-
      unique(
        dataMergeTree[,
                      list(geographicAreaM49, timePointYears, measuredItemParentCPC,
                           measuredItemChildCPC, extractionRate, availability, shareDownUp)
                      ]
      )
    
    dataMergeTree <-
      merge(
        dataMergeTree,
        data[
          measuredElementSuaFbs == 'production',
          list(geographicAreaM49, timePointYears,
               measuredItemChildCPC = measuredItemSuaFbs, Value, Protected)
          ],
        by = c('geographicAreaM49', 'timePointYears', 'measuredItemChildCPC')
      )
    
    dataMergeTree[timePointYears >= 2014 & Protected == FALSE, Value := NA][, Protected := NULL]
    
    dataMergeTree <- dataMergeTree[, processingShare := Value / extractionRate * shareDownUp / availability]
    
    # Fix weird processing shares. This should in theory never happen, but it does in some cases.
    dataMergeTree[processingShare < 0, processingShare := 0]
    dataMergeTree[processingShare > 1, processingShare := 1]
    
    dataMergeTree <-
      dataMergeTree[
        order(geographicAreaM49, measuredItemParentCPC, measuredItemChildCPC, timePointYears)
      ][,
        processingShare_avg := rollavg(processingShare, order = 3),
        by = c("geographicAreaM49", "measuredItemParentCPC", "measuredItemChildCPC")
      ]
    
    # Key here was implicitly set by a previous order()
    setkey(dataMergeTree, NULL)
    
    #dataMergeTree[, nonna := sum(!is.na(processingShare)), list(geographicAreaM49, measuredItemParentCPC, measuredItemChildCPC)]
    
    dataMergeTree[is.na(processingShare), processingShare := processingShare_avg]
    
    dataMergeTree[, processingShare_avg := NULL]


    ############### Fix processing shares for co-products

    ################################# Easy cases

    if (nrow(coproduct_table_easy) > 0) {

      output_easy <-
        dataMergeTree[
          measuredItemChildCPC %in% unique(coproduct_table_easy$measuredItemChildCPC),
          .(geographicAreaM49, timePointYears, measuredItemChildCPC, measuredItemParentCPC, processingShare)
        ]

      if (nrow(output_easy) > 0) {
        output_easy <-
          merge(output_easy, coproduct_table_easy, by = 'measuredItemChildCPC', all.x = TRUE)

        output_easy[,
          reference_share := fmax(measuredItemChildCPC, branch, processingShare, FALSE),
          by = c("geographicAreaM49", "timePointYears", "branch", "measuredItemParentCPC")
        ]

        output_easy[,
          # Ask whether rounding by 2 is fine
          reimpute := (round(processingShare, 2) != round(reference_share, 2)) | is.na(processingShare),
          by = c("geographicAreaM49", "timePointYears", "branch", "measuredItemParentCPC")
        ]
      }
    }

    ################################# / Easy cases


    ################################# Plus cases


    if (nrow(coproduct_table_plus) > 0) {

      output_plus <-
        dataMergeTree[
          measuredItemChildCPC %in% unique(coproduct_table_plus$measuredItemChildCPC),
          .(geographicAreaM49, timePointYears, measuredItemChildCPC, measuredItemParentCPC, processingShare)
        ]


      if (nrow(output_plus) > 0) {
        output_plus <-
          merge(output_plus, coproduct_table_plus, by = 'measuredItemChildCPC', all.x = TRUE)

         output_plus[,
          reference_share := fmax(measuredItemChildCPC, branch, processingShare, TRUE),
          by = c("geographicAreaM49", "timePointYears", "branch", "measuredItemParentCPC")
        ]

         output_plus[,
          # Ask whether rounding by 2 is fine
          reimpute := (round(processingShare, 2) != round(reference_share, 2)) | is.na(processingShare),
          by = c("geographicAreaM49", "timePointYears", "branch", "measuredItemParentCPC")
        ]
      }
    }

    ################################# / Plus cases


    ################################# Or cases

    if (nrow(coproduct_table_or) > 0) {

      output_or <-
        dataMergeTree[
          measuredItemChildCPC %in% unique(coproduct_table_or$measuredItemChildCPC),
          .(geographicAreaM49, timePointYears, measuredItemChildCPC, measuredItemParentCPC, processingShare)
        ]


      if (nrow(output_or) > 0) {
        output_or <-
          merge(output_or, coproduct_table_or, by = 'measuredItemChildCPC', all.x = TRUE)

        output_or[,
          reference_share := fmax(measuredItemChildCPC, branch, processingShare, TRUE),
          by = c("geographicAreaM49", "timePointYears", "branch", "measuredItemParentCPC")
        ]

        output_or[,
          # Ask whether rounding by 2 is fine
          reimpute := (round(processingShare, 2) != round(reference_share, 2)) | is.na(processingShare),
          by = c("geographicAreaM49", "timePointYears", "branch", "measuredItemParentCPC")
        ]
      }
    }

    ################################# / Or cases


    output_to_check <-
      rbindlist(
        list(
          if (exists("output_easy")) output_easy else NULL,
          if (exists("output_plus")) output_plus else NULL,
          if (exists("output_or"))   output_or   else NULL
        ),
        fill = TRUE
      )

    if (nrow(output_to_check) > 0) {

      output_to_check <-
        output_to_check[
          reimpute == TRUE,
          .(geographicAreaM49, timePointYears, measuredItemParentCPC, measuredItemChildCPC, reference_share)
        ]

      dataMergeTree <-
        merge(
          dataMergeTree,
          output_to_check,
          by = c("geographicAreaM49", "timePointYears", "measuredItemParentCPC", "measuredItemChildCPC"),
          all.x = TRUE
        )

      fixed_proc_shares[[as.character(lev)]] <- dataMergeTree[!is.na(reference_share)]

      dataMergeTree[!is.na(reference_share), processingShare := reference_share]

      dataMergeTree[, reference_share := NULL]
    }


    ############### / Fix processing shares for co-products
    
    dataMergeTree <-
      unique(
        dataMergeTree[,
                      list(geographicAreaM49, timePointYears, measuredItemParentCPC,
                           measuredItemChildCPC, Value, extractionRate, shareDownUp,
                           processingShare, availability)]
      )
    
    dataMergeTree[, new_imputation := availability * processingShare * extractionRate]
    
    computed_shares[[as.character(lev)]] <-
      dataMergeTree[,
                    list(geographicAreaM49, timePointYears, measuredItemParentCPC,
                         measuredItemChildCPC, shareDownUp, processingShare)
                    ]
    
    # XXX: change this name
    computed_shares_send[[as.character(lev)]] <-
      dataMergeTree[,
        list(geographicAreaM49, timePointYears, measuredItemParentCPC,
             measuredItemChildCPC, extractionRate, shareDownUp,
             processingShare, availability_parent = availability)
      ]
    
    dataMergeTree <-
      dataMergeTree[,
                    list(
                      measuredElementSuaFbs = 'production',
                      # NOTE: the sum(unique()) is just a temporary HACK: check how to do it properly
                      imputed_deriv_value = sum(unique(round(new_imputation, 2)), na.rm = TRUE)
                    ),
                    list(geographicAreaM49, timePointYears, measuredItemSuaFbs = measuredItemChildCPC)
                    ]
    
    data <-
      merge(
        data,
        dataMergeTree,
        by = c('geographicAreaM49', 'timePointYears', 'measuredItemSuaFbs', 'measuredElementSuaFbs'),
        all.x = TRUE
      )

    z <- data[
        measuredElementSuaFbs %in% c("production", "imports", "exports", "stock_change"),
        list(geographicAreaM49, timePointYears, measuredItemSuaFbs, measuredElementSuaFbs,
             Value = ifelse(measuredElementSuaFbs == 'production' & Protected == FALSE, imputed_deriv_value, Value),
             Protected)
        ]

    z <- dcast(z, geographicAreaM49+timePointYears+measuredItemSuaFbs~measuredElementSuaFbs, value.var = c("Value", "Protected"))
    
    # XXX stock_change may change below (and also production, if required)

    computed_shares_send[[as.character(lev)]] <-
      z[computed_shares_send[[as.character(lev)]],
        on = c('geographicAreaM49'  = 'geographicAreaM49',
               'timePointYears'     = 'timePointYears',
               'measuredItemSuaFbs' = 'measuredItemChildCPC')]
    
    # Assign if non-protected (XXX: here only measuredItemSuaFbs that are child should be assigned)
    # XXX I was told to save only 2014 onwards
    data[
      timePointYears >= 2014 &
        Protected == FALSE &
        measuredElementSuaFbs == 'production' &
        !is.na(imputed_deriv_value),
      `:=`(
        Value = imputed_deriv_value,
        flagObservationStatus = "I", flagMethod = "c")]
    
    data[, imputed_deriv_value := NULL]
  }
}

print("NEWBAL: end of derived production loop")


if (STOP_AFTER_DERIVED == TRUE) {
  print("NEWBAL: stop after production of derived")
  
  data_deriv <-
    data[
      measuredElementSuaFbs == 'production' &
        timePointYears %in% 2014:2017 &
        !dplyr::near(Value, 0) &
        Protected == FALSE,
      list(
        geographicAreaM49,
        measuredElementSuaFbs = "5510",
        measuredItemFbsSua = measuredItemSuaFbs,
        timePointYears,
        Value,
        flagObservationStatus,
        flagMethod
      )
      ]
  
  
  
  out <- SaveData(domain = "suafbs", dataset = "sua_unbalanced", data = data_deriv, waitTimeout = 20000)
  
  if (exists("out")) {
    
    print(paste(out$inserted + out$ignored, "derived products written"))
    
  } else {
    
    print("The newBalancing plugin had a problem when saving derived data.")
    
  }
  
  stop("Plugin stopped after derived, as requested. This is fine.")
}

fixed_proc_shares <- rbindlist(fixed_proc_shares, idcol = "level")

if (nrow(fixed_proc_shares) > 0) {

  setnames(fixed_proc_shares, c("processingShare", "reference_share"), c("orig_proc_share", "new_proc_share"))

  fixed_proc_shares <-
    fixed_proc_shares[order(geographicAreaM49, measuredItemParentCPC, measuredItemChildCPC, timePointYears)]

  fixed_proc_shares[,
    `:=`(
      measuredItemParentCPC = paste0("'", measuredItemParentCPC),
      measuredItemChildCPC = paste0("'", measuredItemChildCPC)
    )
  ]

  # XXX: removing value here as it is not the last computed value,
  # but we probably keep both (old and new)
  fixed_proc_shares[, Value := NULL]
} else {
  fixed_proc_shares <- data.table(info = "No processing shares need to be fixed.")
}

computed_shares <- rbindlist(computed_shares)

computed_shares_send <- rbindlist(computed_shares_send, fill = TRUE)

colnames(computed_shares_send) <- sub("Value_", "", colnames(computed_shares_send))

setnames(
  computed_shares_send,
  c("timePointYears", "measuredItemSuaFbs", "measuredItemParentCPC"),
  c("year", "measuredItemChildCPC_tree", "measuredItemParentCPC_tree")
)

computed_shares_send <- nameData("suafbs", "ess_fbs_commodity_tree2", computed_shares_send)

setnames(
  computed_shares_send,
  c("geographicAreaM49", "geographicAreaM49_description",
    "measuredItemChildCPC_tree", "measuredItemChildCPC_tree_description",
    "measuredItemParentCPC_tree", "measuredItemParentCPC_tree_description"),
  c("Country", "Country_name", "Child", "Child_name", "Parent", "Parent_name")
)

computed_shares_send[, zero_weigth := Child %in% zeroWeight]

computed_shares_send[, `:=`(Child = paste0("'", Child), Parent = paste0("'", Parent))]

negative_availability <- rbindlist(negative_availability)

negative_availability <- nameData("suafbs", "sua_unbalanced", negative_availability)

negative_availability[, measuredItemFbsSua := paste0("'", measuredItemFbsSua)]

## XXX fix these cases (check why these conditions happen)
# (COMMENTED AS THEY SHOULD HAVE BEEN ALREADY FIXED)
#computed_shares[processingShare < 0, processingShare := 0]
#computed_shares[processingShare > 1, processingShare := 1]


# Re-compute stocks. This is just a duplication of the code inside the
# loop by levels done before. Thus, it should be made a function of this.

condition_for_stocks <-
  data$stockable == TRUE &
  data$measuredItemSuaFbs %chin% items_to_generate_stocks &
  data$measuredElementSuaFbs %chin% c('production', 'imports', 'exports')

print("NEWBAL: re-creating stocks")

# XXX These are fake opening stocks
fake_opening_stocks <-
  data[
    condition_for_stocks == TRUE,
    list(timePointYears, measuredElementSuaFbs, Value),
    by = c('geographicAreaM49', 'measuredItemSuaFbs')
    ][,
      list(
        supply =
          sum(
            Value[measuredElementSuaFbs %in% c('production', 'imports')],
            - Value[measuredElementSuaFbs == 'exports'],
            na.rm = TRUE
          )
      ),
      by = c('geographicAreaM49', 'measuredItemSuaFbs', 'timePointYears')
      ][
        # XXX check if this is right (these cases shouldn't happen)
        # In this case opening_stocks will be zero and in the condition of negative delta being less than
        supply < 0,
        supply := 0
        ][
          order(geographicAreaM49, measuredItemSuaFbs, timePointYears)
          ][,
            min_year := timePointYears == min(timePointYears),
            by = c("geographicAreaM49", "measuredItemSuaFbs")
            ][
              min_year == TRUE,
              opening_stocks := ifelse(!is.na(supply), supply, 0)
              ]




# generate stocks for ALL
data_for_stocks <-
  data[
    condition_for_stocks == TRUE
    ][,
      list(
        supply =
          sum(
            Value[measuredElementSuaFbs %in% c('production', 'imports')],
            -Value[measuredElementSuaFbs == 'exports'],
            na.rm = TRUE
          )
      ),
      by = c("measuredItemSuaFbs", "geographicAreaM49", "timePointYears")
      ][
        order(measuredItemSuaFbs, geographicAreaM49, timePointYears)
        ][,
          delta := supply - RcppRoll::roll_mean(supply, 2, fill = 'extend', align = 'right')
          ][,
            supply := NULL
            ]

data_for_stocks <-
  merge(
    data_for_stocks,
    fake_opening_stocks,
    by = c('geographicAreaM49', 'measuredItemSuaFbs', 'timePointYears'),
    all = TRUE
  )

data_for_stocks[,
                min_year := timePointYears == min(timePointYears),
                by = c("geographicAreaM49", "measuredItemSuaFbs")
                ]

# To avoid degenerate cases, we will suppose that in the first year delta stocks are zero
data_for_stocks[min_year == TRUE, delta := 0]

# If generated delta is greater than supply, set it to 20% of supply
data_for_stocks[delta > 0 & delta > 0.2 * supply, delta := 0.2 * supply]

# Fix stocks
data_for_stocks <-
  plyr::ddply(
    data_for_stocks,
    .variables = c('geographicAreaM49', 'measuredItemSuaFbs'),
    .fun = function(x) fix_stocks(x)
  )

setDT(data_for_stocks)

data_for_stocks <-
  data_for_stocks[,
                  list(
                    geographicAreaM49,
                    measuredItemSuaFbs,
                    timePointYears,
                    opening_stocks,
                    delta = delta_updated
                  )
                  ]

data <-
  merge(
    data,
    data_for_stocks,
    by = c('geographicAreaM49', 'timePointYears', 'measuredItemSuaFbs'),
    all.x = TRUE
  )

# This should always happen, but just in case
if ("opening_stocks.x" %in% names(data)) {
  data[, opening_stocks := ifelse(!is.na(opening_stocks.x), opening_stocks.x, opening_stocks.y)]
  data[, c("opening_stocks.x", "opening_stocks.y") := NULL]
}

# XXX I was told to save only from 2014 onwards
data[
  timePointYears >= 2014 &
    Protected == FALSE &
    measuredElementSuaFbs == 'stock_change',
  `:=`(
    Value = delta,
    flagObservationStatus = "E",
    flagMethod = "u"
  )
  ][,
    delta := NULL
    ]

# 


#save stock data in SUA Unbalanced

data_stock_1 <- rbindlist(data_stock_1)

data_stock_1 <- subset(data_stock_1, measuredElementSuaFbs == "stock_change" & timePointYears %in% c(2014:2017))


data_stock_1 <- data_stock_1[!is.na(Value)]


data_stock_1 <- data_stock_1[!duplicated(data_stock_1[,c("geographicAreaM49","timePointYears","measuredItemSuaFbs"),with = F])]

data_stock_to_save_1 <-
  data_stock_1[
    measuredElementSuaFbs == 'stock_change' &
      timePointYears %in% 2014:2017 &
      Protected == FALSE,
    list(
      geographicAreaM49,
      measuredElementSuaFbs = "5071",
      measuredItemFbsSua = measuredItemSuaFbs,
      timePointYears,
      Value,
      flagObservationStatus,
      flagMethod
    )
    ]


data_stock_to_save_2 <-
  data[
    measuredElementSuaFbs == 'stock_change' &
      timePointYears %in% 2014:2017 &
      Protected == FALSE,
    list(
      geographicAreaM49,
      measuredElementSuaFbs = "5071",
      measuredItemFbsSua = measuredItemSuaFbs,
      timePointYears,
      Value,
      flagObservationStatus,
      flagMethod
    )
    ]

data_stock_to_save_2 <- data_stock_to_save_2[!is.na(Value)]

data_stock_to_save <- rbind(data_stock_to_save_1,data_stock_to_save_2)


data_stock_to_save <- data_stock_to_save[!duplicated(data_stock_to_save[,c("geographicAreaM49","timePointYears","measuredItemFbsSua"),with = F])]


SaveData(domain = "suafbs", dataset = "sua_unbalanced", data = data_stock_to_save, waitTimeout = 20000)





# XXX when we arrive here, delta stocks in 2014 for primary commodities with
# no children (missing processing level) are not computed.
# See, e.g., South Africa (710), sweet potatoes (01530)








print("NEWBAL: start outliers")

######################## OUTLIERS #################################
# re-writing of Cristina's outliers plugin with data.table syntax #

if (FIX_OUTLIERS == TRUE) {
  
  commDef <- ReadDatatable("fbs_commodity_definitions") # XXX: updated?
  # 
  primaryProxyPrimary_items <- commDef[proxy_primary == "X" | primary_commodity == "X"]$cpc
  food_items <- commDef[food_item == "X"]$cpc
  
  dout <-
    CJ(
      measuredItemSuaFbs = unique(data$measuredItemSuaFbs),
      measuredElementSuaFbs = unique(data$measuredElementSuaFbs),
      geographicAreaM49 = unique(data$geographicAreaM49),
      timePointYears = unique(data$timePointYears)
    )
  
  dout <-
    merge(
      dout,
      data,
      by = c('geographicAreaM49', 'timePointYears',
             'measuredItemSuaFbs', 'measuredElementSuaFbs'),
      all.x = TRUE
    )
  
  dout[is.na(Protected), Protected := FALSE]
  
  dout[,
    `:=`(
      production = Value[measuredElementSuaFbs  == "production"],
      supply     = sum(Value[measuredElementSuaFbs %in% c("production", "imports")],
                       - Value[measuredElementSuaFbs == "exports"], # XXX stocks?
                       na.rm = TRUE),
      domsupply  = sum(Value[measuredElementSuaFbs %in% c("production", "imports")],
                       na.rm = TRUE)
    ),
    by = c('geographicAreaM49', 'measuredItemSuaFbs', 'timePointYears')
  ]
  
  dout[measuredElementSuaFbs %in% c("feed", "industrial"), element_supply := supply]
  dout[measuredElementSuaFbs == "seed", element_supply := production]
  dout[measuredElementSuaFbs == "loss", element_supply := domsupply]
  
  dout[element_supply < 0, element_supply := NA_real_]
  
  dout[, ratio := Value / element_supply]
  
  dout[,
    `:=`(
      mean_ratio = mean(ratio[timePointYears %in% 2011:2013], na.rm = TRUE),
      Meanold    = mean(Value[timePointYears < 2014], na.rm = TRUE)
    ),
    by = c('geographicAreaM49', 'measuredItemSuaFbs', 'measuredElementSuaFbs')
  ]

  # If the element is new, there will be no `mean_ratio` and `Meanold`, thus we
  # use the new values to re-calculate them.

  dout[
    timePointYears >= 2014 & (is.na(mean_ratio) | is.nan(mean_ratio) | is.infinite(mean_ratio)),
    mean_ratio := mean(ratio[timePointYears >= 2014], na.rm = TRUE),
    by = c('geographicAreaM49', 'measuredItemSuaFbs', 'measuredElementSuaFbs')
  ]

  dout[
    timePointYears >= 2014 & (is.na(Meanold) | is.nan(Meanold) | is.infinite(Meanold)),
    Meanold := mean(Meanold[timePointYears >= 2014], na.rm = TRUE),
    by = c('geographicAreaM49', 'measuredItemSuaFbs', 'measuredElementSuaFbs')
  ]
  
  dout[mean_ratio > 1, mean_ratio := 1]
  
  dout[, abs_diff_threshold := ifelse(measuredElementSuaFbs == "feed", 0.1, 0.05)]
  
  dout[
    Protected == FALSE &
      timePointYears %in% 2014:2017 & # XXX: parameterise
      mean_ratio > 0 &
      abs(element_supply) > 0 &
      measuredElementSuaFbs %in% c("feed", "seed", "loss", "industrial") &
      # the conditions below define the outlier, or cases that were NA
      (is.na(Value) |
         abs(ratio - mean_ratio) > abs_diff_threshold |
         mean_ratio == 1 |
         (mean_ratio != 0 & dplyr::near(ratio, 0)) |
         (outside(Value / Meanold, 0.5, 2) & outside(Value - Meanold, -10000, 10000))),
    impute := element_supply * mean_ratio
  ]
  
  
  # Remove imputation for loss that is non-food and non-primaryProxyPrimary
  dout[
    measuredElementSuaFbs == "loss" &
      !(measuredItemSuaFbs %in% food_items &
          measuredItemSuaFbs %in% primaryProxyPrimary_items),
    impute := NA_real_
  ]
  
  dout <-
    dout[
      !is.na(impute) & (is.na(Value) | round(Value, 1) != round(impute, 1)),
      list(
        geographicAreaM49,
        measuredItemSuaFbs,
        measuredElementSuaFbs,
        timePointYears,
        Value_imputed = impute
      )
    ]
  
  if (nrow(dout) > 0) {
    
    data <-
      merge(
        data,
        dout,
        by = c('geographicAreaM49', 'measuredItemSuaFbs', 'measuredElementSuaFbs', 'timePointYears'),
        all = TRUE
      )
    
    tmp_file_outliers <-
      tempfile(pattern = paste0("OUTLIERS_", COUNTRY, "_"), fileext = '.csv')
    
    if (!file.exists(dirname(tmp_file_outliers))) {
      dir.create(dirname(tmp_file_outliers), recursive = TRUE)
    }
    
    write.csv(
      data[
        !is.na(Value_imputed),
        .(geographicAreaM49, measuredItemSuaFbs, measuredElementSuaFbs,
          timePointYears, flagObservationStatus, flagMethod, Value, Value_imputed)
        ],
      tmp_file_outliers
    )
    
    if (!CheckDebug()) {
      send_mail(
        from = "do-not-reply@fao.org",
        to = swsContext.userEmail,
        subject = "Outliers fixed by newBalancing plugin",
        body = c("There were some outliers (Value), fixed by newBalancing (Value_imputed)",
                 tmp_file_outliers)
      )
    }
    
    data[
      !is.na(Value_imputed),
      `:=`(
        Value = Value_imputed,
        flagObservationStatus = "E",
        flagMethod = "e")
      ]
    
    data[, Value_imputed := NULL]
  }
}

print("NEWBAL: end outliers")


####################### / OUTLIERS #################################




































data <- merge(data, itemMap, by = "measuredItemSuaFbs")

#setnames(itemMap, "measuredItemSuaFbs", "measuredItemParentCPC")

#data <- data[, list(measuredItemSuaFbs, measuredElementSuaFbs, geographicAreaM49,
#             timePointYears, Value, flagObservationStatus, flagMethod,
#             Valid, Protected, Official, type)]



# XXX ???
#cutItems <- ReadDatatable("cut_items2")[, cpc_code]

## XXX ???
#fbsTree <- ReadDatatable("fbs_tree")
## XXX: why this order?
#fbsTree <- fbsTree[, list(fbsID4 = id4, measuredItemSuaFbs = item_sua_fbs, fbsID1 = id1, fbsID2 = id1, fbsID3 = id3)]



#data <- data[, mget(c("measuredItemSuaFbs", "measuredElementSuaFbs", "geographicAreaM49", "timePointYears", "Value", "Official", "Protected", "type", "flagObservationStatus", "flagMethod"))]


## Split data based on the two factors we need to loop over
uniqueLevels <- unique(data[, list(geographicAreaM49, timePointYears)])

uniqueLevels <- uniqueLevels[order(geographicAreaM49, timePointYears)]

#parentNodes <- getCommodityLevel(tree, parentColname = "measuredItemParentCPC", childColname = "measuredItemChildCPC")
#
#parentNodes <- parentNodes[level == 0, node]

aggFun <- function(x) {
  if (length(x) > 1)
    stop("x should only be one value!")
  return(sum(x))
}






# FIXME: some names are set lowercase, but should be now camelCase
data[measuredElementSuaFbs == 'foodmanufacturing', measuredElementSuaFbs := 'foodManufacturing']
data[measuredElementSuaFbs == 'stock_change', measuredElementSuaFbs := 'stockChange']


data <-
  plyr::ddply(
    data,
    .variables = c('geographicAreaM49', 'timePointYears'),
    .fun = function(x) addMissingElements(as.data.table(x), p)
  )

setDT(data)

data[, stockable := measuredItemSuaFbs %chin% stockable_items]

# NOTE: the definition of "food_resid" changed (see inside newBalancing)
#data[, food_resid := measuredItemSuaFbs %chin% food_only_items]



# Remove stocks for non stockable items
data <- data[!(measuredElementSuaFbs == 'stock_change' & stockable == 'FALSE')]

#setDT(data)



# Remove residual and tourist (for now)
data <- data[!(measuredElementSuaFbs %chin% c('residual', 'tourist'))]

data[
  measuredElementSuaFbs %chin% c("feed", "food", "industrial", "loss", "seed"),
  movsum_value := RcppRoll::roll_sum(shift(Value), 3, fill = 'extend', align = 'right'),
  by = c("geographicAreaM49", "measuredItemSuaFbs", "measuredElementSuaFbs")
  ]

data[
  measuredElementSuaFbs %chin% c("feed", "food", "industrial", "loss", "seed"),
  mov_share := movsum_value / sum(movsum_value, na.rm = TRUE),
  by = c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs")
  ]

# Impute share if missing
data[
  measuredElementSuaFbs %chin% c("feed", "food", "industrial", "loss", "seed"),
  mov_share := rollavg(mov_share),
  by = c("geographicAreaM49", "measuredItemSuaFbs", "measuredElementSuaFbs")
  ]

# Set sum of shares = 1
data[
  measuredElementSuaFbs %chin% c("feed", "food", "industrial", "loss", "seed"),
  mov_share := mov_share / sum(mov_share, na.rm = TRUE),
  by = c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs")
  ]

#data[
#  measuredElementSuaFbs %chin% c("feed", "food", "industrial", "loss", "seed"),
#  movshare_sd := RcppRoll::roll_sd(shift(mov_share), 3, fill = 'extend', align = 'right'),
#  by = c("geographicAreaM49", "measuredItemSuaFbs", "measuredElementSuaFbs")
#]




data[is.na(Official), Official := FALSE]
data[is.na(Protected), Protected := FALSE]


# XXX Only from 2004 onwards
uniqueLevels <- uniqueLevels[timePointYears >= 2014][order(timePointYears)]


print("NEWBAL: set thresholds")


if (THRESHOLD_METHOD == 'nolimits') {
  
  print("NEWBAL: thresholds, nolimits")
  
  data[, min_threshold := -Inf]
  data[, max_threshold := Inf]
  
} else if (THRESHOLD_METHOD == 'level') { ############ DON'T USE
  
  print("NEWBAL: thresholds, level")
  
  data[,
       `:=`(
         min_threshold = min(Value[timePointYears %in% 2000:2013], na.rm = TRUE),
         max_threshold = max(Value[timePointYears %in% 2000:2013], na.rm = TRUE)
       ),
       by = c("measuredItemSuaFbs", "measuredElementSuaFbs", "geographicAreaM49")
       ]
  
} else if (THRESHOLD_METHOD == 'levelquartile') {
  
  print("NEWBAL: thresholds, share")
  
  data[,
       `:=`(
         min_threshold = quantile(Value[timePointYears %in% 2000:2013], 0.25, na.rm = TRUE),
         max_threshold = quantile(Value[timePointYears %in% 2000:2013], 0.75, na.rm = TRUE)
       ),
       by = c("measuredItemSuaFbs", "measuredElementSuaFbs", "geographicAreaM49")
       ]
  
} else if (THRESHOLD_METHOD == 'share') {
  
  print("NEWBAL: thresholds, share")
  
  data[,
       supply :=
         sum(
           Value[measuredElementSuaFbs %in% c('production', 'imports')],
           - Value[measuredElementSuaFbs %in% c('exports', 'stockChange')],
           na.rm = TRUE
         ),
       by = c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs")
       ]
  
  data[supply < 0, supply := 0]

  # `util_share` is the utilization share, defined over supply: for validated
  # years (i.e, until 2013), it should sum up to 1, given that utilizations
  # were balanced. For non-validated years (or better, for non balanced SUAs)
  # it does not sum to 1. min and max are defined over the validated years,
  # so using shares that sum to 1.
  
  data[!(measuredElementSuaFbs %chin% c("production", "imports", "exports", "stockChange")), util_share := Value / supply]

  data[is.infinite(util_share) | is.nan(util_share), util_share := NA_real_]

  data[util_share < 0, util_share := 0] # This really shouldn't happen

  data[util_share > 1, util_share := 1]
  
  if (NEW_THRESHOLDS == TRUE) {
    #No change in the  min/max threshold for Seed (decided by Salar on 22/07/2019)
    data[measuredElementSuaFbs == "seed",
         `:=`(
           min_util_share = min(util_share[timePointYears %in% 2000:2013], na.rm = TRUE),
           max_util_share = max(util_share[timePointYears %in% 2000:2013], na.rm = TRUE)
         ),
         by = c("measuredItemSuaFbs", "measuredElementSuaFbs", "geographicAreaM49")
         ]
    
    #Relaxing the min/max threshold of food,loss and feed by 10 % (decided by Salar on 22/07/2019)
    
    data[measuredElementSuaFbs %in% c("food","loss","feed"),
         `:=`(
           min_util_share = max(min(util_share[timePointYears %in% 2000:2013], na.rm = TRUE)-min(util_share[timePointYears %in% 2000:2013], na.rm = TRUE)*.1,0),
           max_util_share = min(max(util_share[timePointYears %in% 2000:2013], na.rm = TRUE)+max(util_share[timePointYears %in% 2000:2013], na.rm = TRUE)*.1,1)
         ),
         by = c("measuredItemSuaFbs", "measuredElementSuaFbs","geographicAreaM49")
         ]
    
    #Relaxing the min/max threshold of industrial by 100 % (decided by Salar on 22/07/2019)
    
    data[measuredElementSuaFbs %in% c("industrial"),
         `:=`(
           min_util_share = max(min(util_share[timePointYears %in% 2000:2013], na.rm = TRUE)-min(util_share[timePointYears %in% 2000:2013], na.rm = TRUE)*1,0),
           max_util_share = min(max(util_share[timePointYears %in% 2000:2013], na.rm = TRUE)+max(util_share[timePointYears %in% 2000:2013], na.rm = TRUE)*1,1)
         ),
         by = c("measuredItemSuaFbs", "measuredElementSuaFbs","geographicAreaM49")
         ]
  } else {

    data[,
      CV := sd(util_share[data$timePointYears %in% 2000:2013], na.rm = TRUE) / mean(util_share[data$timePointYears %in% 2000:2013], na.rm = TRUE),
      by = c("measuredItemSuaFbs", "measuredElementSuaFbs", "geographicAreaM49")
    ]

    data[is.na(CV) | is.infinite(CV) | is.nan(CV), CV := 0]
   
    data[,
        `:=`(
          min_util_share = max(min(util_share[timePointYears %in% 2000:2013], na.rm = TRUE)*(1-CV),0),
          max_util_share = min(max(util_share[timePointYears %in% 2000:2013], na.rm = TRUE)*(1+CV),1)
        ),
        by = c("measuredItemSuaFbs", "measuredElementSuaFbs", "geographicAreaM49")
        ]
  }
  
  data[is.infinite(min_util_share) | is.nan(min_util_share), min_util_share := NA_real_]
  
  data[is.infinite(max_util_share) | is.nan(max_util_share), max_util_share := NA_real_]
  
  data[max_util_share > 1, max_util_share := 1]
  
  data[,
       `:=`(
         min_threshold = supply * min_util_share,
         max_threshold = supply * max_util_share
       )
       ]
  
  data[, supply := NULL]
  
} else if (THRESHOLD_METHOD == "sharequartile") {
  
  print("NEWBAL: thresholds, sharequartile")
  
  data[,
       supply :=
         sum(
           Value[measuredElementSuaFbs %in% c('production', 'imports')],
           - Value[measuredElementSuaFbs %in% c('exports', 'stockChange')],
           na.rm = TRUE
         ),
       by = c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs")
       ]
  
  data[supply < 0, supply := 0]
  
  data[, util_share := Value / supply]
  
  data[is.infinite(util_share) | is.nan(util_share), util_share := NA_real_]
  
  data[,
       `:=`(
         min_util_share = quantile(util_share[timePointYears %in% 2000:2013], 0.25, na.rm = TRUE),
         max_util_share = quantile(util_share[timePointYears %in% 2000:2013], 0.75, na.rm = TRUE)
       ),
       by = c("measuredItemSuaFbs", "measuredElementSuaFbs", "geographicAreaM49")
       ]
  
  data[is.infinite(min_util_share) | is.nan(min_util_share), min_util_share := NA_real_]
  
  data[is.infinite(max_util_share) | is.nan(max_util_share), max_util_share := NA_real_]
  
  data[max_util_share > 1, max_util_share := 1]
  
  data[,
       `:=`(
         min_threshold = supply * min_util_share,
         max_threshold = supply * max_util_share
       )
       ]
  
  data[, supply := NULL]
  
} else if (THRESHOLD_METHOD == "sharedeviation1") { #  XXXXXXXXXXXXX don't use. DO NOT.
  
  print("NEWBAL: thresholds, sharedeviation1")
  
  data[,
       supply :=
         sum(
           Value[measuredElementSuaFbs %in% c('production', 'imports')],
           - Value[measuredElementSuaFbs %in% c('exports', 'stockChange')],
           na.rm = TRUE
         ),
       by = c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs")
       ]
  
  data[supply < 0, supply := 0]
  
  data[, util_share := Value / supply]
  
  data[is.infinite(util_share) | is.nan(util_share), util_share := NA_real_]
  
  data[,
       util_share_sd := sd(util_share[timePointYears %in% 2000:2013], na.rm = TRUE),
       by = c("measuredItemSuaFbs", "measuredElementSuaFbs", "geographicAreaM49")
       ]
  
  data[,
       `:=`(
         min_util_share = util_share - ifelse(!is.na(util_share_sd), util_share_sd, 0),
         max_util_share = util_share + ifelse(!is.na(util_share_sd), util_share_sd, 0)
       )
       ]
  
  data[is.infinite(min_util_share) | is.nan(min_util_share), min_util_share := NA_real_]
  
  data[is.infinite(max_util_share) | is.nan(max_util_share), max_util_share := NA_real_]
  
  data[max_util_share > 1, max_util_share := 1]
  
  data[,
       `:=`(
         min_threshold = supply * min_util_share,
         max_threshold = supply * max_util_share
       )
       ]
  
  data[, supply := NULL]
  
} else {
  stop("Invalid method.")
}

data[,
     `:=`(
       min_adj = min_threshold / Value,
       max_adj = max_threshold / Value
     )
     ]


data[min_adj > 1, min_adj := 0.9]
data[min_adj < 0, min_adj := 0.01]

data[max_adj < 1, max_adj := 1.1]
data[max_adj > 10, max_adj := 10] # XXX Too much?

# Recalculate the levels, given the recalculation of adj factors
data[, min_threshold := Value * min_adj]
data[, max_threshold := Value * max_adj]

# We need the min and max to be "near" one (but not too near) in case of
# Protected figures, so that they are not changed

data[Protected == TRUE, min_adj := 0.999]
data[Protected == TRUE, max_adj := 1.001]
# This fix should also be done in optim function?
data[
  dplyr::near(min_adj, 1) & dplyr::near(max_adj, 1),
  `:=`(
    min_adj = 0.999,
    max_adj = 1.001
  )
  ]



# Filter elements that appear for the first time

data_complete <-
  data.table(
    geographicAreaM49 = unique(data$geographicAreaM49),
    timePointYears = sort(unique(data$timePointYears)))[
      unique(data[, .(geographicAreaM49, measuredItemSuaFbs, measuredElementSuaFbs)]),
      on = "geographicAreaM49",
      allow.cartesian = TRUE
    ]

data_complete <-
  merge(
    data_complete,
    data[Value > 0, .(geographicAreaM49, measuredItemSuaFbs, measuredElementSuaFbs, timePointYears, Value)],
    by = c("geographicAreaM49", "measuredItemSuaFbs", "measuredElementSuaFbs", "timePointYears"),
    all.x = TRUE
  )

data_complete[, y := 1]

data_complete <-
  data_complete[,
    .(
      t_pre   = sum(y[timePointYears <= 2013]),
      t_post  = sum(y[timePointYears >= 2014]),
      na_pre  = sum(is.na(Value[timePointYears <= 2013])),
      na_post = sum(is.na(Value[timePointYears >= 2014]))
    ),
    by = c("geographicAreaM49", "measuredElementSuaFbs", "measuredItemSuaFbs")
  ]

new_elements <-
  data_complete[
    na_pre == t_pre & na_post < t_post
  ][,
    .(geographicAreaM49, measuredElementSuaFbs, measuredItemFbsSua = measuredItemSuaFbs)
  ][
    order(geographicAreaM49, measuredElementSuaFbs, measuredItemFbsSua)
  ]

new_elements <- nameData("suafbs", "sua_unbalanced", new_elements, except = "measuredElementSuaFbs")

new_elements[, measuredItemFbsSua := paste0("'", measuredItemFbsSua)]

tmp_file_name_new <- tempfile(pattern = paste0("NEW_ELEMENTS_", COUNTRY, "_"), fileext = '.csv')

write.csv(new_elements, tmp_file_name_new)

# / Filter elements that appear for the first time



## 1 => year = 2014
i <- 1

print("NEWBAL: starting balancing loop")

standData <- vector(mode = "list", length = nrow(uniqueLevels))

for (i in seq_len(nrow(uniqueLevels))) {
  #for (i in 1:17) {
  
  # For stocks, the first year no need to see back in time. After the first year was done,
  # stocks may have changed, so opening need to be changed in "data".
  
  if (i > 1) {
    items_stocks_changed <-
      unique(standData[[i-1]][!is.na(change_stocks)]$measuredItemSuaFbs)
    
    xxx <-
      rbind(
        # Previous data (balanced)
        standData[[i-1]][
          measuredItemSuaFbs %in% items_stocks_changed & measuredElementSuaFbs == 'stockChange',
          list(geographicAreaM49, timePointYears, measuredItemSuaFbs, delta = Value, opening_stocks)
          ],
        # New data (unbalanced)
        data[
          timePointYears > unique(standData[[i-1]]$timePointYears) &
            measuredItemSuaFbs %in% items_stocks_changed &
            measuredElementSuaFbs == 'stockChange',
          list(geographicAreaM49, timePointYears, measuredItemSuaFbs, delta = Value, opening_stocks)
          ]
      )
    
    # Remove these (there is no opening stock in the minimum year
    xxx <- xxx[!(measuredItemSuaFbs %in% xxx[timePointYears == min(timePointYears) & is.na(opening_stocks)]$measuredItemSuaFbs)]
    
    xxx1 <-
      plyr::ddply(
        xxx,
        .variables = c('geographicAreaM49', 'measuredItemSuaFbs'),
        .fun = function(x) fix_stocks(x)
      )
    
    setDT(xxx1)
    
    xxx1 <-
      xxx1[
        timePointYears > unique(standData[[i-1]]$timePointYears),
        list(geographicAreaM49, timePointYears, measuredItemSuaFbs,
             delta_updated, opening_stocks_updated = opening_stocks)
        ]
    
    data <-
      merge(
        data,
        xxx1,
        by = c('geographicAreaM49', 'timePointYears', 'measuredItemSuaFbs'),
        all.x = TRUE
      )
    
    data[
      timePointYears > unique(standData[[i-1]]$timePointYears) &
        measuredElementSuaFbs == "stockChange" &
        !is.na(delta_updated),
      `:=`(
        Value = delta_updated,
        opening_stocks = opening_stocks_updated,
        flagObservationStatus = "T",
        flagMethod = "c"
      )
      ][,
        c("delta_updated", "opening_stocks_updated") := NULL
        ]
  }
  
  filter <- uniqueLevels[i, ]
  data = as.data.frame(data)
  data = data %>%
    group_by(geographicAreaM49,measuredItemSuaFbs) %>%
    mutate(Food_Median = median(Value[measuredElementSuaFbs=="food" & timePointYears %in%2000:2013],na.rm=TRUE)) %>%
    mutate(Feed_Median = median(Value[measuredElementSuaFbs=="feed" & timePointYears %in%2000:2013],na.rm=TRUE)) %>%
    mutate(Industrial_Median = median(Value[measuredElementSuaFbs=="industrial" & timePointYears %in%2000:2013],na.rm=TRUE)) %>%
    # mutate(Value=replace(Value,measuredElementSuaFbs=="feed" & is.na(Feed_Median),NA)) %>%
    ungroup()
  data = as.data.table(data)
 
  utilizationTableSubset <-
    utilizationTable[uniqueLevels[i, list(geographicAreaM49)], on = 'geographicAreaM49']
  
  treeSubset <- tree[filter, on = c("geographicAreaM49", "timePointYears")]
  
  treeSubset[, c("geographicAreaM49", "timePointYears") := NULL]
  
    dataSubset <- data[filter, on = c("geographicAreaM49", "timePointYears")]

  #for (j in 1:10) {
    #print(i); flush.console()

    #debug(newBalancing)
    standData[[i]] <-
      newBalancing(
        data = dataSubset,
        tree = treeSubset,
        #nutrientData = subNutrientData,
        #batchnumber = batchnumber,
        utilizationTable = utilizationTableSubset, # XXX: this one needs to be removed
        Utilization_Table = Utilization_Table,
        zeroWeight = zeroWeight #,
        #fbsTree = fbsTree,
        #cutItems = cutItems
      )

    # FIXME: we are now assigning the "Protected" flag to ALL processing as
    # after the first loop it should have been computed and that value SHOULD
    # never be touched again.
    #standData[[i]][measuredElementSuaFbs == "foodManufacturing", Protected := TRUE]

    #dataSubset = as.data.table(standData[[i]])
  #}
  
}

print("NEWBAL: end of balancing loop")

standData <- rbindlist(standData)

standData[,
          `:=`(
            supply =
              sum(Value[measuredElementSuaFbs %chin% c('production', 'imports')],
                  - Value[measuredElementSuaFbs %chin% c('exports', 'stockChange')],
                  na.rm = TRUE),
            utilizations =
              sum(Value[!(measuredElementSuaFbs %chin% c('production', 'imports', 'exports', 'stockChange'))],
                  na.rm = TRUE)
          ),
          # year is quite unnesessary, but let's use it in any case
          by = c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs")
          ][,
            imbalance := supply - utilizations
            ][
              supply > 0,
              imbalance_percent := imbalance / supply * 100
              ]

opening_stocks_data <-
  standData[
    !is.na(Value) & measuredElementSuaFbs == 'stockChange',
    list(
      geographicAreaM49,
      measuredElementSuaFbs = "5113",
      measuredItemFbsSua = measuredItemSuaFbs,
      timePointYears,
      Value = opening_stocks,
      flagObservationStatus,
      flagMethod,
      Protected
    )
    ]

imbalances <-
  unique(
    standData[,
              list(
                geographicAreaM49,
                measuredElementSuaFbs = "5166",
                measuredItemFbsSua = measuredItemSuaFbs,
                timePointYears,
                Value = imbalance,
                flagObservationStatus = "I",
                flagMethod = "i",
                Protected = FALSE
              )
              ]
  )

imbalances_to_send <-
  standData[
    !is.na(Value) & outside(imbalance, -100, 100) & timePointYears >= 2014,
    list(
      geographicAreaM49,
      year = timePointYears,
      measuredItemFbsSua = measuredItemSuaFbs,
      measuredElementSuaFbs,
      flagObservationStatus,
      flagMethod,
      Value,
      imbalance,
      imbalance_percent,
      supply,
      utilizations
    )
    ]

d_imbal_info <-
  unique(
    imbalances_to_send[,
      .(
        geographicAreaM49,
        year,
        measuredItemFbsSua,
        supply = round(supply, 2),
        imbalance = round(imbalance, 2),
        perc_imb = round(abs(imbalance / supply) * 100, 2)
      )
    ]
  )

imbalances_info <-
  c(
    all_items = nrow(unique(standData[, .(geographicAreaM49, timePointYears, measuredItemSuaFbs)])),
    imb_tot = nrow(d_imbal_info),
    imb_pos_supply = nrow(d_imbal_info[supply > 0]),
    imb_gt_5percent = nrow(d_imbal_info[supply > 0][perc_imb > 5]),
    imb_avg_percent = d_imbal_info[supply > 0, mean(abs(perc_imb))]
  )

imbalances_to_send <-
  nameData('suafbs', 'sua_unbalanced', imbalances_to_send, except = c('measuredElementSuaFbs'))

imbalances_to_send[, measuredItemFbsSua := paste0("'", measuredItemFbsSua)]

data_OUTPUT <-
  imbalances_to_send[
    utilizations == 0 &
      imbalance < 0 &
      round(imbalance, 10) == round(supply, 10) &
      measuredElementSuaFbs %in% c("production", "imports", "exports")
  ]

non_existing_for_imputation <-
  data.table(measuredItemFbsSua = non_existing_for_imputation)

non_existing_for_imputation <-
  nameData('suafbs', 'sua_unbalanced', non_existing_for_imputation)

non_existing_for_imputation[, measuredItemFbsSua := paste0("'", measuredItemFbsSua)]

tmp_file_name_imb       <- tempfile(pattern = paste0("IMBALANCE_", COUNTRY, "_"), fileext = '.csv')
tmp_file_name_shares    <- tempfile(pattern = paste0("SHARES_", COUNTRY, "_"), fileext = '.csv')
tmp_file_name_negative  <- tempfile(pattern = paste0("NEGATIVE_AVAILAB_", COUNTRY, "_"), fileext = '.csv')
tmp_file_name_non_exist <- tempfile(pattern = paste0("NONEXISTENT_", COUNTRY, "_"), fileext = '.csv')
tmp_file_name_fix_shares  <- tempfile(pattern = paste0("FIXED_PROC_SHARES_", COUNTRY, "_"), fileext = '.csv')
tmp_file_name_NegNetTrade <- tempfile(pattern = paste0("NEG_NET_TRADE_", COUNTRY, "_"), fileext = '.csv')

if (!file.exists(dirname(tmp_file_name_imb))) {
  dir.create(dirname(tmp_file_name_imb), recursive = TRUE)
}

write.csv(imbalances_to_send,          tmp_file_name_imb)
write.csv(computed_shares_send,        tmp_file_name_shares)
write.csv(negative_availability,       tmp_file_name_negative)
write.csv(non_existing_for_imputation, tmp_file_name_non_exist)
write.csv(fixed_proc_shares,           tmp_file_name_fix_shares)
write.csv(data_OUTPUT, tmp_file_name_NegNetTrade)

saveRDS(
  computed_shares_send,
  file.path(R_SWS_SHARE_PATH, 'mongeau', paste0('computed_shares_send_', COUNTRY, '.rds'))
)




# XXX fix this
codes <- tibble::tribble(
  ~code,  ~name,
  "5910", "exports",
  "5520", "feed",
  "5141", "food",
  "5023", "foodManufacturing",
  "5610", "imports",
  "5165", "industrial",
  "5016", "loss",
  "5510", "production",
  "5525", "seed",
  "5071", "stockChange"
)

setDT(codes)

standData <- standData[codes, on = c('measuredElementSuaFbs' = 'name')]


standData <-
  standData[,
            list(
              geographicAreaM49,
              measuredElementSuaFbs = code,
              measuredItemFbsSua = measuredItemSuaFbs,
              timePointYears,
              Value,
              flagObservationStatus,
              flagMethod,
              Protected
            )
            ]

standData <- standData[!is.na(Value)]

# These cases should not happen (i.e., all flags should already be
# set), but in any case, add specific flags so to check.
standData[is.na(flagObservationStatus), flagObservationStatus := "M"]
standData[is.na(flagMethod), flagObservationStatus := "q"]


# Calculate calories

calories_per_capita <-
  merge(
    # Food
    standData[
      measuredElementSuaFbs == '5141',
      list(
        geographicAreaM49,
        measuredElementSuaFbs = "664",
        measuredItemFbsSua,
        timePointYears,
        food = Value,
        flagObservationStatus = "T",
        flagMethod = "i"
      )
      ],
    # Calories
    nutrientData[,
                 list(
                   geographicAreaM49,
                   measuredItemFbsSua = measuredItemCPC,
                   timePointYears = timePointYearsSP,
                   calories = Value
                 )
                 ],
    by = c('geographicAreaM49', 'timePointYears', 'measuredItemFbsSua'),
    all.x = TRUE
  )


calories_per_capita <-
  merge(
    calories_per_capita,
    popSWS[, list(geographicAreaM49, timePointYears, population = Value)],
    by = c('geographicAreaM49', 'timePointYears'),
    all.x = TRUE
  )


calories_per_capita[, Value := food * calories / population / 365 * 10]

calories_per_capita[, Protected := FALSE]

calories_per_capita[, c("food", "calories", "population") := NULL]

standData <- rbind(standData, imbalances, opening_stocks_data, calories_per_capita)

standData[dplyr::near(Value, 0) & Protected == FALSE, Value := NA_real_]

standData <- standData[timePointYears >= 2014][!is.na(Value)]

standData[, Protected := NULL]


# Download also calories
elemKeys_all <- c(elemKeys, "664")


# Get ALL data from SUA BALANCED, do an anti-join, set to NA whatever was not
# generated here so to clean the session on unbalanced of non-existing cells

if (CheckDebug()) {
  key_all <-
    DatasetKey(
      domain = "suafbs",
      dataset = "sua_balanced",
      dimensions =
        list(
          geographicAreaM49 = Dimension(name = "geographicAreaM49", keys = COUNTRY),
          measuredElementSuaFbs = Dimension(name = "measuredElementSuaFbs", keys = elemKeys_all),
          measuredItemFbsSua = Dimension(name = "measuredItemFbsSua", keys = itemKeys),
          # Get from 2010, just for the SUA aggregation
          timePointYears = Dimension(name = "timePointYears", keys = as.character(2010:2017))
        )
    )
} else {
  key_all <- swsContext.datasets[[1]]
  
  key_all@dimensions$timePointYears@keys <- as.character(2010:2017)
  key_all@dimensions$measuredItemFbsSua@keys <- itemKeys
  key_all@dimensions$measuredElementSuaFbs@keys <- elemKeys_all
  key_all@dimensions$geographicAreaM49@keys <- COUNTRY
}


data_suabal <- GetData(key_all)

if (nrow(data_suabal[timePointYears >= 2014]) > 0) {
  
  data_suabal_missing <-
    data_suabal[
      timePointYears >= 2014
      ][
        !standData,
        on = c('geographicAreaM49', 'measuredElementSuaFbs',
               'measuredItemFbsSua', 'timePointYears')
        ]
  
  if (nrow(data_suabal_missing) > 0) {
    
    data_suabal_missing[,
                        `:=`(
                          Value = NA_real_,
                          flagObservationStatus = NA_character_,
                          flagMethod = NA_character_
                        )
                        ]
    
    standData <- rbind(standData, data_suabal_missing)
  }
}


########## DES calculation

des <- 
  rbind(
    data_suabal[
      measuredElementSuaFbs == '664' & timePointYears %in% 2010:2013
      ][,
        list(Value = sum(Value, na.rm = TRUE)),
        by = c('geographicAreaM49', 'timePointYears', 'measuredItemFbsSua')
        ],
    standData[
      measuredElementSuaFbs == '664' & timePointYears >= 2014
      ][,
        list(Value = sum(Value, na.rm = TRUE)),
        by = c('geographicAreaM49', 'timePointYears', 'measuredItemFbsSua')
        ]
  )

des <-
  rbind(
    des,
    des[,
        list(measuredItemFbsSua = 'S2901', Value = sum(Value)),
        by = c('geographicAreaM49', 'timePointYears')
        ]
  )

des[, Value := round(Value, 2)]

##### Plot of main DES absolute variations

des_diff <-
  des[
    order(geographicAreaM49, measuredItemFbsSua, timePointYears)
  ][,
    .(year = timePointYears, diff = Value - shift(Value)),
    .(geographicAreaM49, item = measuredItemFbsSua)
  ][
    year != min(year)
  ]

des_diff <-
  des_diff[item %in% des_diff[abs(diff) > 20]$item]

des_diff[item == "S2901", item := "GRAND TOTAL"]

plot_main_des_diff <-
  ggplot(des_diff, aes(x = year, diff, group = item, color = item)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  ggtitle("Absolute variation of DES and main variations of items")

tmp_file_plot_main_des_diff <-
  tempfile(pattern = paste0("PLOT_MAIN_DES_DIFF_", COUNTRY, "_"), fileext = '.pdf')

ggsave(tmp_file_plot_main_des_diff, plot = plot_main_des_diff)

##### / Plot of main DES absolute variations

##### Plot of main DES

main_des_items <-
  des[,
    .(geographicAreaM49, year = timePointYears, item = measuredItemFbsSua, Value)
  ][
    item %in% des[Value > 100]$measuredItemFbsSua][order(geographicAreaM49, item, year)
  ]

main_des_items[item == "S2901", item := "GRAND TOTAL"]

plot_main_des_items <-
  ggplot(main_des_items, aes(x = year, Value, group = item, color = item)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  ggtitle("Main DES items (> 100 Calories)")

tmp_file_plot_main_des_items <-
  tempfile(pattern = paste0("PLOT_MAIN_DES_ITEMS_", COUNTRY, "_"), fileext = '.pdf')

ggsave(tmp_file_plot_main_des_items, plot = plot_main_des_items)

##### / Plot of main DES

des_cast <-
  dcast(
    des,
    geographicAreaM49 + measuredItemFbsSua ~ timePointYears,
    fun.aggregate = sum,
    value.var = "Value"
  )

des_cast <- nameData("suafbs", "sua_balanced", des_cast)

# The "000" is a trick so that it appears in first place after ordering
des_cast[measuredItemFbsSua == "S2901", measuredItemFbsSua_description := paste0("000", measuredItemFbsSua_description)]

des_cast <- des_cast[order(measuredItemFbsSua_description)]

des_cast[, measuredItemFbsSua_description := sub("^000", "", measuredItemFbsSua_description)]


# Main items are considered those for which in at least one
# year their calories were at least 50.
des_main <-
  des_cast[measuredItemFbsSua %chin% c("S2901", des[Value > 50, unique(measuredItemFbsSua)])]

des_main <-
  rbind(
    des_main,
    des_main[,
             lapply(.SD, function(x) round(sum(x[-1]) / x[1] * 100, 2)),
             .SDcols = c(sort(unique(des$timePointYears)))
             ],
    fill = TRUE
  )

des_main[
  is.na(measuredItemFbsSua),
  measuredItemFbsSua_description := "PERCENTAGE OF MAIN OVER TOTAL"
  ]

des_cast[, measuredItemFbsSua := paste0("'", measuredItemFbsSua)]
des_main[, measuredItemFbsSua := paste0("'", measuredItemFbsSua)]

tmp_file_des      <- tempfile(pattern = paste0("DES_", COUNTRY, "_"), fileext = '.csv')
tmp_file_des_main <- tempfile(pattern = paste0("DES_MAIN_ITEMS_", COUNTRY, "_"), fileext = '.csv')

write.csv(des_cast, tmp_file_des)
write.csv(des_main, tmp_file_des_main)

########## / DES calculation




out <- SaveData(domain = "suafbs", dataset = "sua_balanced", data = standData, waitTimeout = 20000)

if (exists("out")) {
  
  body_message <-
    sprintf(
      "Plugin completed in %1.2f minutes.

      ################################################
      # Imbalances (greater than 100 t in abs value) #
      ################################################

      unbalanced items: %s (out of %s)
      unbalanced items for items with positive supply: %s
      unbalanced items for items with imbalance > 5%%: %s
      average percent imbalance in absolute value: %1.2f

      ###############################################
      ############       Parameters       ###########
      ###############################################

      country = %s
      balancing method = %s
      thresold method = %s
      fix_outliers = %s
      fill_extraction_rates= %s
      new_thresholds = %s
      new_stocks_position = %s
      new_food_residual = %s
      residual_balancing= %s

      ###############################################
      ##############       Files       ##############
      ###############################################

      The following files are attached:

      - PLOT_MAIN_DES_ITEMS_*.pdf = Plot of main DES items (> 100 Calories)

      - PLOT_MAIN_DES_DIFF_*.pdf = Plot of main DES Calories variations

      - DES_*.csv = calculation of DES (total and by items)

      - DES_MAIN_ITEMS_*.csv = as DES_*.csv, but only with items that
          accounted for at least 50 calories in at least one year
          from 2010-2014, so, basically, the 'main' items

      - SHARES_*.csv = Parents/Children shares, availability, etc.

      - FILLED_ER_*.csv = filled extraction rates

      - IMBALANCE_*.csv = imbalances, supply and utilizations

      - NEGATIVE_AVAILAB_*.csv = items with negative availability

      - NONEXISTENT_*.csv = items for which no production, imports,
          or exports exist after 2013

      - NEW_ELEMENTS_*.csv = elements that appear for the first time

      - FIXED_PROC_SHARES_*.csv = items for which the processing share
          was fixed, to keep it consistent with its main co-product

      - NEG_NET_TRADE_*.csv = negative net trade

      ",
      difftime(Sys.time(), start_time, units = "min"),
      imbalances_info[['imb_tot']],
      prettyNum(imbalances_info[['all_items']], big.mark = ","),
      imbalances_info[['imb_pos_supply']],
      imbalances_info[['imb_gt_5percent']],
      imbalances_info[['imb_avg_percent']],
      COUNTRY,
      BALANCING_METHOD,
      THRESHOLD_METHOD,
      FIX_OUTLIERS,
      FILL_EXTRACTION_RATES,
      NEW_THRESHOLDS,
      NEW_STOCKS_POSITION,
      NEW_FOOD_RESIDUAL,
      RESIDUAL_BALANCING
    )
  
  if (!CheckDebug()) {
    send_mail(
      from = "do-not-reply@fao.org",
      to = swsContext.userEmail,
      subject = "Results from newBalancing plugin",
      body = c(body_message,
               tmp_file_plot_main_des_items,
               tmp_file_plot_main_des_diff,
               tmp_file_des,
               tmp_file_des_main,
               tmp_file_name_shares,
               tmp_file_name_imb,
               tmp_file_name_extr,
               tmp_file_name_negative,
               tmp_file_name_non_exist,
               tmp_file_name_new,
               tmp_file_name_fix_shares,
               tmp_file_name_NegNetTrade
              )
    )
  }
  
  print(paste(out$inserted + out$ignored, "observations written and problems with", out$discarded))
  
} else {
  
  print("The newBalancing plugin had a problem when saving data.")
  
}
