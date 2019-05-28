
food_old <- readRDS("modules/Food Module/Data/old_food.rds")
  
food_old[, c("measuredElementSuaFbs") := NULL]  

setnames(food_old, c("measuredItemFbsSua","Value", "flagObservationStatus", "flagMethod"),c("measuredItemCPC","food_old","status_old","method_old"))

food_new <- readRDS("modules/Food Module/Data/newFood.rds")

food_new[, c("measuredElement") := NULL]  

setnames(food_new, c("Value", "flagObservationStatus", "flagMethod"),c("food_as_estimate","status_new","method_new"))

food_Classification <- readRDS("modules/Food Module/Data/food_classification_country_specific.rds")


data_food <- merge(food_old,food_new,by= c("geographicAreaM49","measuredItemCPC", "timePointYears"), all = T)

# data_food <- subset(data_food, timePointYears %in% c(2014:2017))


data_food <- merge(data_food, food_Classification, by.x = c("geographicAreaM49","measuredItemCPC"),
                   by.y =c("geographic_area_m49","measured_item_cpc") , all.x = TRUE)


data_food <- nameData("food", "fooddata", data_food)


data_food[, timePointYears_description := NULL]




cases <- data_food[status_old == "I" & status_new == "I" & method_old == "e" &
            method_new == "e" & food_classification == 'Food Estimate' & (food_old < 0.95 * food_as_estimate | food_old > 1.05 * food_as_estimate)]




cases[, old := scales::comma(round(food_old, 1))]

cases[, new := scales::comma(round(food_as_estimate, 1))]


cases[, ratio := round(100 * food_as_estimate / food_old,0) ]


z = subset(cases, ratio > 150)

z <- select(z, c("geographicAreaM49", "geographicAreaM49_description", "measuredItemCPC", "measuredItemCPC_description", "food_classification","timePointYears"
                 ,"old","new","ratio"))

write.xlsx(data_food,"modules/Food Module/Data/foodData_updated.xlsx",row.names = F)
