# This script aims to process datamodel's datasource
# Author: Phuong Nguyen

# Load packages and get forms ---------------------------------------------

pkgs <- c("readr", "tidyr", "dplyr", "RTA", "xlsx", "osrm", "purrr")
sapply(pkgs, require, character.only = TRUE)
options(stringsAsFactors = FALSE)

# Get input  ---------------------------------------------
try(msr_staff <- get_dm_data("C011", "MSR_staff"))
try(msr_store <- get_dm_data("C011", "MSR_store"))
try(msr_store_gps <- get_dm_data("C011", "MSR_store_gps"))
try(msr_new_store <- get_dm_data("C011", "MSR_newedit_store"))

# End get input  ---------------------------------------------


## -------------------------------------------------------
## transform-data
## filter dm
msr_staff <- filter(msr_staff, marked_as_deleted == 0)
msr_store <- filter(msr_store, marked_as_deleted == 0)
msr_store_gps <- filter(msr_store_gps, marked_as_deleted == 0)
msr_new_store <- filter(msr_new_store, marked_as_deleted == 0)              

## remove testing instances
msr_staff <- filter(msr_staff, grepl("^dgw", username))
msr_staff <- filter(msr_staff, staff_status != "Inactive")
# msr_staff <- filter(msr_staff, username != "dgw_vthieu")
msr_new_store <- filter(msr_new_store, grepl("^dgw", username))
msr_store <- bind_rows(msr_store, msr_new_store)


## --------------------------------------------------------------
## calculation

store_plan <- msr_store %>% 
  filter(!is.na(salerep_id), salerep_id != "",
         !is.na(visitday_id), visitday_id != "") %>% 
  select(store_id, salerep_id, visitday_id) %>% 
  mutate(visitday_id = strsplit(visitday_id, " ")) %>% 
  unnest(visitday_id)

store_plan <- left_join(store_plan,
                        select(msr_store_gps, store_id, store_gpsLatitude, store_gpsLongitude),
                        by = "store_id")

store_plan_na <- filter(store_plan, is.na(store_gpsLatitude), is.na(store_gpsLongitude))
store_plan <- filter(store_plan, !is.na(store_gpsLatitude), !is.na(store_gpsLongitude))
store_plan <- arrange(store_plan, salerep_id, visitday_id, store_gpsLatitude)

store_plan <- store_plan %>%
  mutate(loc = paste(store_id, store_gpsLongitude, store_gpsLatitude),
         loc = strsplit(loc, " "))

store_plan <- store_plan %>% 
  group_by(salerep_id) %>% 
  mutate(store_id_lead = lead(store_id),
         loc_lead = lead(loc)) %>%
  filter(!is.na(store_id)) %>% 
  ungroup(salerep_id)

store_plan <- select(store_plan, salerep_id, visitday_id, store_id, store_id_lead, loc, loc_lead)
store_plan_gps_na <- filter(store_plan, is.na(loc_lead))
store_plan <- filter(store_plan, !is.na(loc_lead))
store_plan$route <- map2(store_plan$loc, store_plan$loc_lead, osrmRoute)
store_plan <- unnest(store_plan, route)
store_plan <- bind_rows(store_plan, store_plan_gps_na)
store_plan <- store_plan %>% 
  select(salerep_id, visitday_id, store_id, store_id_lead, lat, lon) %>% 
  mutate(id = 1:n()) %>% 
  select(id, everything())
# Data transformation -----------------------------------------------------

preload <- store_plan

### remove empty columns and rows ###
col_isna <- apply(preload, 2, function(x) all(is.na(x)))
nacol_index <- which(col_isna == TRUE)

if (length(nacol_index) >= 1) {
  preload <- preload[, -nacol_index]
}

row_isna <- apply(preload, 1, function(x) all(is.na(x)))
narow_index <- which(row_isna == TRUE)

if (length(narow_index) >= 1) {
  preload <- preload[-narow_index, ]
}

rm(col_isna, nacol_index, row_isna, narow_index)

# set xlsx (csv) name
result <- preload
if(nrow(result)>0){
  result <- sapply(result, as.character)
}
result <- rbind(colnames(result),result)
jsonlite::toJSON(result, dataframe = 'values')