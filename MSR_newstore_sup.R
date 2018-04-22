# This script aims to process datamodel's datasource
# Author: Phuong Nguyen

# Load packages and get forms ---------------------------------------------

library(readxl)
library(tidyr)
library(dplyr, warn.conflicts = FALSE)
options(stringsAsFactors = FALSE)

# Get input  ---------------------------------------------
preload <- RTA::get_dm_data("C011", "MSR_newedit_store")
preload <- filter(preload, marked_as_deleted == 0)
preload <- select(preload, -marked_as_deleted, -max_order, -id)

msr_staff <- RTA::get_dm_data("C011", "MSR_staff")
msr_staff <- filter(msr_staff, marked_as_deleted == 0)
msr_staff <- filter(msr_staff, grepl("^dgw", username))

# Get duplicated staff_id with salerep_id in supasm_id/suprsm_id/supsd_id (get to filter in form)
preload_asm <- select(preload, -salerep_id, -salerep_lb)
preload_asm <- left_join(preload_asm, 
                         select(msr_staff, supasm_id, supasm_lb, staff_id), 
                         by = "staff_id") %>% 
               rename(salerep_id = supasm_id,
                      salerep_lb = supasm_lb)


preload_rsm <- select(preload, -salerep_id, -salerep_lb)
preload_rsm <- left_join(preload_rsm, 
                         select(msr_staff, suprsm_id, suprsm_lb, staff_id), 
                         by = "staff_id") %>% 
                rename(salerep_id = suprsm_id,
                       salerep_lb = suprsm_lb)

preload_sd <- select(preload, -salerep_id, -salerep_lb)
preload_sd <- left_join(preload_sd, 
                         select(msr_staff, supsd_id, supsd_lb, staff_id), 
                         by = "staff_id") %>% 
                rename(salerep_id = supsd_id,
                       salerep_lb = supsd_lb)

preload <- bind_rows(preload, preload_asm, preload_rsm, preload_sd)
preload <- preload %>% 
    arrange(submission_date)

# Create asm_id/rsm_id/sd_id
preload$sup_id <- preload$salerep_id
preload$sup_id <- ifelse(!preload$sup_id %in% "vacant" & !is.na(preload$sup_id) & preload$sup_id != "", 
                         preload$salerep_id, preload$staff_id)

preload <- left_join(preload, 
                     select(msr_staff, staff_id, asm_id = supasm_id, asm_lb = supasm_lb,
                                     rsm_id = suprsm_id, rsm_lb = suprsm_lb, 
                                     sd_id = supsd_id, sd_lb = supsd_lb), 
                     by = c("sup_id" = "staff_id"))

preload <- mutate(preload, id = 1:n())
preload <- select(preload, id, everything())
preload <- select(preload, -sup_id)

# End get input  ---------------------------------------------

# Data transformation -----------------------------------------------------

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