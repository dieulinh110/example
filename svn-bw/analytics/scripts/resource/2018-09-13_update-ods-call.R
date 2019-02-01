################################################################################
## Code: 2018-09-13_update-ods-call.R
## Description:
## Author: dieulinh
## Created date: 2018-09-13
## Updated date: 2018-09-13
################################################################################


## -----------------------------------------------------------------------------
## SET UP
## -----------------------------------------------------------------------------

## load packages
RTA::load_packages(readxl, tidyr, dplyr, RTA)

## import data
# call_history <- readxl::read_excel("~/Desktop/Lịch sử cuộc gọi từ 1008 - 2709.xlsx", col_types = "text")

call_history <- readr::read_csv("~/Desktop/Untitled 1.csv", col_types = cols(.default = "c"))
call_history_raw <- get_datasource_data("S038", "call_history")



## -----------------------------------------------------------------------------
## TRANSFORM DATA
## -----------------------------------------------------------------------------
names(call_history) <- toupper(names(call_history))
names(call_history) <- iconv(names(call_history), from = "UTF-8", to = "ASCII//TRANSLIT")
name_dict <- make_dict(c("THOI GIAN", "SO GOI",
                         "DAU SO", "SO NHAN",
                         "TRANG THAI", "THOI GIAN CUOC GOI",
                         "THOI GIAN THUC GOI", "PIN"),
                       c("starttime", "out_ipacc",
                         "out_telno", "receiver_telno",
                         "call_status", "call_duration",
                         "actual_duration", "pin"))

names(call_history) <- encode(names(call_history), name_dict)

call_history$pin <- NULL

call_history_raw <- call_history_raw %>%
  select(-dm_id, -max_order, -marked_as_deleted) %>%
  mutate(keyid = as.double(keyid))
## -----------------------------------------------------------------------------
## Format date

call_history$starttime <- as.POSIXct(call_history$starttime)
call_history$starttime <- format(call_history$starttime, "%d/%m/%Y %H:%M")
call_history <- call_history %>%
  mutate(ip_acc = gsub("^[^<]+<|>.*", "", out_ipacc),
         ip_acc = ifelse(grepl("<|>", out_ipacc), ip_acc, ""),
         ip_display = gsub('"', '', regmatches(out_ipacc, gregexpr('"([^"]*)"', out_ipacc))),
         ip_display = gsub(" $", "", ip_display),
         ip_display = ifelse(ip_display == "character(0)", "", ip_display),
         ip_display = ifelse(grepl("<|>", out_ipacc), ip_display, out_ipacc),
         keyid = max(call_history_raw$keyid)+1:n(),
         call_duration = paste0("0", call_duration),
         actual_duration = paste0("0", actual_duration))

## -----------------------------------------------------------------------------
## Union data raw and data

result <- bind_rows(call_history_raw, call_history)

# result1 <- result  %>% filter(!duplicated(paste0(starttime, ip_acc, ip_display, receiver_telno,
#                                                call_status, call_duration, actual_duration,
#                                                out_telno)))

## -----------------------------------------------------------------------------
## EXPORT DATA
## -----------------------------------------------------------------------------

if (is_local()) {
    ## Check data to look for possible errors
    DT::datatable(result)
} else {
    ## Remove unnecessary objects
    rm(list = setdiff(ls(), "result"))

    ## Save result to disk
    data.table::fwrite(result, file = "result.csv")
}
