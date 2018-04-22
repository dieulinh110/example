# Resource   : UPDATE_MCP	 
# Author     : rta_dieulinh
# Date       : February 27th, 2018

# Load packages -------------------------------------------------------

library(readxl)
library(tidyr)
library(RSQLite)
library(jsonlite)
library(dplyr, warn.conflicts = FALSE)
library(RTA)
options(stringsAsFactors = FALSE)
sys_user <- Sys.info()["user"]

## Get function

paste_NA <- function(..., sep = NULL, collapse = NULL, na.rm = TRUE) {
    if (na.rm == TRUE) {
        
        paste.na <- function(x, sep) {
            x <- gsub("^\\s+|\\s+$", "", x)
            ret <- paste(na.omit(x), collapse = sep)
            is.na(ret) <- ret == ""
            return(ret)
        }
        df <- data.frame(..., stringsAsFactors = F)
        ret <- apply(df, 1, FUN = function(x) paste.na(x, sep))
        
        if (is.null(collapse))
            ret
        else {
            paste.na(ret, sep = collapse)
        }
    }
    else {
        
        paste(..., sep = sep, collapse = collapse)      
        
    }
}


switch(
    sys_user,
    dieulinh = {
        data <- #"~/Downloads/MCP_UPDATE.xlsx"
            "MEGAsync/RTA/KINGSMEN/form/MCP_UPDATE.xlsx"
    }, {
        data <- "MCP_UPDATE.xlsx"
    }
)		

## Get input
msr_store_update <- tryCatch(
    read_excel(data),
    error = function(x) {
        stop('File phải có tên dưới dạng "MCP_UPDATE.xlsx"', call. = FALSE)
    }
)

#msr_store <- get_dm_data("C011", "test_update_mcp")
msr_store <- get_dm_data("C011", "MSR_mcp_store")
msr_staff <- get_dm_data("C011", "MSR_staff")
province <- get_dm_data("C011", "km_vnadmin3")
region_city <- get_dm_data("C011", "kingsmen_province")

province <- select(province, city_id = tinh_id, city_lb = tinh_name) %>% unique()

col <- c("store_lb_new", "city_id_new",
         "region_lb_new")

namecol <- c("Tên KH", "Mã Tỉnh/Thành Phố",
             "Khu vực")

name_col <- make_dict(col, c("Tên KH", "Mã Tỉnh/Thành Phố",
                             "Khu vực"))


cols <- c("store_hnumber_new", "store_street_new", "store_ward_lb_new", 
          "store_district_lb_new", "store_type_id_new", 
          "visitday_id_mon", "visitday_id_tue", "visitday_id_wed", 
          "visitday_id_thu", "visitday_id_fri", "visitday_id_sat", 
          "store_owner_name_new",
          "store_owner_dob_new", "store_phone_new", 
          "store_id_new", "store_status_new", "username_new", col)

namecols <- c("Số Nhà", "Đường", 
              "Phường/Xã", "Quận/Huyện", "Loại KH", 
              "Thứ 2", "Thứ 3", "Thứ 4", "Thứ 5", "Thứ 6", "Thứ 7",
              "Họ & Tên", "Ngày tháng năm sinh", "Số ĐT", "Mã KH",
              "Trạng Thái KH", "RTA Account", namecol)

name_cols <- make_dict(cols, c("Số Nhà", "Đường", 
                               "Phường/Xã", "Quận/Huyện", "Loại KH", 
                               "Thứ 2", "Thứ 3", "Thứ 4", "Thứ 5", "Thứ 6", "Thứ 7",
                               "Họ & Tên", "Ngày tháng năm sinh", "Số ĐT","Mã KH",
                               "Trạng Thái KH", "RTA Account", name_col))

## Remove data test

msr_store[msr_store == ""] <- NA
msr_staff <- filter(msr_staff, !grepl("^rta|thiendao", username))
msr_staff[msr_staff == ""] <- NA

## Remove double space in data


remove_space <- function(data) {
    out <- lapply(data, function(x) gsub("^ *|(?<= ) | *$", "", x, perl=T))
    out <- as.data.frame(out, stringsAsFactors = FALSE)
    out
}

msr_store <- remove_space(msr_store)
msr_staff <- remove_space(msr_staff)
msr_store_update <- remove_space(msr_store_update)

#sheet_name <- openxlsx::getSheetNames(data)

## Add warning to user

#if (all(!sheet_name %in% " MCP ( Master Data )")) {
#    stop('"File phải có sheet có tên dưới dạng " MCP ( Master Data )".', call. = FALSE)
#}

#if (ncol(msr_store_update) < 20) {
#    stop("Bảng dữ liệu có ít hơn 20 cột cần thiết.", call. = FALSE)
#}

test_col_v <- apply(msr_store_update, 1, function(x) any(x %in% c("Mã KH", "Tên KH", "ASM")))
test_col_e <- all(cols %in% names(msr_store_update))

#test1 <- all(!names(msr_store_update) %in% cols)

if (test_col_e == TRUE) {
    
    msr_store_update <- msr_store_update
    
} else if (test_col_e == FALSE) {
    
if (any(test_col_v == TRUE)) {
    
    colnames(msr_store_update) <- msr_store_update[test_col_v == TRUE, ]
    col_dict <- make_dict(namecols, cols)
    names(msr_store_update) <- encode(names(msr_store_update), col_dict)
    
    }
    
}

msr_store_update <- msr_store_update[test_col_v != TRUE, ]

if (any(!cols %in% names(msr_store_update))) {
    col_exist_e <- cols[!cols %in% names(msr_store_update)]
    col_exist_v <- name_cols[!cols %in% names(msr_store_update)]
    stop(paste0("Bảng dữ liệu thiếu cột ", col_exist_v, " (", col_exist_e, ").\n"), call. = FALSE)
}


## Create first row to column name
msr_store$store_lb <- capwords(msr_store$store_lb, strict = TRUE)
msr_store_update$store_lb_new <- capwords(msr_store_update$store_lb_new, strict = TRUE)

msr_store_update <- msr_store_update[,!names(msr_store_update) %in% ""]
msr_store_update <- msr_store_update[,!is.na(colnames(msr_store_update))]
msr_store_update <- msr_store_update[, !grepl("^NA.", names(msr_store_update))]
msr_store_update <- msr_store_update[, !grepl(".1", names(msr_store_update))]
row_all_na <- apply(msr_store_update, 1, function(x) all(is.na(x)))
msr_store_update <- msr_store_update[!row_all_na,]

#msr_store_update <- filter(msr_store_update, !is.na(store_lb_new) & !store_lb_new %in% "Tên KH")


if (any(is.na(msr_store_update[, col]))) {
    name_col_na <- names(msr_store_update)[apply(msr_store_update, 1, anyNA)]
    col_na_v <- name_col[col %in% name_col_na]
    col_na_e <- col[col %in% name_col_na]
    n <- which(is.na(msr_store_update[, col_na_e]), arr.ind=TRUE)
    a <- as.data.frame(n, stringsAsFactors = FALSE)
    a$name <- col_na_v[a$col]
    #a <- filter(a, row != 1 & row != 2)
    if (nrow(a) > 0) {
        stop(paste0("Dòng ", a$row + 2, " - cột ", a$name, " phải điền đầy đủ thông tin.\n"), call. = FALSE)
    }
}


## Remove NA column
msr_store_update <- msr_store_update[-1,]
msr_store_update <- filter(msr_store_update, !store_lb_new %in% "Tên KH")

if (any(!msr_store_update$region_lb_new %in% msr_store$region_lb)) {
    region_exist <- msr_store_update$region_lb_new[!msr_store_update$region_lb_new %in% msr_store$region_lb]
    region_exist <- unique(region_exist[!is.na(region_exist)])
    if (length(region_exist) > 0) {
    stop(paste0('Dữ liệu Khu vực "', region_exist, '" không có trong hệ thống.\n'), call. = FALSE)
    }
}

if (any(!msr_store_update$city_id_new %in% province$city_id)) {
    city_exist <- msr_store_update$city_id_new[!msr_store_update$city_id_new %in% province$city_id]
    city_exist <- unique(city_exist[!is.na(city_exist)])
    if (length(city_exist) > 0) {
    stop(paste0('Dữ liệu Mã Tỉnh/Thành phố "', city_exist, '" không có trong hệ thống.\n'), call. = FALSE)
    }
}

if (any(!msr_store_update$username_new %in% msr_staff$username)) {
    staff_exist <- msr_store_update$username_new[!msr_store_update$username_new %in% msr_staff$username]
    staff_exist <- unique(staff_exist[!is.na(staff_exist)])
    if (length(staff_exist) > 0) {
    stop(paste0('Dữ liệu RTA Account "', staff_exist, '" không có trong hệ thống.\n'), call. = FALSE)
    }
}




## Divide store test

msr_store_test <- filter(msr_store, grepl("^RTA|TEST", store_id))
msr_store <- filter(msr_store, !grepl("^RTA|TEST", store_id))				 

## Create city_id

msr_store_update <- left_join(msr_store_update, 
                              select(province, city_id_final = city_id, city_lb_final = city_lb), 
                              by = c("city_id_new" = "city_id_final"))

msr_store_update$city_lb_new <- msr_store_update$city_lb_final

## Create region_id

region_city1 <- select(region_city, city_id, region_id)



region <- distinct(select(msr_store, region_id_new = region_id, region_lb_new = region_lb))
msr_store_update <- left_join(msr_store_update, region, by = "region_lb_new")


if (any(!msr_store_update$username_new %in% msr_staff$username)) {
    staff_exist <- msr_store_update$username_new[!msr_store_update$username_new %in% msr_staff$username]
    staff_exist <- unique(staff_exist[!is.na(staff_exist)])
    if (length(staff_exist) > 0) {
        stop(paste0('Dữ liệu RTA Account "', staff_exist, '" không có trong hệ thống.\n'), call. = FALSE)
    }
}

## Create store_add
col_add <- c("store_street_new", "store_ward_lb_new","store_district_lb_new", "city_lb_new")

msr_store_update$store_add_new <- paste_NA(list("SN:", msr_store_update$store_hnumber_new), sep = " ")

msr_store_update$store_add_new <- paste_NA(list(msr_store_update$store_add_new,
                                                msr_store_update[, col_add]),
                                           sep = " - ")
## Change phonenumber

msr_store_update$store_phone_new <- gsub("\\.|,|\\s|-|\\*|`|:|\\\\|\\(.*\\)", "", msr_store_update$store_phone_new)
msr_store_update$store_phone_new <- stringr::str_replace_all(msr_store_update$store_phone_new, "[^[:digit:]]", "")
msr_store_update$store_phone_new <- ifelse(grepl("^0", msr_store_update$store_phone_new), 
                                           #msr_store_update$store_phone_new <- ifelse(startsWith(msr_store_update$store_phone_new, "0"), 
                                           msr_store_update$store_phone_new,
                                           paste_NA(list("0", msr_store_update$store_phone_new), sep = ""))

## Create visitday_id

col_visit <- c("visitday_id_mon", "visitday_id_tue", "visitday_id_wed",
               "visitday_id_thu", "visitday_id_fri", "visitday_id_sat")

msr_store_update[, col_visit] <- ifelse(!is.na(msr_store_update[, col_visit]), 0, NA)

msr_store_update$visitday_id_mon <- ifelse(nchar(msr_store_update$visitday_id_mon) > 0, "Mon", NA)
msr_store_update$visitday_id_tue <- ifelse(nchar(msr_store_update$visitday_id_tue) > 0, "Tue", NA)
msr_store_update$visitday_id_wed <- ifelse(nchar(msr_store_update$visitday_id_wed) > 0, "Wed", NA)
msr_store_update$visitday_id_thu <- ifelse(nchar(msr_store_update$visitday_id_thu) > 0, "Thu", NA)
msr_store_update$visitday_id_fri <- ifelse(nchar(msr_store_update$visitday_id_fri) > 0, "Fri", NA)
msr_store_update$visitday_id_sat <- ifelse(nchar(msr_store_update$visitday_id_sat) > 0, "Sat", NA)

msr_store_update$visitday_id_new <- paste_NA(msr_store_update[, col_visit], sep = " ")

msr_store_update$visitday_lb_new <- msr_store_update$visitday_id_new
# msr_store_update$visitday_lb_new <- gsub("Mon", "Thứ 2", msr_store_update$visitday_lb_new)
# msr_store_update$visitday_lb_new <- gsub("Tue", "Thứ 3", msr_store_update$visitday_lb_new)
# msr_store_update$visitday_lb_new <- gsub("Wed", "Thứ 4", msr_store_update$visitday_lb_new)
# msr_store_update$visitday_lb_new <- gsub("Thu", "Thứ 5", msr_store_update$visitday_lb_new)
# msr_store_update$visitday_lb_new <- gsub("Fri", "Thứ 6", msr_store_update$visitday_lb_new)
# msr_store_update$visitday_lb_new <- gsub("Sat", "Thứ 7", msr_store_update$visitday_lb_new)

visit_id_dict <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
visit_lb_dict <- c("Thứ 2", "Thứ 3", "Thứ 4", "Thứ 5", "Thứ 6", "Thứ 7")
visit_dict <- make_dict(visit_id_dict, visit_lb_dict)

# msr_store_update <- mutate(msr_store_update,
#                            visitday_lb_new = strsplit(visitday_id_new, " "),
#                            visitday_lb_new = lapply(visitday_lb_new, encode, visit_dict),
#                            visitday_lb_new = lapply(visitday_lb_new, paste0, collapse = " "),
#                            visitday_lb_new = unlist(visitday_lb_new),
#                            visitday_lb_new = ifelse(visitday_lb_new == "NA", NA, visitday_lb_new))

for(i in 1:length(visit_lb_dict)){
    msr_store_update$visitday_lb_new <- 
        gsub(visit_id_dict[i], visit_lb_dict[i], msr_store_update$visitday_lb_new)}



## Create salerep_id

msr_store_update <- left_join(msr_store_update, select(msr_staff, username, staff_id, staff_lb), 
                              by = c("username_new" = "username"))
msr_store_update$salerep_id_new <- msr_store_update$staff_id
msr_store_update$salerep_lb_new <- msr_store_update$staff_lb

## Create store_status
msr_store_update$store_status_new <- ifelse(is.na(msr_store_update$store_status_new), 
                                            "On", msr_store_update$store_status_new)
msr_store_update$store_status_new <- toupper(msr_store_update$store_status_new)
status_dict <- make_dict(c("ON", "ON1/2", "OFF"), c("ON", "ON", "OFF"))
msr_store_update$store_status_new <- encode(msr_store_update$store_status_new, status_dict)

## Create store_owner_name

msr_store_update <- mutate(msr_store_update, 
                           store_owner_name_new = ifelse(is.na(store_owner_name_new), 
                                                         ".", store_owner_name_new))


## Create store_type_id

type_id_dict <- c("A", "B", "C")
type_lb_dict <- c("Lớn", "Vừa", "Nhỏ")
type_dict <- make_dict(type_id_dict, type_lb_dict)
msr_store_update$store_type_id_new <- toupper(msr_store_update$store_type_id_new)

msr_store_update$store_type_lb_new <- msr_store_update$store_type_id_new
msr_store_update$store_type_lb_new <- encode(msr_store_update$store_type_lb_new, type_dict)
msr_store_update <- mutate(msr_store_update, 
                           store_type_lb_new = ifelse(is.na(store_type_lb_new), 
                                                      ".", store_type_lb_new),
                           store_type_id_new = ifelse(is.na(store_type_id_new), 
                                                      ".", store_type_id_new)
)

## Remove duplicated store

msr_store_update <- msr_store_update[!duplicated(msr_store_update[,c("store_lb_new", 
                                                                     "store_add_new", 
                                                                     "store_type_id_new", 
                                                                     "store_owner_name_new",
                                                                     "store_phone_new", 
                                                                     "visitday_id_new")]), ]

## Update_time
msr_store_update$update_time_new <- Sys.Date()
msr_store_update$update_hour_new <- Sys.time() 

## Check existed store

msr_store_not_check <- filter(msr_store, !city_id %in% msr_store_update$city_id_new | 
                                  city_id %in% msr_store_update$city_id_new & !salerep_id %in% msr_store_update$salerep_id_new)
msr_store_not_check$update_time <- as.Date(msr_store_not_check$update_time)
msr_store_not_check$update_hour <- as.POSIXct(msr_store_not_check$update_hour)

msr_store_check <- filter(msr_store, city_id %in% msr_store_update$city_id_new &
                              salerep_id %in% msr_store_update$salerep_id_new)

msr_store_check$update_time <- as.Date(msr_store_check$update_time)
msr_store_check$update_hour <- as.POSIXct(msr_store_check$update_hour)

msr_store_check <- left_join(msr_store_check, msr_store_update, 
                             by = c("store_id" = "store_id_new"))

### Existed store

msr_store_on <- filter(msr_store_check, !is.na(store_add_new))

replace_data <- function(data) {
    data$store_hnumber <- data$store_hnumber_new
    data$store_street <- data$store_street_new
    data$store_ward_lb <- data$store_ward_lb_new
    data$store_district_lb <- data$store_district_lb_new
    data$city_id <- data$city_id_new
    data$city_lb <- data$city_lb_new
    data$region_id <- data$region_id_new
    data$region_lb <- data$region_lb_new
    data$store_add <- data$store_add_new
    data$store_phone <- data$store_phone_new
    data$store_owner_dob <- data$store_owner_dob_new
    data$store_owner_name <- data$store_owner_name_new
    data$store_type_id <- data$store_type_id_new
    data$store_type_lb <- data$store_type_lb_new
    data$visitday_id <- data$visitday_id_new
    data$visitday_lb <- data$visitday_lb_new
    data$store_status <- data$store_status_new
    data$salerep_id <- data$salerep_id_new
    data$salerep_lb <- data$salerep_lb_new
    data$update_time <- data$update_time_new
    data$update_hour <- data$update_hour_new
    data
}

if (nrow(msr_store_on) > 0) {
    
    msr_store_on <- replace_data(msr_store_on)
    
}

### Change stores have store_id but not in file update by store_status = OFF

msr_store_off <- filter(msr_store_check, is.na(store_add_new))
msr_store_off <- mutate(msr_store_off, store_status = "OFF")
#if (nrow(msr_store_off) > 0) {

#    msr_store_off <- replace_data(msr_store_off)

#}

### Stores haven't store_id

msr_store_not_exist <- anti_join(msr_store_update, msr_store_check, 
                                 by = c("store_id_new" = "store_id"))

msr_store_not_exist <- replace_data(msr_store_not_exist)
msr_store_not_exist$store_lb <- msr_store_not_exist$store_lb_new
#msr_store_not_exist$store_phone <- msr_store_not_exist$store_phone_new
#msr_store_not_exist$store_type_id <- msr_store_not_exist$store_type_id_new
#msr_store_not_exist$store_type_lb <- msr_store_not_exist$store_type_lb_new

## Final stores

msr_store_final <- bind_rows(msr_store_not_check, msr_store_on, msr_store_off, msr_store_not_exist)

## Store_id của HCM có cấu trúc chia theo 4 vùng và bán sỉ/theo chuỗi

msr_store_final$region_hcm <- msr_store_final$region_id
region_hcm_dict <- make_dict(c("H1", "H2", "H3", "H4", "CHCM", "SiH"), 
                             c("HCM1", "HCM2", "HCM3", "HCM4", "HCMC", "HCMS"))
msr_store_final$region_hcm <- encode(msr_store_final$region_hcm, region_hcm_dict)
msr_store_final <- mutate(msr_store_final, city_split = ifelse(city_id %in% "HCM", region_hcm, paste0(city_id, "0")))

## Create store_id

msr_store_final <- split(msr_store_final, msr_store_final$city_split)
msr_store_final <- lapply(msr_store_final, function(x) {
    x <- x[order(x$store_id, na.last = TRUE),]
    
    substrRight <- function(x, n){
        #substr(x, nchar(x)-n+1, nchar(x))
        substring(x, n)
    }
    
    x$b <- as.double(substrRight(x$store_id, 5))
    
    my.max <- function(x) ifelse(!all(is.na(x)), max(x, na.rm=T), NA)
    x$max <- my.max(x$b)
    
    for (i in unique(is.na(x$b))) {
        x$id_na <- NA
        x$id_na[is.na(x$b) == i] <- seq_len(sum(is.na(x$b) == i))
    }
    
    x$a <- ifelse(is.na(x$b), rowSums(cbind(x$max, x$id_na), na.rm = TRUE), x$b)
    
    pad_zero <- function(x) {
        x <- as.character(x)
        len <- nchar(x)
        if (len < 5 & x != "") x <- paste0(paste0(rep("0", 5 - len), collapse = ""), x)
        x
    }
    
    x$a <- vapply(x$a, pad_zero, "")
    
    x$store_id <- paste0(x$city_split, x$a)
    x
    
})
msr_store_final <- do.call("rbind", msr_store_final)

#msr_store_final <- mutate(msr_store_final, store_id = ifelse(is.na(store_id), id, store_id))

## Create new column

msr_store_final$store_ward_id <- NA
msr_store_final$store_district_id <- NA
msr_store_final$store_doe <- NA

msr_store_final <- left_join(msr_store_final, select(msr_staff, staff_id,
                                                     supasm_id, supasm_lb, suprsm_id, 
                                                     suprsm_lb, supsd_id, supsd_lb), 
                             by = c("salerep_id" = "staff_id"))

msr_store_final$asm_id <- msr_store_final$supasm_id
msr_store_final$asm_lb <- msr_store_final$supasm_lb
msr_store_final$rsm_id <- msr_store_final$suprsm_id
msr_store_final$rsm_lb <- msr_store_final$suprsm_lb
msr_store_final$sd_id <- msr_store_final$supsd_id
msr_store_final$sd_lb <- msr_store_final$supsd_lb

msr_store_test$update_time <- as.Date(msr_store_test$update_time)
msr_store_test$update_hour <- as.POSIXct(msr_store_test$update_hour)

msr_store_final <- bind_rows(msr_store_final, msr_store_test)
msr_store_final$store_marker <- 1
msr_store_final <- arrange(msr_store_final, desc(update_time))  

## Final table

final <- msr_store_final %>% 
    select(store_id, store_lb,
           store_hnumber, store_street, store_ward_id, store_ward_lb,
           store_district_id, store_district_lb, city_id, city_lb,
           region_id, region_lb, 
           store_add, store_phone, store_owner_dob, 
           store_owner_name, store_doe, 
           store_type_id, store_type_lb,
           visitday_id, visitday_lb, store_status,
           salerep_id, salerep_lb, asm_id, asm_lb, rsm_id, rsm_lb, sd_id, sd_lb, update_time, update_hour, store_marker)

## Supall_id
staff <- filter(msr_staff, !staff_status %in% "Inactive")
sup_sd <- c(staff$staff_id[staff$user_role %in% "SD"])
sup_admin <- c(staff$staff_id[staff$user_role %in% "ADMIN"])
sup_all <- paste(unlist(strsplit(c(sup_sd, sup_admin), " ")), collapse = " ")

final$supall_id <- paste_NA(c(final[, c("asm_id", "rsm_id", "sd_id")], sup_all), sep = " ")

final$supall_id <- sapply(final$supall_id, 
                          function(x) paste(unique(unlist(strsplit(x, " "))), collapse = " "))


#final$id <- 1:nrow(final)
final[is.na(final)] <- ""

#final <- msr_store_update

# Handle script -------------------------------------------------------

output <- final

# Save result ---------------------------------------------------------

write.csv(output, 'result.csv', row.names = FALSE)
