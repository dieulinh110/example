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


## Get function

paste_visit <- function(..., sep = NULL, collapse = NULL, na.rm = TRUE) {
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
data <- "~/Documents/MCP_UPDATE.xlsx"

## Get input

msr_store_update <- tryCatch(
    read_excel(data),
    error = function(x) {
        stop('File phải có tên dưới dạng "MCP_UPDATE.xlsx"', call. = FALSE)
    }
)

msr_store <- get_dm_data("C011", "MSR_store")
msr_staff <- get_dm_data("C011", "MSR_staff")
province <- get_dm_data("C011", "kingsmen_province")

col <- c("store_lb_new", "city_id_new","city_lb_new",
         "region_lb_new", "salerep_lb_new", "store_status_new")

name_col <- make_dict(col, c("Tên KH", "Mã Tỉnh/Thành phố", "Tỉnh/Thành phố",
                             "Khu vực", "Tên NVBH Sales Rep", "Trạng thái KH"))


cols <- c("store_hnumber_new", "store_street_new", "store_ward_lb_new", 
          "store_district_lb_new", "store_type_id_new", 
          "visitday_id_mon", "visitday_id_tue", "visitday_id_wed", 
          "visitday_id_thu", "visitday_id_fri", "visitday_id_sat", 
          "store_owner_name_new",
          "store_owner_dob_new", "store_phone_new", col)

name_cols <- make_dict(cols, c("Số Nhà", "Đường", 
                               "Phường/Xã", "Quận/Huyện", "Loại KH", 
                               "Thứ 2", "Thứ 3", "Thứ 4", "Thứ 5", "Thứ 6", "Thứ 7",
                               "Họ & Tên", "Ngày tháng năm sinh", "SĐT",
                               name_col))

#sheet_name <- openxlsx::getSheetNames(data)

## Add warning to user

#if (all(!sheet_name %in% " MCP ( Master Data )")) {
#    stop('"File phải có sheet có tên dưới dạng " MCP ( Master Data )".', call. = FALSE)
#}

#if (ncol(msr_store_update) < 20) {
#    stop("Bảng dữ liệu có ít hơn 20 cột cần thiết.", call. = FALSE)
#}


if (any(!cols %in% names(msr_store_update))) {
    col_exist_e <- cols[!cols %in% names(msr_store_update)]
    col_exist_v <- name_cols[!cols %in% names(msr_store_update)]
    stop(paste0("Bảng dữ liệu thiếu cột ", col_exist_v, " (", col_exist_e, ").\n"), call. = FALSE)
}


## Create first row to column name
msr_store_update <- msr_store_update[!names(msr_store_update) %in% ""]
msr_store_update <- msr_store_update[!is.na(names(msr_store_update))]
row_all_na <- apply(msr_store_update, 1, function(x) all(is.na(x)))
msr_store_update <- msr_store_update[!row_all_na,]
#msr_store_update <- filter(msr_store_update, !is.na(store_lb_new) & !store_lb_new %in% "Tên KH")


if (any(is.na(msr_store_update[, col]))) {
    name_col_na <- names(msr_store_update)[apply(msr_store_update, 2, anyNA)]
    col_na_v <- name_col[col %in% name_col_na]
    col_na_e <- col[col %in% name_col_na]
    n <- which(is.na(msr_store_update[, col_na_e]), arr.ind=TRUE)
    a <- as.data.frame(n, stringsAsFactors = FALSE)
    a$name <- col_na[a$col]
    a <- filter(a, row != 1)
    if (nrow(a) > 0) {
        stop(paste0("Dòng ", a$row + 1, " - cột ", a$name, " phải điền đầy đủ thông tin.\n"), call. = FALSE)
    }
}

## Remove NA column
msr_store_update <- msr_store_update[-1,]
msr_store_update <- filter(msr_store_update, !store_lb_new %in% "Tên KH")

## Remove data test

msr_store[msr_store == ""] <- NA
msr_staff <- filter(msr_staff, !grepl("^rta|thiendao", username))
msr_staff[msr_staff == ""] <- NA

## Remove double space in data
remove_space <- function(data) {
    out <- lapply(data, function(x) gsub("[[:space:]][[:space:]]", " ", x))
    out <- as.data.frame(out, stringsAsFactors = FALSE)
    out
}

msr_store <- remove_space(msr_store)
msr_staff <- remove_space(msr_staff)
msr_store_update <- remove_space(msr_store_update)

## Divide store test

msr_store_test <- filter(msr_store, grepl("^RTA|TEST", store_id))
msr_store <- filter(msr_store, !grepl("^RTA|TEST", store_id))				 

## Create store_add

col_add <- c("store_street_new", "store_ward_lb_new", "store_district_lb_new", "city_lb_new")
msr_store_update$store_add_new <- paste_visit(list("SN:", msr_store_update$store_hnumber_new), sep = " ")

msr_store_update$store_add_new <- paste_visit(list(msr_store_update$store_add_new,
                                                   msr_store_update[, col_add]),
                                              sep = " - ")
## Change phonenumber

msr_store_update$store_phone_new <- gsub("\\.|,|\\s|-|\\*|`|:|\\\\|\\(.*\\)", "", msr_store_update$store_phone_new)
msr_store_update$store_phone_new <- stringr::str_replace_all(msr_store_update$store_phone_new, "[^[:digit:]]", "")
msr_store_update$store_phone_new <- ifelse(grepl("^0", msr_store_update$store_phone_new), 
                                           #msr_store_update$store_phone_new <- ifelse(startsWith(msr_store_update$store_phone_new, "0"), 
                                           msr_store_update$store_phone_new,
                                           paste_visit(list("0", msr_store_update$store_phone_new), sep = ""))

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

msr_store_update$visitday_id_new <- paste_visit(msr_store_update[, col_visit], sep = " ")

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

sales_code <- mutate(msr_store_update,
                     visitday_lb_new = strsplit(visitday_id_new, " "),
                     visitday_lb_new = lapply(visitday_lb_new, encode, visit_dict),
                     visitday_lb_new = lapply(visitday_lb_new, paste0, collapse = " "),
                     visitday_lb_new = unlist(visitday_lb_new),
                     visitday_lb_new = ifelse(visitday_lb_new == "NA", NA, visitday_lb_new))




## Create city_id

msr_store_update <- left_join(msr_store_update, 
                              select(province, city_id_new = dgw_city_id, dgw_city_lb, 
                                     city_id, city_lb), 
                              by = "city_id_new")
msr_store_update$city_id_new <- msr_store_update$city_id
msr_store_update$city_lb_new <- msr_store_update$city_lb

## Create salerep_id
msr_store_update <- msr_store_update %>% 
    rename(staff_id = salerep_id_new)
msr_store_update$salerep_lb_new <- capwords(msr_store_update$salerep_lb_new, strict = TRUE)
msr_store_update <- left_join(msr_store_update, select(msr_staff, salerep_id_new = staff_id, 
                                                       salerep_lb_new = staff_lb), 
                              by = "salerep_lb_new")


## Create store_status

msr_store_update$store_status_new <- toupper(msr_store_update$store_status_new)
status_dict <- make_dict(c("ON", "ON1/2", "OFF"), c("ON", "ON", "OFF"))
msr_store_update$store_status_new <- encode(msr_store_update$store_status_new, status_dict)

## Create store_owner_name

msr_store_update <- mutate(msr_store_update, 
                           store_owner_name_new = ifelse(is.na(store_owner_name_new), 
                                                         ".", store_owner_name_new))

## Create region_id

region <- distinct(select(msr_store, region_id_new = region_id, region_lb_new = region_lb))
msr_store_update <- left_join(msr_store_update, region, by = "region_lb_new")

## Create store_type_id

type_id_dict <- c("A", "B", "C")
type_lb_dict <- c("Lớn", "Vừa", "Nhỏ")
type_dict <- make_dict(type_id_dict, type_lb_dict)
msr_store_update$store_type_id_new <- toupper(msr_store_update$store_type_id_new)

msr_store_update$store_type_lb_new <- msr_store_update$store_type_id_new
msr_store_update$store_type_lb_new <- encode(msr_store_update$store_type_lb_new, type_dict)

##Update_time
msr_store_update$update_time_new <- Sys.time()

## Check existed store

msr_store_not_check <- filter(msr_store, !city_id %in% msr_store_update$city_id_new | 
                                  city_id %in% msr_store_update$city_id_new & !salerep_id %in% msr_store_update$salerep_id_new )
msr_store_check <- filter(msr_store, city_id %in% msr_store_update$city_id_new &
                              salerep_id %in% msr_store_update$salerep_id_new)

msr_store_check <- left_join(msr_store_check, msr_store_update, 
                             by = c("store_lb" = "store_lb_new",
                                    "store_phone" = "store_phone_new", 
                                    "store_type_id" = "store_type_id_new", 
                                    "store_type_lb" = "store_type_lb_new"))

### Existed store

msr_store_on <- filter(msr_store_check, !is.na(store_add_new))

replace_data <- function(data) {
    data$store_lb <- data$store_lb_new
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
    data$visitday_id <- data$visitday_id_new
    data$visitday_lb <- data$visitday_lb_new
    data$salerep_id <- data$salerep_id_new
    data$salerep_lb <- data$salerep_lb_new
    data$update_time <- data$update_time_new
    data
}

if (nrow(msr_store_on) > 0) {
    
    msr_store_on <- replace_data(msr_store_on)
    
}

### Change stores have store_id but not in file update by store_status = OFF

msr_store_off <- filter(msr_store_check, is.na(store_add_new))
msr_store_off <- mutate(msr_store_off, store_status = "OFF")
if (nrow(msr_store_off) > 0) {
    
    msr_store_off <- replace_data(msr_store_off)
    
}
### Stores haven't store_id

msr_store_not_exist <- anti_join(msr_store_update, msr_store_check, 
                                 by = c("store_lb_new" = "store_lb"))

msr_store_not_exist <- replace_data(msr_store_not_exist)

## Final stores

msr_store_final <- bind_rows(msr_store_not_check, msr_store_on, msr_store_off, msr_store_not_exist)

## Store_id của HCM có cấu trúc chia theo 4 vùng và bán sỉ/theo chuỗi

msr_store_final$region_hcm <- msr_store_final$region_id
region_hcm_dict <- make_dict(c("H1", "H2", "H3", "H4", "CHCM", "SiH"), 
                             c("HCM1", "HCM2", "HCM3", "HCM4", "HCMC", "HCMS"))
msr_store_final$region_hcm <- encode(msr_store_final$region_hcm, region_hcm_dict)
msr_store_final <- mutate(msr_store_final, city_split = ifelse(city_id %in% "HCM", region_hcm, city_id))

## Create store_id

msr_store_final <- split(msr_store_final, msr_store_final$city_split)
msr_store_final <- lapply(msr_store_final, function(x) {
    x <- x[order(x$store_id, na.last = TRUE),]
    
    x <- tidyr::separate(x, store_id, 
                         into = c("a", "b"), sep = x$city_split, remove = FALSE)
    
    x$b <- as.double(x$b)
    
    my.max <- function(x) ifelse(!all(is.na(x)), max(x, na.rm=T), NA)
    x$max <- my.max(x$b)
    
    for (i in unique(is.na(x$b))) {
        x$id_na <- NA
        x$id_na[is.na(x$b) == i] <- seq_len(sum(is.na(x$b) == i))
    }
    
    x$a <- ifelse(is.na(x$b), rowSums(cbind(x$max, x$id_na), na.rm = TRUE), x$b)
    
    x$store_id <- paste0(x$city_split, sprintf("%05d", x$a))
    x
    
})
msr_store_final <- do.call("rbind", msr_store_final)

#msr_store_final <- mutate(msr_store_final, store_id = ifelse(is.na(store_id), id, store_id))

## Create new column

msr_store_final$store_ward_id <- NA
msr_store_final$store_district_id <- NA
msr_store_final$store_doe <- NA

msr_store_final <- left_join(msr_store_final, select(msr_staff, staff_id,
                                                     supasm_id, supasm_lb, suprsm_id, suprsm_lb), 
                             by = c("salerep_id" = "staff_id"))

msr_store_final$asm_id <- msr_store_final$supasm_id
msr_store_final$asm_lb <- msr_store_final$supasm_lb
msr_store_final$rsm_id <- msr_store_final$suprsm_lb
msr_store_final$rsm_lb <- msr_store_final$suprsm_lb

msr_store_final <- bind_rows(msr_store_final, msr_store_test)

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
           salerep_id, salerep_lb, asm_id, asm_lb, rsm_id, rsm_lb)

final$id <- 1:nrow(final)

#final <- msr_store_update

# Handle script -------------------------------------------------------

output <- final

# Save result ---------------------------------------------------------

write.csv(output, 'result.csv', row.names = FALSE)
