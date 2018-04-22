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

## Get input

msr_store_update <- tryCatch(
    read_excel("MCP_UPDATE.xlsx", sheet = " MCP ( Master Data )"),
    error = function(x) {
        stop("File cần được đặt tên là MCP_UPDATE", call. = FALSE)
    }
)
msr_store <- get_dm_data("C011", "MSR_store")
msr_staff <- get_dm_data("C011", "MSR_staff")
province <- get_dm_data("CPMS0006", "province")

col <- c("Tên KH", "Tỉnh/Thành Phố",
         "Khu vực", "Tên NVBH Sales Rep ")


cols <- c("Số Nhà", "Đường", "Phường/Xã", 
          "Quận/Huyện", "Loại KH", 
          "Thứ 2", "Thứ 3", "Thứ 4", 
          "Thứ 5", "Thứ 6", "Thứ 7", "Trạng thái KH", "Họ & Tên",
          "Ngày tháng năm sinh", "Số ĐT", col)

sheet_name <- openxlsx::getSheetNames("~/MCP_UPDATE.xlsx")

if (all(!sheet_name %in% " MCP ( Master Data )")) {
    stop('"File phải có sheet có tên dưới dạng " MCP ( Master Data )"', call. = FALSE)
}

if (ncol(msr_store_update) < 19) {
    stop("Bảng dữ liệu có ít hơn 19 cột", call. = FALSE)
}

if (all(!names(msr_store_update) %in% cols)) {
    stop(paste("Bảng dữ liệu không có cột",  cols, ".\n"), call. = FALSE)
}


if (any(is.na(msr_store_update[, col]))) {
    stop(paste("Cột",  col, "phải điền đầy đủ thông tin.\n"), call. = FALSE)
}


## Create first row to column name

#msr_store_update <- filter(msr_store_update, !is.na(`Tên KH`))
#msr_store_update <- filter(msr_store_update, !is.na(msr_store_update[, 2]))
msr_store_update <- msr_store_update[-1,]
colnames(msr_store_update) <- msr_store_update[1, ]
msr_store_update <- msr_store_update[-1, ]


## Create store_add

#msr_store_update[,c(3:7)][is.na(msr_store_update[,c(3:7)])] <- ""
#msr_store_update <- mutate(msr_store_update, store_add = paste("SN:", `Số Nhà`, "-", Đường, "-", `Phường/Xã`, 
#                                                               "-", `Quận/Huyện`, "-",  `Tỉnh/Thành Phố`))

msr_store_update$store_add_new <- paste_visit(list("SN:", msr_store_update[, 3]), sep = " ")

msr_store_update$store_add_new <- paste_visit(list(msr_store_update$store_add_new,
                                                   msr_store_update[, c(4:7)]),
                                              sep = " - ")
## Change phonenumber

msr_store_update$store_phone <- msr_store_update$`Số ĐT`
msr_store_update$store_phone <- gsub("\\.", "", msr_store_update$store_phone)
msr_store_update$store_phone <- ifelse(grepl("^0", msr_store_update$store_phone), msr_store_update$store_phone, 
                                       paste_visit(list("0", msr_store_update$store_phone), sep = ""))

## Create visitday_id

msr_store_update$`Thứ 2` <- ifelse(!is.na(msr_store_update$`Thứ 2`), "Mon", NA)
msr_store_update$`Thứ 3` <- ifelse(!is.na(msr_store_update$`Thứ 3`), "Tue", NA)
msr_store_update$`Thứ 4` <- ifelse(!is.na(msr_store_update$`Thứ 4`), "Wed", NA)
msr_store_update$`Thứ 5` <- ifelse(!is.na(msr_store_update$`Thứ 5`), "Thu", NA)
msr_store_update$`Thứ 6` <- ifelse(!is.na(msr_store_update$`Thứ 6`), "Fri", NA)
msr_store_update$`Thứ 7` <- ifelse(!is.na(msr_store_update$`Thứ 7`), "Sat", NA)

msr_store_update$visitday_id_new <- paste_visit(msr_store_update[,c(15:20)], sep = " ")

msr_store_update$visitday_lb_new <- msr_store_update$visitday_id_new
msr_store_update$visitday_lb_new <- gsub("Mon", "Thứ 2", msr_store_update$visitday_lb_new)
msr_store_update$visitday_lb_new <- gsub("Tue", "Thứ 3", msr_store_update$visitday_lb_new)
msr_store_update$visitday_lb_new <- gsub("Wed", "Thứ 4", msr_store_update$visitday_lb_new)
msr_store_update$visitday_lb_new <- gsub("Thu", "Thứ 5", msr_store_update$visitday_lb_new)
msr_store_update$visitday_lb_new <- gsub("Fri", "Thứ 6", msr_store_update$visitday_lb_new)
msr_store_update$visitday_lb_new <- gsub("Sat", "Thứ 7", msr_store_update$visitday_lb_new)

# visit_id_dict <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
# visit_lb_dict <- c("Thứ 2", "Thứ 3", "Thứ 4", "Thứ 5", "Thứ 6", "Thứ 7")
# visit_dict <- make_dict(visit_id_dict, visit_lb_dict)


## Create city_id

msr_store_update$city_lb_new <- msr_store_update$`Tỉnh/Thành Phố`
msr_store_update <- left_join(msr_store_update, 
                              select(province, city_id_new = city_id, city_lb_new = city_lb), 
                              by = "city_lb_new")


## Create new column

msr_store_update$store_lb <- msr_store_update$`Tên KH`
msr_store_update$store_hnumber_new <- msr_store_update$`Số Nhà`
msr_store_update$store_street_new <- msr_store_update$Đường
msr_store_update$store_ward_id_new <- NA
msr_store_update$store_ward_lb_new <- msr_store_update$`Phường/Xã`
msr_store_update$store_district_id_new <- NA
msr_store_update$store_district_lb_new <- msr_store_update$`Quận/Huyện`
msr_store_update$store_owner_dob_new <- msr_store_update$`Ngày tháng năm sinh`
msr_store_update$store_doe_new <- NA


## Create salerep_id

#msr_store_update$salerep_lb_new <- msr_store_update$`Tên NVBH Sales Rep`
msr_store_update$salerep_lb_new <- msr_store_update$`Tên NVBH Sales Rep `
msr_store_update <- left_join(msr_store_update, select(msr_staff, salerep_id_new = staff_id, salerep_lb_new = staff_lb,
                                                       asm_id_new = supasm_id, asm_lb_new = supasm_lb,
                                                       rsm_id_new = suprsm_id, rsm_lb_new = suprsm_lb), by = "salerep_lb_new")


## Create store_status

msr_store_update$store_status_new <- msr_store_update$`Trạng Thái KH`
msr_store_update$store_status_new <- toupper(msr_store_update$store_status_new)

## Create store_owner_name

msr_store_update$store_owner_name_new <- msr_store_update$`Họ & Tên`
msr_store_update <- mutate(msr_store_update, 
                           store_owner_name_new = ifelse(is.na(store_owner_name_new), 
                                                         ".", store_owner_name_new))

## Create region_id

region <- distinct(select(msr_store, region_id_new = region_id, region_lb_new = region_lb))
msr_store_update$region_lb_new <- msr_store_update$`Khu vực`
msr_store_update <- left_join(msr_store_update, region, by = "region_lb_new")

## Create store_type_id

type_id_dict <- c("A", "B", "C")
type_lb_dict <- c("Lớn", "Vừa", "Nhỏ")
type_dict <- make_dict(type_id_dict, type_lb_dict)

msr_store_update$store_type_id <- msr_store_update$`Loại KH`
msr_store_update$store_type_lb <- msr_store_update$store_type_id
msr_store_update$store_type_lb <- encode(msr_store_update$store_type_lb, type_dict)

## Check existed store

msr_store_not_check <- filter(msr_store, !city_id %in% msr_store_update$city_id_new)
msr_store_check <- filter(msr_store, city_id %in% msr_store_update$city_id_new)

msr_store_check <- left_join(msr_store_check, msr_store_update, 
                             by = c("store_lb", "store_phone", "store_type_id", "store_type_lb"))

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
    data$store_owner_dob <- data$store_owner_dob_new
    data$store_owner_name <- data$store_owner_name_new
    data$visitday_id <- data$visitday_id_new
    data$visitday_lb <- data$visitday_lb_new
    data$salerep_id <- data$salerep_id_new
    data$salerep_lb <- data$salerep_lb_new
    data$asm_id <- data$asm_id_new
    data$asm_lb <- data$asm_lb_new
    data$rsm_id <- data$rsm_id_new
    data$rsm_lb <- data$rsm_lb_new
    data
}

if (nrow(msr_store_on) > 0) {
    
    msr_store_on <- replace_data(msr_store_on)
    
}

### Change stores have store_id but not in file update by store_status = OFF

msr_store_off <- filter(msr_store_check, is.na(store_add_new))
msr_store_off <- mutate(msr_store_off, store_status = "OFF")

### Stores haven't store_id

msr_store_not_exist <- anti_join(msr_store_update, msr_store_check, 
                                 by = c("store_lb", "store_phone", "store_type_id", "store_type_lb"))
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
    x <- mutate(x, id = paste0(city_split, sprintf("%05d", 1:nrow(x))))
})
msr_store_final <- do.call("rbind", msr_store_final)

msr_store_final <- mutate(msr_store_final, store_id = ifelse(is.na(store_id), id, store_id))

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
