# Resource   : UPDATE_MCP	 
# Author     : rta_dieulinh
# Date       : February 27th, 2018

#-------------------------------------------------------
## Load packages 
#-------------------------------------------------------

pkgs <- c("readxl", "tidyr", "RSQLite", "jsonlite", "dplyr", "RTA", "lubridate")
sapply(pkgs, require, character.only = TRUE)
options(stringsAsFactors = FALSE)
options(scipen = 999)
sys_user <- Sys.info()["user"]

#-------------------------------------------------------
## Get function
#-------------------------------------------------------

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

remove_space <- function(data) {
    out <- lapply(data, function(x) gsub("^ *| (?<= ) | *$", "", x, perl=T))
    out <- as.data.frame(out, stringsAsFactors = FALSE)
    out
}


##-------------------------------------------------------
## Get input data
##-------------------------------------------------------

switch(
    sys_user,
    dieulinh = {
        data <- "~/MEGAsync/MCP_UPDATE.xlsx"
    }, {
        data <- "MCP_UPDATE.xlsx"
    }
)		


msr_store_update <- tryCatch(
    read_excel(data),
    error = function(x) {
        stop('File phải có tên dưới dạng "MCP_UPDATE.xlsx"', call. = FALSE)
    }
)
#msr_store <- output1
#msr_store <- get_dm_data("C011", "test_mcp")
msr_store <- get_dm_data("C011", "MSR_mcp_store")
msr_store$update_time_on <- Sys.Date()
msr_staff <- get_dm_data("C011", "MSR_staff")
province <- get_dm_data("C011", "km_vnadmin3")
region_city <- get_dm_data("C011", "kingsmen_province")

province <- select(province, city_id = tinh_id, city_lb = tinh_name) %>% unique()


##-------------------------------------------------------
## Get name column
##-------------------------------------------------------

col <- c("store_lb_new", "city_id_new",
         "region_id_new")

namecol <- c("Tên KH", "Mã Tỉnh/Thành Phố",
             "Mã Khu vực")

name_col <- make_dict(col, c("Tên KH", "Mã Tỉnh/Thành Phố",
                             "Mã Khu vực"))


cols <- c("store_hnumber_new", "store_street_new", "store_ward_lb_new", 
          "store_district_lb_new", "store_type_id_new", 
          "visitday_id_mon", "visitday_id_tue", "visitday_id_wed", 
          "visitday_id_thu", "visitday_id_fri", "visitday_id_sat", 
          "store_owner_name_new",
          "store_owner_dob_new", "store_phone_new", 
          "store_status_new", "username_new", 
          "dgw_store_id_new", "store_id_new",col)

namecols <- c("Số Nhà", "Đường", 
              "Phường/Xã", "Quận/Huyện", "Loại KH", 
              "Thứ 2", "Thứ 3", "Thứ 4", "Thứ 5", "Thứ 6", "Thứ 7",
              "Họ & Tên", "Ngày tháng năm sinh", "Số ĐT",
              "Trạng Thái KH", "rtWork Account", 
              "Mã KH", "MÃ KH RTA", namecol)

name_cols <- make_dict(cols, c("Số Nhà", "Đường", 
                               "Phường/Xã", "Quận/Huyện", "Loại KH", 
                               "Thứ 2", "Thứ 3", "Thứ 4", "Thứ 5", "Thứ 6", "Thứ 7",
                               "Họ & Tên", "Ngày tháng năm sinh", "Số ĐT",
                               "Trạng Thái KH", "rtWork Account",  
                               "Mã KH", "MÃ KH RTA", name_col))


##-------------------------------------------------------
## Remove double space in data
##-------------------------------------------------------

msr_store[msr_store == ""] <- NA
msr_staff <- filter(msr_staff, !grepl("^rta|thiendao", username))
msr_staff[msr_staff == ""] <- NA

msr_store <- remove_space(msr_store)
msr_staff <- remove_space(msr_staff)
msr_store_update <- remove_space(msr_store_update)

msr_store_update <- mutate_all(msr_store_update, funs(toupper))
msr_store$region_id <- toupper(msr_store$region_id)
msr_staff$username <- toupper(msr_staff$username)
province$city_id <- toupper(province$city_id)

##-------------------------------------------------------
## Create column name from Viet's label
##-------------------------------------------------------

test_col_v <- apply(msr_store_update, 1, function(x) any(x %in% c("MÃ KH", "TÊN KH", "ASM")))
test_col_e <- all(cols %in% names(msr_store_update))

if (test_col_e == TRUE) {
    
    msr_store_update <- msr_store_update
    
} else if (test_col_e == FALSE) {
    
    if (any(test_col_v == TRUE)) {
        msr_store_update[test_col_v == TRUE, ] <- toupper(msr_store_update[test_col_v == TRUE, ])
        namecols <- toupper(namecols)
        colnames(msr_store_update) <- msr_store_update[test_col_v == TRUE, ]
        col_dict <- make_dict(namecols, cols)
        names(msr_store_update) <- encode(names(msr_store_update), col_dict)
        
    }
}

##-------------------------------------------------------
## Add warning to user
##-------------------------------------------------------

### Warning thiếu cột   

if (any(!cols %in% names(msr_store_update))) {
    col_exist_e <- cols[!cols %in% names(msr_store_update)]
    col_exist_v <- name_cols[!cols %in% names(msr_store_update)]
    stop(paste0("Bảng dữ liệu thiếu cột ", col_exist_v, " (", col_exist_e, ").\n"), call. = FALSE)
}

### Warning store_id bị trùng

if (any(duplicated(msr_store_update$store_id_new))) {
    id_double <- msr_store_update$store_id_new[duplicated(msr_store_update$store_id_new)]
    id_double <- unique(id_double[!is.na(id_double)])
    if (length(id_double) > 0) {
        stop(paste0('Bảng dữ liệu ghi nhận Mã KH "', id_double, '" bị trùng nhau.\n'), call. = FALSE)
    }
}


## Remove row NA, col NA
msr_store_update <- msr_store_update[test_col_v != TRUE, ]
msr_store_update <- msr_store_update[,!names(msr_store_update) %in% ""]
msr_store_update <- msr_store_update[,!is.na(colnames(msr_store_update))]
msr_store_update <- msr_store_update[, !grepl("^NA.", names(msr_store_update))]
msr_store_update <- msr_store_update[, !grepl(".1", names(msr_store_update))]

row_all_na <- apply(msr_store_update, 1, function(x) all(is.na(x)))
msr_store_update <- msr_store_update[!row_all_na,]


## Remove row 2 in excel
msr_store_update <- msr_store_update[-1,]
#msr_store_update <- filter(msr_store_update, !store_lb_new %in% "Tên KH")

### Warning thiếu thông tin

if (any(is.na(msr_store_update[, col]))) {
    name_col_na <- names(msr_store_update)[apply(msr_store_update, 1, anyNA)]
    col_na_v <- name_col[col %in% name_col_na]
    col_na_e <- col[col %in% name_col_na]
    
    n <- which(!rowSums(!is.na(msr_store_update)), arr.ind=TRUE) ##rows have all NA value
    n <- as.data.frame(n, stringsAsFactors = FALSE)
    n1 <- which(is.na(msr_store_update[, col_na_e]), arr.ind=TRUE) ##rows have any NA value
    n1 <- as.data.frame(n1, stringsAsFactors = FALSE)
    n2 <- n1[!n1$row %in% n$n, ] ## remove case all NA value
    
    n2$name <- col_na_v[n2$col]
    n2$lb <- msr_store_update$store_lb_new[n2$row]
    
    not_store_lb <- filter(n2, is.na(n2$lb))
    not_of_othercol <- filter(n2, !is.na(n2$lb))
    
    if (nrow(not_store_lb) > 0) {
        stop(paste0("Cột Tên KH thiếu thông tin.\n"), call. = FALSE)
    }
    
    
    if (nrow(not_of_othercol) > 0) {
        stop(paste0("Cột ", n2$name, " - tên KH ", n2$lb, " thiếu thông tin.\n"), call. = FALSE)
    }
}

### Warning dữ liệu không có trong hệ thống

if (any(!msr_store_update$store_id_new %in% msr_store$store_id)) {
    store_exist <- msr_store_update$store_id_new[!msr_store_update$store_id_new %in% msr_store$store_id]
    store_exist <- unique(store_exist[!is.na(store_exist)])
    if (length(store_exist) > 0) {
        stop(paste0('Dữ liệu Mã KH RTA "', store_exist, '" không có trong hệ thống.\n'), call. = FALSE)
    }
}

if (any(!msr_store_update$region_id_new %in% msr_store$region_id)) {
    region_exist <- msr_store_update$region_id_new[!msr_store_update$region_id_new %in% msr_store$region_id]
    region_exist <- unique(region_exist[!is.na(region_exist)])
    if (length(region_exist) > 0) {
        stop(paste0('Dữ liệu Mã Khu vực "', region_exist, '" không có trong hệ thống.\n'), call. = FALSE)
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
        staff_exist <- tolower(staff_exist)
        stop(paste0('Dữ liệu rtWork Account "', staff_exist, '" không có trong hệ thống.\n'), call. = FALSE)
    }
}

##-------------------------------------------------------
## Filter store test
##-------------------------------------------------------

msr_store_test <- filter(msr_store, grepl("^RTA|TEST", store_id))
msr_store <- filter(msr_store, !grepl("^RTA|TEST", store_id))
msr_store_timeon <- select(msr_store, store_id, update_time_on)
msr_store <- select(msr_store, -update_time_on)

##-------------------------------------------------------
## Create city_lb
#-------------------------------------------------------

msr_store_update <- left_join(msr_store_update, 
                              select(province, city_id_final = city_id, city_lb_final = city_lb), 
                              by = c("city_id_new" = "city_id_final"))

msr_store_update$city_lb_new <- msr_store_update$city_lb_final

#-------------------------------------------------------
## Create region_lb
#-------------------------------------------------------

store_msr <- RTA::get_dm_data("C011", "MSR_store")
region <- distinct(select(store_msr[!is.na(store_msr$region_lb) & store_msr$region_lb != "", ], 
                          region_id_final = region_id, region_lb_final = region_lb))

region$region_id_new <- toupper(region$region_id_final)

msr_store_update <- left_join(msr_store_update, 
                              select(region, -region_id_final), 
                              by = "region_id_new")
msr_store_update$region_lb_new <- msr_store_update$region_lb_final


#-------------------------------------------------------
## Warning in region of HCM
#-------------------------------------------------------

city_region <- region_city %>%
    select(city_id, region_id) %>% 
    mutate(region_id = strsplit(region_id, " ")) %>%
    unnest(region_id) %>% 
    mutate(city_region = toupper(paste_NA(city_id, region_id, sep = "_")))

msr_store_update <- mutate(msr_store_update, city_region = paste_NA(city_id_new, region_id_new, sep = "_"))

if (any(msr_store_update$city_id_new %in% "HCM")) {
    if(any(!msr_store_update$city_region %in% city_region$city_region)) {
        city <- msr_store_update$city_id_new[!msr_store_update$city_region %in% city_region$city_region]
        city <- unique(city[!is.na(city)])
        
        region <- msr_store_update$region_id_new[!msr_store_update$city_region %in% city_region$city_region]
        region <- unique(region[!is.na(region)])
        stop(paste0("Khu vực ", region, " không bao gồm Mã Tỉnh/Thành Phố ", city, ".\n"), call. = FALSE)
    }
}

#if (any(msr_store_update$city_id_new %in% "HCM")) {
#    if(any(!msr_store_update$city_region %in% city_region$city_region)) {
#        city_id <- msr_store_update$city_id_new[!msr_store_update$city_region %in% city_region$city_region]
#        city_lb <- unique(region_city$city_lb[region_city$city_id %in% city_id])
#        region <- msr_store_update$region_id_new[!msr_store_update$city_region %in% city_region$city_region]
#        region <- unique(region[!is.na(region)])
#        stop(paste0("Khu vực ", region, " không bao gồm Tỉnh/Thành Phố ", city_lb, ".\n"))
#    }
#}

if (nrow(msr_store_update) == 0) {
    stop("Bảng không có dữ liệu để cập nhật.")
}

#-------------------------------------------------------
## Create store_add
#-------------------------------------------------------

col_add <- c("store_street_new", "store_ward_lb_new","store_district_lb_new", "city_lb_new")

msr_store_update$store_add_new <- paste_NA(list("SN:", msr_store_update$store_hnumber_new), sep = " ")

msr_store_update$store_add_new <- paste_NA(list(msr_store_update$store_add_new,
                                                msr_store_update[, col_add]),
                                           sep = " - ")

#-------------------------------------------------------
## Change phonenumber
#-------------------------------------------------------

msr_store_update$store_phone_new <- gsub("\\.|,|\\s|-|\\*|`|:|\\\\|\\(.*\\)", "", msr_store_update$store_phone_new)
msr_store_update$store_phone_new <- stringr::str_replace_all(msr_store_update$store_phone_new, "[^[:digit:]]", "")
msr_store_update$store_phone_new <- ifelse(grepl("^0", msr_store_update$store_phone_new), 
                                           msr_store_update$store_phone_new,
                                           paste_NA(list("0", msr_store_update$store_phone_new), sep = ""))

#-------------------------------------------------------
## Create visitday_id + visitday_lb
#-------------------------------------------------------

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

visit_id_dict <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
visit_lb_dict <- c("Thứ 2", "Thứ 3", "Thứ 4", "Thứ 5", "Thứ 6", "Thứ 7")
visit_dict <- make_dict(visit_id_dict, visit_lb_dict)


for(i in 1:length(visit_lb_dict)){
    msr_store_update$visitday_lb_new <- 
        gsub(visit_id_dict[i], visit_lb_dict[i], msr_store_update$visitday_lb_new)
}


#-------------------------------------------------------
## Create salerep_id
#-------------------------------------------------------

msr_store_update <- left_join(msr_store_update, select(msr_staff, username, staff_id, staff_lb), 
                              by = c("username_new" = "username"))
msr_store_update$salerep_id_new <- msr_store_update$staff_id
msr_store_update$salerep_lb_new <- msr_store_update$staff_lb

#-------------------------------------------------------
## Create store_status
#-------------------------------------------------------

msr_store_update$store_status_new <- ifelse(is.na(msr_store_update$store_status_new), 
                                            "On", msr_store_update$store_status_new)
msr_store_update$store_status_new <- toupper(msr_store_update$store_status_new)
status_dict <- make_dict(c("ON", "ON1/2", "OFF"), c("ON", "ON", "OFF"))
msr_store_update$store_status_new <- encode(msr_store_update$store_status_new, status_dict)

#-------------------------------------------------------
## Create store_owner_name
#-------------------------------------------------------

msr_store_update <- mutate(msr_store_update, 
                           store_owner_name_new = ifelse(is.na(store_owner_name_new), 
                                                         ".", store_owner_name_new))

#-------------------------------------------------------
## Create store_type_id + store_type_lb
#-------------------------------------------------------

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
                                                      ".", store_type_id_new))

#-------------------------------------------------------
## Remove duplicated store
#-------------------------------------------------------

msr_store_update <- msr_store_update[!duplicated(msr_store_update[,c("store_lb_new", 
                                                                     "store_add_new", 
                                                                     "store_type_id_new", 
                                                                     "store_owner_name_new",
                                                                     "store_phone_new", 
                                                                     "visitday_id_new")]), ]


#-------------------------------------------------------
## Create update_time
#-------------------------------------------------------

msr_store_update$update_time_new <- Sys.Date()
msr_store_update$update_hour_new <- Sys.time() 


#-------------------------------------------------------
## Check existed store
#-------------------------------------------------------

### Store not update
msr_store_not_check <- filter(msr_store, !city_id %in% msr_store_update$city_id_new 
                              & !salerep_id %in% msr_store_update$salerep_id_new |
                                  !city_id %in% msr_store_update$city_id_new 
                              & salerep_id %in% msr_store_update$salerep_id_new |
                                  city_id %in% msr_store_update$city_id_new 
                              & !salerep_id %in% msr_store_update$salerep_id_new)

msr_store_not_check$update_time <- as.Date(msr_store_not_check$update_time)
msr_store_not_check$update_hour <- as.POSIXct(msr_store_not_check$update_hour)
#msr_store_not_check$update_time_off <- msr_store_not_check$update_time
msr_store_not_check$update_time_off <- msr_store_not_check$update_hour

### Store updated

msr_store_check <- filter(msr_store, city_id %in% msr_store_update$city_id_new 
                          & salerep_id %in% msr_store_update$salerep_id_new)

msr_store_check$update_time <- as.Date(msr_store_check$update_time)
msr_store_check$update_hour <- as.POSIXct(msr_store_check$update_hour)

msr_store_checkon <- left_join(msr_store, msr_store_update, 
                               by = c("store_id" = "store_id_new"))

msr_store_checkoff <- left_join(msr_store_check, msr_store_update, 
                                by = c("store_id" = "store_id_new"))

### Existed store have store_id

msr_store_on <- filter(msr_store_checkon, !is.na(store_add_new))

replace_data <- function(data) {
    data$store_lb <- data$store_lb_new
    data$dgw_store_id <- data$dgw_store_id_new
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
    data$update_hour <- ymd_hms(data$update_hour_new)
    data$update_time_off <- Sys.Date()
    #data$update_time_off <- ymd_hms(Sys.time())
    data
}

if (nrow(msr_store_on) > 0) {
    
    msr_store_on <- replace_data(msr_store_on)
    msr_store_on$update_time <- as.Date(msr_store_on$update_time)
    
}

### Change stores have store_id but not in file update by store_status = OFF

msr_store_off <- filter(msr_store_checkoff, is.na(store_add_new))
msr_store_off <- mutate(msr_store_off, store_status_new = "OFF")

msr_store_off <- mutate(msr_store_off, 
                        update_time = ifelse(store_status == store_status_new, 
                                             update_time, Sys.Date()),
                        update_hour = ifelse(store_status == store_status_new, 
                                             update_hour, Sys.time()),
                        update_time_off = ifelse(store_status == store_status_new, 
                                                  update_time, Sys.Date())
                        #update_time_off = ifelse(store_status == store_status_new, 
                        #                         update_hour, Sys.time())
)
#msr_store_off$update_time_off <- as.POSIXct.numeric(msr_store_off$update_time_off, 
#                                                    origin = "1970-01-01 00:00:00")

msr_store_off$update_time_off <- as.Date.numeric(msr_store_off$update_time_off, 
                                                  origin = "1970-01-01")

msr_store_off$update_time <- as.Date.numeric(msr_store_off$update_time, 
                                             origin = "1970-01-01")

msr_store_off$update_hour <- as.POSIXct.numeric(msr_store_off$update_hour, 
                                                origin = "1970-01-01 00:00:00")

msr_store_off$store_status <- msr_store_off$store_status_new
### Filter store not store_id

msr_store_not_exist <- anti_join(msr_store_update, msr_store, 
                                 by = c("store_id_new" = "store_id"))

if (nrow(msr_store_not_exist) > 0) {
    
    msr_store_not_exist <- replace_data(msr_store_not_exist)
    msr_store_not_exist$update_time <- as.Date(msr_store_not_exist$update_time)
    msr_store_not_exist$update_time_on <- Sys.Date()
    
}

if (nrow(msr_store_not_exist) == 0) {
    msr_store_not_exist$update_time <- msr_store_not_exist$update_time_new
    msr_store_not_exist$update_hour <- msr_store_not_exist$update_hour_new
    #msr_store_not_exist$update_time_off <- msr_store_not_exist$update_hour
    msr_store_not_exist$update_time_off <- msr_store_not_exist$update_time
    msr_store_not_exist$update_time_on <- msr_store_not_exist$update_time
}
### Bind row final store

msr_store_not_exist[] <- lapply(msr_store_not_exist[], as.character)
msr_store_not_check[] <- lapply(msr_store_not_check[], as.character)
msr_store_on[] <- lapply(msr_store_on[], as.character)
msr_store_off[] <- lapply(msr_store_off[], as.character)

msr_store_final <- bind_rows(msr_store_not_check, msr_store_on, msr_store_off)
msr_store_final <- left_join(msr_store_final, msr_store_timeon, by = "store_id")
msr_store_final <- bind_rows(msr_store_final, msr_store_not_exist)

#-------------------------------------------------------
## Create case if city in HCM
#-------------------------------------------------------
## Store_id của HCM có cấu trúc chia theo 4 vùng và bán sỉ/theo chuỗi nên tạo cột city mới để nối với STT

msr_store_final$region_hcm <- msr_store_final$region_id
region_hcm_dict <- make_dict(toupper(c("H1", "H2", "H3", "H4", "CHCM", "SiH")), 
                             c("HCM1", "HCM2", "HCM3", "HCM4", "HCMC", "HCMS"))
msr_store_final$region_hcm <- encode(msr_store_final$region_hcm, region_hcm_dict)
msr_store_final <- mutate(msr_store_final, city_split = ifelse(city_id %in% "HCM", 
                                                               region_hcm, paste0(city_id, "0")))

#-------------------------------------------------------
## Create store_id
#-------------------------------------------------------

msr_store_final <- split(msr_store_final, msr_store_final$city_split)
msr_store_final <- lapply(msr_store_final, function(x) {
    x <- x[order(x$store_id, na.last = TRUE),]
    
    #substrRight <- function(x, n){
    #substr(x, nchar(x)-n+1, nchar(x))
    #   substring(x, n)
    #}
    
    x$b <- as.double(substring(x$store_id, 5)) ## Lấy STT từ store id cũ
    
    my.max <- function(x) ifelse(!all(is.na(x)), max(x, na.rm=T), NA)
    x$max <- my.max(x$b) ## Lấy STT lớn nhất
    
    for (i in unique(is.na(x$b))) {
        x$id_na <- NA
        x$id_na[is.na(x$b) == i] <- seq_len(sum(is.na(x$b) == i)) 
    }
    
    x$a <- ifelse(is.na(x$b), rowSums(cbind(x$max, x$id_na), na.rm = TRUE), x$b) ## tạo thứ tự cho NT chưa có id
    
    
    
    pad_zero <- function(x) {
        
        if (nchar(as.character(x)) < 5 & x != "") {
            len <- nchar(as.character(x))
            x <- paste0(paste0(rep("0", 5 - len), collapse = ""), x)
        } else {
            x <- format(x, scientific = FALSE)
        }
        x
    }
    
    x$a <- vapply(x$a, pad_zero, "")
    
    x$store_id_check <- paste0(x$city_split, x$a) ## Map với mã tỉnh/ khu vực mới
    x
    
})
msr_store_final <- do.call("rbind", msr_store_final)

msr_store_final <- mutate(msr_store_final, store_id = ifelse(is.na(store_id), store_id_check, store_id))

#-------------------------------------------------------
## Create new column
#-------------------------------------------------------

msr_store_final$store_ward_id <- NA
msr_store_final$store_district_id <- NA
msr_store_final$store_doe <- NA

#-------------------------------------------------------
## Map with sup id
#-------------------------------------------------------

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

#-------------------------------------------------------
## Create supall_id
#-------------------------------------------------------

staff <- filter(msr_staff, !staff_status %in% "Inactive")
staff <- filter(staff, !grepl("DGWKM0005|DGWKM0094|DGWKM0130", staff_id))
sup_sd <- c(staff$staff_id[staff$user_role %in% "SD"])
sup_admin <- c(staff$staff_id[staff$user_role %in% "ADMIN"])
sup_all <- paste(unlist(strsplit(c(sup_sd, sup_admin), " ")), collapse = " ")

msr_store_final$supall_id <- paste_NA(c(msr_store_final[, c("asm_id", "rsm_id", "sd_id")], sup_all), sep = " ")

msr_store_final$supall_id <- sapply(msr_store_final$supall_id, 
                                    function(x) paste(unique(unlist(strsplit(x, " "))), collapse = " "))

msr_store_test$update_time <- as.Date(msr_store_test$update_time)
msr_store_test$update_hour <- as.POSIXct(msr_store_test$update_hour)
#msr_store_test$update_time_off <- msr_store_test$update_hour
msr_store_test$update_time_off <- msr_store_test$update_time
msr_store_test$update_time_on <- msr_store_test$update_time

msr_store_test[] <- lapply(msr_store_test[], as.character)

#-------------------------------------------------------
## Create store_marker
#-------------------------------------------------------

msr_store_final <- bind_rows(msr_store_final, msr_store_test)
msr_store_final$store_marker <- 1

msr_store_final <- left_join(msr_store_final, region, by = c("region_id" = "region_id_new"))
msr_store_final$region_id <- msr_store_final$region_id_final

#-------------------------------------------------------
## Final table
#-------------------------------------------------------

msr_store_final <- arrange(msr_store_final, desc(update_hour))


final <- msr_store_final %>% 
    select(store_id, dgw_store_id, store_lb,
           store_hnumber, store_street, store_ward_id, store_ward_lb,
           store_district_id, store_district_lb, city_id, city_lb,
           region_id, region_lb, 
           store_add, store_phone, store_owner_dob, 
           store_owner_name, store_doe, 
           store_type_id, store_type_lb,
           visitday_id, visitday_lb, store_status,
           salerep_id, salerep_lb, 
           asm_id, asm_lb, rsm_id, rsm_lb, 
           sd_id, sd_lb, update_time, update_hour, 
           update_time_off, update_time_on,
           store_marker, supall_id)

final <- final %>% 
    arrange(desc(update_hour)) %>% 
    filter(!duplicated(store_id))

#final$id <- 1:nrow(final)
final[is.na(final)] <- ""
final[] <- lapply(final[], as.character)

#-------------------------------------------------------
## Handle script
#-------------------------------------------------------

output <- final


#-------------------------------------------------------
## Save result
#-------------------------------------------------------

write.csv(output, 'result.csv', row.names = FALSE)
