library(readr)

library(dplyr)
t <- read_csv("t.csv")
t <- read_csv("store_1.csv")

checkin <- read_csv("~/MEGAsync/RTA/KINGSMEN/data/MSR_CHECKIN_DEMO/MSR_CHECKIN_DEMO.csv")

full <- t[, c(1:14)]
check <- t[, c(15:29)]

hau_check <- filter(check, salerep_id == "DGWKM0065")
cuong_check <- filter(check, salerep_id == "DGWKM0047")

hau_full <- filter(full, s_id == "DGWKM0065")
cuong_full <- filter(full, s_id == "DGWKM0047")

cuong_final <- left_join(cuong_full, cuong_check, 
                         by = c("phone" = "store_phone", "visit" = "visitday_id", "type" = "store_type_id"))

hau_full$lb <- stringi::stri_trans_totitle(hau_full$lb)
hau_check$store_lb <- stringi::stri_trans_totitle(hau_check$store_lb)

hau_final <- left_join(hau_full, hau_check, 
                         by = c("visit" = "visitday_id", "lb" = "store_lb"))
hau_final[is.na(hau_final)] <- ""
cuong_final[is.na(cuong_final)] <-""

write.csv(hau_final, "hau.csv")


hau_full$add <- stringi::stri_trans_totitle(hau_full$add)

hau_check$store_add <- stringi::stri_trans_totitle(hau_check$store_add)

hau_final1 <- left_join(hau_full, hau_check, 
                       by = c("lb"="store_lb", "add"="store_add"))

cuong_checkin <- filter(checkin, staff_id == "DGWKM0047")
cuong_test <- data.frame(id = unique(cuong_checkin$store_id))
hau_checkin <- filter(checkin, staff_id == "DGWKM0065")
hau_test <- data.frame(id = unique(hau_checkin$store_id))


test <- read_csv("Untitled 1.csv")
test1 <- select(test, lb)
test2 <- select(test, store_lb, check)
tx <- left_join(test1, test2, by = c("lb"="store_lb"))

library(readr)
library(dplyr)
t <- read_csv("tx.csv")
full <- t[, c(1:28)]
check <- t[, c(29:56)]
full$add <- stringi::stri_trans_totitle(full$add)
check$store_add <- stringi::stri_trans_totitle(check$store_add)
full$lb <- stringi::stri_trans_totitle(full$lb)
check$store_lb <- stringi::stri_trans_totitle(check$store_lb)

tx <- left_join(full, check, 
                by = c("phone" = "store_phone"))
