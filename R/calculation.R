

#' @return predictor output for UUP
#' @export 


eb_number <- function(LTABC, LTCTWO, abcweight,ctwodeweight){
  
  library(dplyr)
  load("~/TerranceONeill/data/Dataset_3004_2011_1_.rda")
  
  eb_ds <- Dataset_3004_2011_1_ %>% 
    filter(Dataset_3004_2011_1_$Ward %in% c('Ballyhackamore', 'Ballymacarrett', 'Belmont', 'Bloomfield (Belfast LGD)', 'Cherryvalley', 'Island', 'Knock', 'Orangefield', 'Stormont', 'Sydenham','The Mount', 'Ballyhanwood', 'Carrowreagh', 'Cregagh', 'Downshire', 'Dundonald', 'Enler', "Gilnahirk", "Graham's Bridge", 'Lisnasharragh', 'Lower Braniel', 'Tullycarnet','Upper Braniel'))
  
  eb_ds$abcone <- eb_ds$`Approximated social grade of HRP aged 16-64 years: AB` + eb_ds$`Approximated social grade of HRP aged 16-64 years: C1`
  eb_ds$ctwode <- eb_ds$`Approximated social grade of HRP aged 16-64 years: C2` + eb_ds$`Approximated social grade of HRP aged 16-64 years: DE`
  
  #lucidtalk numbers
  LTABC = LTABC
  LTCTWO = LTCTWO
  
  eb_ds$adbc_unweighted <- eb_ds$abcone * LTABC
  eb_ds$ctwode_unweighted <- eb_ds$ctwode * LTCTWO
  #adjusted census 2011 with no exponetial growth
  Religion <- c("None", "Catholic", "Protestant", "Other")
  Number <- c(0.22, 0.41, 0.37, 0.01)
  
  demographic <- cbind(Religion, Number)
  demographic <- as.data.frame(demographic)
  #assembly info demogaphic eb with no exponetial growth
  eb_religion <- c("None", "Catholic", "Protestant", "Other")
  eb_number <- c(0.12, 0.13, 0.72,0.03)
  
  eb_demographic <- cbind(demographic, eb_number) 
  eb_demographic$Number <- as.numeric(eb_demographic$Number)
  eb_demographic$ratio <- eb_demographic$eb_number/eb_demographic$Number 
  
  eb_ds$weighted_abc <- ((eb_ds$adbc_unweighted*0.12)*0.545) + 
    ((eb_ds$adbc_unweighted*0.13)*0.317) +
    ((eb_ds$adbc_unweighted*0.72)*1.945) +
    ((eb_ds$adbc_unweighted*0.03)*3)  
  
  eb_ds$weighted_ctwode <- ((eb_ds$ctwode_unweighted*0.12)*0.545) + 
    ((eb_ds$ctwode_unweighted*0.13)*0.317) +
    ((eb_ds$ctwode_unweighted*0.72)*1.945) +
    ((eb_ds$ctwode_unweighted*0.03)*3)  
  
  eb_ds$support_by_ward_raw <- (eb_ds$weighted_abc*abcweight) + (eb_ds$weighted_ctwode*ctwodeweight)
  
  eb_ds$percentage_ward <- eb_ds$support_by_ward_raw / (eb_ds$abcone + eb_ds$ctwode)
  
  eb_ds[3] <- NULL
  
  return(eb_ds)}

load_census <- function(){
  load(file="~/TerranceONeill/data/Dataset_3004_2011_1_.rda")
  return(Dataset_3004_2011_1_)
}


#' @return predictor output for UUP
#' @export titanic_number
titanic_number <- function(LTABC, LTCTWO, abcweight,ctwodeweight){
  library(dplyr)
  
  eb_ds <- load_census()
  
  eb_ds <- eb_ds %>%
    filter(eb_ds$Ward %in% c('Ballymacarrett', 
                             'Bloomfield (Belfast LGD)', 
                             'Connswater', 
                             'Orangefield', 'Sydenham','The Mount'))
  
  eb_ds$abcone <- eb_ds$`Approximated social grade of HRP aged 16-64 years: AB` + eb_ds$`Approximated social grade of HRP aged 16-64 years: C1`
  eb_ds$ctwode <- eb_ds$`Approximated social grade of HRP aged 16-64 years: C2` + eb_ds$`Approximated social grade of HRP aged 16-64 years: DE`
  
  #lucidtalk numbers
  LTABC = LTABC
  LTCTWO = LTCTWO
  
  eb_ds$adbc_unweighted <- eb_ds$abcone * LTABC
  eb_ds$ctwode_unweighted <- eb_ds$ctwode * LTCTWO
  #adjusted census 2011 with no exponetial growth
  Religion <- c("None", "Catholic", "Protestant", "Other")
  Number <- c(0.22, 0.41, 0.37, 0.01)
  
  demographic <- cbind(Religion, Number)
  demographic <- as.data.frame(demographic)
  #assembly info demogaphic eb with no exponetial growth
  eb_religion <- c("None", "Catholic", "Protestant", "Other")
  eb_number <- c(0.12, 0.13, 0.72,0.03)
  
  eb_demographic <- cbind(demographic, eb_number) 
  eb_demographic$Number <- as.numeric(eb_demographic$Number)
  eb_demographic$ratio <- eb_demographic$eb_number/eb_demographic$Number 
  
  eb_ds$weighted_abc <- ((eb_ds$adbc_unweighted*0.12)*0.545) + 
    ((eb_ds$adbc_unweighted*0.13)*0.317) +
    ((eb_ds$adbc_unweighted*0.72)*1.945) +
    ((eb_ds$adbc_unweighted*0.03)*3)  
  
  eb_ds$weighted_ctwode <- ((eb_ds$ctwode_unweighted*0.12)*0.545) + 
    ((eb_ds$ctwode_unweighted*0.13)*0.317) +
    ((eb_ds$ctwode_unweighted*0.72)*1.945) +
    ((eb_ds$ctwode_unweighted*0.03)*3)  
  
  eb_ds$support_by_ward_raw <- (eb_ds$weighted_abc*abcweight) + (eb_ds$weighted_ctwode*ctwodeweight)
  
  eb_ds$percentage_ward <- eb_ds$support_by_ward_raw / (eb_ds$abcone + eb_ds$ctwode)
  
  eb_ds[3] <- NULL
  
  return(eb_ds)}

#' @return predictor output for UUP
#' @export sb_number
sb_number <- function(LTABC, LTCTWO, abcweight,ctwodeweight){
  
  library(dplyr)
  load("~/TerranceONeill/data/Dataset_3004_2011_1_.rda")
  
  sb_ds <- Dataset_3004_2011_1_ %>% 
    filter(Dataset_3004_2011_1_$Ward %in% c('Beechill', 'Botanic','Belvoir', 'Blackstaff', 'Cairnshill', 'Carryduff East', 'Carryduff West', 'Wynchurch','Cregagh', 'Falls', 'Finaghy', 'Galwally', 'Hillfoot', 'Knockbracken','Malone','Minnowburn','Musgrave','Newtownbreda', 'Shaftesbury', 'Ravenhill', 'Rosetta','Stranmillis','Upper Malone', 'Windsor','Woodstock'))
  sb_ds$abcone <- sb_ds$`Approximated social grade of HRP aged 16-64 years: AB` + sb_ds$`Approximated social grade of HRP aged 16-64 years: C1`
  sb_ds$ctwode <- sb_ds$`Approximated social grade of HRP aged 16-64 years: C2` + sb_ds$`Approximated social grade of HRP aged 16-64 years: DE`
  
  #lucidtalk numbers
  LTABC = LTABC
  LTCTWO = LTCTWO
  
  sb_ds$adbc_unweighted <- sb_ds$abcone * LTABC
  sb_ds$ctwode_unweighted <- sb_ds$ctwode * LTCTWO
  #adjusted census 2011 with no exponetial growth
  Religion <- c("None", "Catholic", "Protestant", "Other")
  Number <- c(0.22, 0.41, 0.37, 0.01)
  
  demographic <- cbind(Religion, Number)
  demographic <- as.data.frame(demographic)
  #assembly info demogaphic eb with no exponetial growth
  sb_religion <- c("None", "Catholic", "Protestant", "Other")
  sb_number <- c(0.09, 0.46, 0.42,0.03)
  
  sb_demographic <- cbind(demographic, sb_number) 
  sb_demographic$Number <- as.numeric(sb_demographic$Number)
  sb_demographic$ratio <- sb_demographic$sb_number/sb_demographic$Number 
  
  sb_ds$weighted_abc <- ((sb_ds$adbc_unweighted*0.09)*0.4090909) + 
    ((sb_ds$adbc_unweighted*0.46)*1.1219512) +
    ((sb_ds$adbc_unweighted*0.42)*1.1351351) +
    ((sb_ds$adbc_unweighted*0.03)*3)  
  
  sb_ds$weighted_ctwode <- ((sb_ds$ctwode_unweighted*0.09)*0.4090909) + 
    ((sb_ds$ctwode_unweighted*0.46)*1.1219512) +
    ((sb_ds$ctwode_unweighted*0.42)*1.1351351) +
    ((sb_ds$ctwode_unweighted*0.03)*3)  
  
  sb_ds$support_by_ward_raw <- (sb_ds$weighted_abc*abcweight) + (sb_ds$weighted_ctwode*ctwodeweight)
  
  sb_ds$percentage_ward <- sb_ds$support_by_ward_raw / (sb_ds$abcone + sb_ds$ctwode)
  
  sb_ds[3] <- NULL
  
  return(sb_ds)}

#' @return predictor output for UUP
#' @export str_number
str_number <- function(LTABC, LTCTWO, abcweight,ctwodeweight){
  
  
  load("~/TerranceONeill/data/Dataset_3004_2011_1_.rda")
  
  
  str_ds <- Dataset_3004_2011_1_ %>% 
    filter(Dataset_3004_2011_1_$Ward %in% c('Loughries', "Bradshaw's Brae",'Glen', 'Whitespots', 'Movilla', 'Carrowdore', 'Ballyrainey', 'Central (Ards LGD)','Gregstown', 'Scrabo', 'Comber West', 'Comber North', 'Moneyreagh', 'Comber East','Lisbane','Killinchy','Ballygowan','Portavogie', 'Kircubbin', 'Saintfield', 'Derryboy','Kilmore','Ballymaglave', 'Portaferry','Ballynahinch East', 'Killyleagh'))
  
  str_ds$abcone <- str_ds$`Approximated social grade of HRP aged 16-64 years: AB` + str_ds$`Approximated social grade of HRP aged 16-64 years: C1`
  str_ds$ctwode <- str_ds$`Approximated social grade of HRP aged 16-64 years: C2` + str_ds$`Approximated social grade of HRP aged 16-64 years: DE`
  
  #lucidtalk numbers
  LTABC = LTABC
  LTCTWO = LTCTWO
  
  str_ds$adbc_unweighted <- str_ds$abcone * LTABC
  str_ds$ctwode_unweighted <- str_ds$ctwode * LTCTWO
  #adjusted census 2011 with no exponetial growth
  Religion <- c("None", "Catholic", "Protestant", "Other")
  Number <- c(0.22, 0.41, 0.37, 0.01)
  
  demographic <- cbind(Religion, Number)
  demographic <- as.data.frame(demographic)
  #assembly info demogaphic str with no exponetial growth
  str_religion <- c("None", "Catholic", "Protestant", "Other")
  str_number <- c(0.113, 0.177, 0.70,0.01)
  
  str_demographic <- cbind(demographic, str_number) 
  str_demographic$Number <- as.numeric(str_demographic$Number)
  str_demographic$ratio <- str_demographic$str_number/str_demographic$Number 
  
  str_ds$weighted_abc <- ((str_ds$adbc_unweighted*0.113)*0.5136364) + 
    ((str_ds$adbc_unweighted*0.177)*0.4317073) +
    ((str_ds$adbc_unweighted*0.70)*1.8918919) +
    ((str_ds$adbc_unweighted*0.01)*1)  
  
  str_ds$weighted_ctwode <- ((str_ds$ctwode_unweighted*0.113)*0.5136364) + 
    ((str_ds$ctwode_unweighted*0.177)*0.4317073) +
    ((str_ds$ctwode_unweighted*0.70)*1.8918919) +
    ((str_ds$ctwode_unweighted*0.01)*1)  
  
  str_ds$support_by_ward_raw <- (str_ds$weighted_abc*abcweight) + (str_ds$weighted_ctwode*ctwodeweight)
  
  str_ds$percentage_ward <- str_ds$support_by_ward_raw / (str_ds$abcone + str_ds$ctwode)
  
  str_ds[3] <- NULL
  
  return(str_ds)}

