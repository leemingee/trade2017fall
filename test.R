library(xgboost)
library(data.table)

fraud <- fread("Fraud_Data.csv", stringsAsFactors = F)
country <- fread("IpAddress_to_Country.csv")

unique.countries <- unique(country$country)
ip = fraud$ip_address[1:5]

map_ip_to_country <- function(ip){
  simplify2array(sapply(ip, extract_country))
}

extract_country <- function(single.ip){
  single.country <- country[lower_bound_ip_address < single.ip &
            upper_bound_ip_address > single.ip,]
  ifelse(nrow(single.country) == 1, single.country$country, NA)
}

map_ip_to_country <- function(ip){
  simplify2array(sapply(ip, extract_country))
}

fraud[, `:=`(country = .(country = map_ip_to_country(ip_address)))]

names(fraud)
