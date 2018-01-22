# this is for creating data.sample for the presentation in shiny
# generated from the origin.RData

load("Origin.RData")

library(dplyr)
total <- rbind(older, newer)
sample.total <- total %>% group_by(Year) %>% sample_n(size = 500)

write.csv(sample.total, "sample.total.csv")
