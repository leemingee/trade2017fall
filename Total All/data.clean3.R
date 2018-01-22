# this is for the data generation for the clustering Panel
# import the data, country.cleaned.csv
# it will output the clustering.import.csv file
# Ming Li, 20171201

country_import <- read.csv("country.cleaned.csv")

countries = as.character(unique(country_import$Country))
years = rep(1996:2017,length(country_import)-2)

create.names <- function(names, rep.time){
  name.vec <- vector()
  for (i in 1:length(names)){
    name.vec <- c(name.vec, rep(names[i], rep.time))
  }
  return(name.vec)
}

# commodities = c(rep("Coffee",21),rep("Tea",21),rep("Spices",21),rep("Chocolate",21),rep("Cocoa",21))

names <- names(country_import)[-c(1,2)]
rep.time <- 2017-1996+1
commodity.names <- create.names(names = names, rep.time = rep.time)

df <- data.frame(matrix(ncol = length(countries)+ 2, nrow = length(years), data = 0))
colnames(df) <- c("Year","Commodity",countries)
df$Year = years
df$Commodity = commodity.names


for(j in 1996:2017){
  temp1 = subset(country_import, Year == j)
  for(k in 1:length(countries)){
    for(i in 1:5){
      temp2 = subset(temp1, Country == countries[k])
      df[22*(i-1)+j-1995,2+k] = ifelse(is.na(temp2[1,i+2]),0,temp2[1,i+2])
    }
  }
}


write.csv(df, "clustering.import.csv")

# here is a trial for the cluster based on the Kmeans, and I will deploy some more features if possible and time available.
k = 11
kmeans(t(df[, 3:length(df)]),k)[1]
