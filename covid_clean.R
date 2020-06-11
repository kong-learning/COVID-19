setwd("~/R/WIE2003/Covid-19")
library(data.table)

# #Read cases
confirmed <- read.csv("./datasets/time_series_covid19_confirmed_global.csv",check.names = FALSE)
deaths <- read.csv("./datasets/time_series_covid19_deaths_global.csv",check.names = FALSE)
recovered <- read.csv("./datasets/time_series_covid19_recovered_global.csv",check.names = FALSE)

#Replacing spaces within Country names by underscore to calculate COR properly
confirmed$`Country/Region` <- gsub(" ", "_", confirmed$`Country/Region`)
deaths$`Country/Region` <- gsub(" ", "_", deaths$`Country/Region`)
recovered$`Country/Region` <- gsub(" ", "_", recovered$`Country/Region`)

#Removing Lat and Long from data frame
confirmed <- within(confirmed, rm("Lat", "Long", "Province/State"))
deaths <- within(deaths, rm("Lat", "Long", "Province/State"))
recovered <- within(recovered, rm("Lat", "Long", "Province/State"))

#After removing col Lat and Long, we have dates from range 2 to x and categorizing by country
confirmed <- aggregate(confirmed[,2:ncol(confirmed)], by=list(Category=confirmed$`Country/Region`), FUN=sum)
deaths <- aggregate(deaths[,2:ncol(deaths)], by=list(Category=deaths$`Country/Region`), FUN=sum)
recovered <- aggregate(recovered[,2:ncol(recovered)], by=list(Category=recovered$`Country/Region`), FUN=sum)

#picked out Association of Southeast Asian Nations
Country <- c("Cambodia","Brunei","Indonesia", "Laos", 
                  "Malaysia", "Philippines", "Singapore", "Thailand", "Vietnam", "Burma")
select_rows <- ifelse(deaths$Category %in% Country, TRUE, FALSE)
confirmed <- confirmed[select_rows,]
deaths <- deaths[select_rows,]
recovered <- recovered[select_rows,]

#Generation date
Date <- as.data.frame(names(recovered)[-1])
Country <- as.data.frame(recovered$Category)
col_1 <- merge(Date, Country)

#Delete the first row of column names
confirmed=confirmed[-1]
recovered=recovered[-1]
deaths=deaths[-1]

#Calculate the case fatality rate
cfr <- deaths/confirmed
cfr[is.na(cfr)] <- 0
names(Country) <- c('Country')
cfr <- cbind(Country, cfr)
#Writing the data table in csv file 
write.csv(cfr, "./cfr.csv", sep=",", col.names=TRUE, quote=FALSE, row.names=FALSE)

#dataframe to row vector
confirmed_row <- confirmed[1, ]
for (i in 2:dim(confirmed)[1]){
  confirmed_row <- cbind(confirmed_row, confirmed[i, ])
} 

recovered_row <- recovered[1, ]
for (i in 2:dim(recovered)[1]){
  recovered_row <- cbind(recovered_row, recovered[i, ])
} 

deaths_row <- deaths[1, ]
for (i in 2:dim(deaths)[1]){
  deaths_row <- cbind(deaths_row, deaths[i, ])
} 

#Transpose the confirmed data to analyze the linear model
confirmed_row <- transpose(confirmed_row)
recovered_row <- transpose(recovered_row)
deaths_row <- transpose(deaths_row)

#Combine data and modify column names
csv_for_inquire <- cbind(col_1, confirmed_row, recovered_row, deaths_row)
names(csv_for_inquire) <- c('Date', 'Country', 'Confirmed', 'Recovered', 'Deaths')

#Writing the data table in csv file 
write.csv(csv_for_inquire, "./csv_for_inquire.csv", sep=",", col.names=TRUE, quote=FALSE, row.names=FALSE)

