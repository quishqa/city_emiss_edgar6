library(data.table)
library(openair)

total <- fread("data/sum-sectors-anthropogeni.csv", sep = ",",
               header = T, dec = ".")
total <- as.data.frame(total)
names(total) <- gsub(" ", "_", names(total))
names(total)[1] <- "date"
total$date <- as.POSIXct(total$date)

cities <- c("Delhi", "Mexico", "Pekin", "San_Fransico", 
            "Tokyo", "London", "Sao_Paulo", "Manille")
colors <- c("orange", "chartreuse4", "brown4", "darkblue",
            "firebrick", "cadetblue4", "red", "darkgolderod")

total <- selectByDate(total, start = "1/1/2000", "1/1/2018")

co_cities <- paste0("CO_", cities)
ylim <- max(range(co[co_cities]))
co <- total[c("date", co_cities)]
plot(co$date, co$CO_Mexico, t = "n", ylim = c(0, ylim),
     xlab = "Year", ylab = "CO emission (Tg)")
grid()
for (i in seq(1, length(co_cities))){
  lines(co$date, co[[co_cities[i]]], col = colors[i], lwd = 1.5)
  points(co$date, co[[co_cities[i]]], col = colors[i], pch = 19)
}