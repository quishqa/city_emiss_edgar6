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
cities_lab <- c("Delhi", "Mexico", "Pekin", "San Fransico", 
                "Tokyo", "London", "São Paulo", "Manille")
colors <- c("orange", "chartreuse4", "darkred", "darkblue",
            "firebrick1", "cadetblue4", "black", "darkgoldenrod")

total <- selectByDate(total, start = "1/1/2000", "1/1/2018")

plot_emiss_pol <- function(total, pol, ylab = "[Tg]", round = T, 
                           legend = T){
  cities <- c("Delhi", "Mexico", "Pekin", "San_Fransico", 
              "Tokyo", "London", "Sao_Paulo", "Manille")
  cities_lab <- c("Delhi", "Mexico City", "Pekin", "San Fransico", 
                  "Tokyo", "London", "São Paulo", "Manille")
  colors <- c("orange", "chartreuse4", "darkred", "darkblue",
              "firebrick1", "cadetblue4", "black", "darkgoldenrod")
  
  par(mar = c(2.5, 4, 1.5, 1.5))
  pol_cities <- paste0(pol, "_", cities)
  ylim <- max(range(total[pol_cities]))
  if (round){
    ylim <- round(ylim)
  }
  pol_df <- total[c("date", pol_cities)]
  plot(2000:2018, pol_df[[2]], t = "n", ylim = c(0, ylim),
       xlab = "", ylab = "",
       main = "", axes = F)
  axis(1)
  axis(2)
  box()
  mtext(ylab, side = 2, line = 2.5)
  grid(lty=1)
  for (i in seq(1, length(co_cities))){
    lines(2000:2018, pol_df[[pol_cities[i]]], col = colors[i], lwd = 1.5)
    points(2000:2018, pol_df[[pol_cities[i]]], col = colors[i], pch = 19)
  }
  if (legend){
    legend("topleft", legend = cities_lab, col = colors, 
           lty = 1, pch = 19, ncol = 2,  cex = 0.8)
  }
}

png("results/sum_anthro_cities.png", res = 300, units = "in",
    height = 5, width = 10)
par(mar = c(2.5, 4, 1.5, 1.5), mfrow = c(2, 2))
plot_emiss_pol(total, "SO2", 
               ylab = expression("SO"[2] * " [Tg]"))
plot_emiss_pol(total, "NOx", round = F, legend = F,
               ylab = expression("NO"[X] * " [Tg]"))
plot_emiss_pol(total, "CO", legend = F,
               ylab = "CO [Tg]" )
plot_emiss_pol(total, "CO2-excl-short-cycle", 
               ylab = expression("CO"[2] * " [Tg]"), legend = F)
dev.off()

# png("results/co_sum_anthro_cities.png", res = 300, units = "in",
#     height = 4, width = 7)
# par(mar = c(2.5, 4, 1.5, 1.5))
# co_cities <- paste0("CO_", cities)
# ylim <- round(max(range(co[co_cities])))
# co <- total[c("date", co_cities)]
# plot(2000:2018, co$CO_Mexico, t = "n", ylim = c(0, ylim),
#      xlab = "", ylab = "[Tg]",
#      main = "Total anthropogenic CO emissions")
# grid(lty=1)
# for (i in seq(1, length(co_cities))){
#   lines(2000:2018, co[[co_cities[i]]], col = colors[i], lwd = 1.5)
#   points(2000:2018, co[[co_cities[i]]], col = colors[i], pch = 19)
# }
# legend("topleft", legend = cities_lab, col = colors, 
#        lty = 1, pch = 19, ncol = 2,  cex = 0.7)
# dev.off()
