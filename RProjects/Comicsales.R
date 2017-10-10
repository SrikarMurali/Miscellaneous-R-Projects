#Superman Sales From 1965 - 1987


supermansales <- read.csv("C:/Users/Nathan/Desktop/R/SupermanSales.csv", header = TRUE)
batmansales <- read.csv("C:/Users/Nathan/Desktop/R/BatmanSales.csv", header = TRUE)
spidermansales <- read.csv("C:/Users/Nathan/Desktop/R/SpidermanSales.csv", header = TRUE)
require("RColorBrewer")


opt <- options("scipen" = 20)
op <- par(mar = c(5,7,4,2) + 0.1)


plot(supermansales,
     main = "Superman, Batman and Spiderman\n Comic Sales from 1965-1987",
     xlab = "Years",
     ylab = "Total Paid Ciculation",
     pch = 16,
     col = "blue2",
     col.lab = "darkred",
     col.axis = "blue4",
     col.main = "gray4",
     cex.axis = 1,
     cex.lab = 1.5)
points(batmansales, pch = 16)
points(spidermansales, pch = 16, col = "red2")
par(op)



legend("topright", c("Superman Sales", "Batman Sales",
                     "Spiderman Sales"),
       col = c("blue", "black", "red"),
       pch = 16,
       bty = "n"
)


lines(lowess(supermansales$Year, supermansales$Total.Sales), col = "blue")
lines(lowess(batmansales$Years, batmansales$Total.Paid.Circulation), col = "black")
lines(lowess(spidermansales$Year, spidermansales$Sales), col = "red")

rm(list = ls())
