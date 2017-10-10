#kiteman appearances

kiteman <- read.csv("C:/Users/Nathan/Desktop/R/Kiteman.csv", header = TRUE)
require("RColorBrewer")
plot(kiteman,
     col = c("goldenrod3"),
     xlab = "Years",
     ylab = "Number of Apperances",
     main = "Apperances of Kiteman Throughout the Years",
     col.main = "seagreen",
     col.axis = "darkgreen",
     col.lab = "yellow4",
     cex.main = 0.85,
     pch = 16
     )

rm(list = ls())
