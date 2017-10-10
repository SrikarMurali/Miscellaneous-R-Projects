
#Weed Usage

a <-  read.csv("C:/Users/Nathan/Desktop/R/WeedUsage.csv", header = TRUE)
require("RColorBrewer")
barplot(a$X.,
        ylab = "Percentage of people that smoke",
        xlab = "Age Groups",
        ylim = c(0,100),
        names.arg = c("18-29", "30-49", "50-64", "65"),
        main = "Percentage of People who have tried Marijuana\n by Age Group",
        col.lab = "springgreen",
        col.axis = "seagreen4",
        col.main = "mediumseagreen",
        cex.main = 0.75,
        col = c("darkgreen", "forestgreen", "limegreen", "lawngreen"),
        axes = TRUE,
        axis.lty = 1,
        panel.first = abline(
          h = seq.int(0, 100, 10),
          col = "green2",
          lty = 2
        )
      )


legend(locator(1), c("18-29", "30-49",
                     "50-64", "65+"),
       col = c("darkgreen", "forestgreen", "limegreen", "lawngreen"),
       pch = 16,
       bty = "n"
)
