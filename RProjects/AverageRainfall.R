#tracklist bargraph

require("RColorBrewer")
wd <- matrix(
  c(
    5.55, 2.05, 3.90, 
    3.46, 1.93, 2.95, 
    3.70, 2.72, 4.06,
    2.68, 3.62, 3.94,
    1.93, 4.13, 4.45,
    1.54, 4.06, 3.50,
    0.67, 4.02, 4.53,
    0.87, 3.98, 4.13,
    1.42, 3.31, 3.98,
    3.46, 3.23, 3.39,
    6.54, 3.43, 3.82,
    5.31, 2.56, 3.58
  ),
  nrow = 3,
  ncol = 12
)
wd
colnames(wd) <- month.name[c(1:12)]
colnames(wd)
rownames(wd) <- c("Seattle", "Chicago", "New York")

#par(mar = c(5, 4, 1.5, 0.5), ps = 12, cex = 1, cex.main = 2, las = 1)

barplot(
  wd,
  beside = TRUE,
  ylim = c(0,10),
  xlab = "Month",
  axes = TRUE,
  axis.lty = 1,
  ylab = "Average Monthly Rainfall (Inches)",
  col = c("dodgerblue2", "deepskyblue3", "darkblue"),
  main = "Average Monthly Rainfall for Seattle, Chicago and New York",
  col.axis = "blue4",
  col.main = "navy",
  col.lab = "midnightblue",
  cex.lab = 1,
  cex.main = 0.85,
  panel.first = abline(
    h = seq.int(0, 8, 2),
    col = "grey",
    lty = 2
  )
)

legend(locator(1), c("Seattle", "Chicago",
                     "New York"),
       col = c("dodgerblue2", "deepskyblue3", "darkblue"),
       pch = 16,
       bty = "n"
)

box()
grid()
