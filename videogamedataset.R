

library(caret)

vg <- read.csv('C:/Users/srika/Documents/R/RPrograms/vg.csv', header=TRUE)
validation_index <- createDataPartition(vg$Genre, p=0.8, list=FALSE)
vg <- vg[validation_index, ]


#EDA
dim(vg)
head(vg)
sapply(vg, class)
levels(vg$Genre)
summary(vg)
summary(vg$Genre)


ggplot(vg$Genre, aes(x=Global_Sales)) +
  geom_bar() +
  ylab('Global Sales')

head(as.character(vg$Publisher))
length(unique(as.character(vg$Publisher)))
unique(as.character(vg$Publisher))

ggplot(vg, aes(x=))

#distribution of the class summary
percentage <- prop.table(table(vg$Genre)) * 100
cbind(freq=table(vg$Genre), percentage=percentage)
