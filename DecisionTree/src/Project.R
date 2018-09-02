
setwd("/home/roshan/workspace/R Studio/DataMining/")
library(party)
library(corrplot)
library(caTools)
library(ggplot2)

data <- read.csv(file = "wineQualityWhites.csv", sep = ",", header = TRUE)

cr <- cor(data[c(-1)])
corrplot(cr, type = "lower")

data$quality.factor <- as.factor(data$quality)

levels(data$quality.factor)
ggplot(data = data, aes(x = quality)) +
  geom_histogram(binwidth = 1)

for(i in 1:nrow(data)){
  if(data$quality[[i]] < 6){
    data$quality.order[[i]] <- "Bad"
  }else{
    data$quality.order[[i]] <- "Good"
  } 
}

i <- j <- 0
for(q in data$quality.order){
  if(q == "Bad"){
    i <- i+1    
  }else{
    j <- j+1
  }
}
print(i)
print(j)
data$quality.order <- factor(factor(data$quality.order), levels = c("Bad", "Good"))

for(i in 1:nrow(data)){
  if(data$quality.order[[i]] == "Bad"){
    data$quality.num[[i]] <- 0
  }else{
    data$quality.num[[i]] <- 1
  } 
}

value <- sample.split(data$X, SplitRatio = 0.7)
train.data <- subset(data, value == TRUE)
test.data <- subset(data, value == FALSE)

write.csv(train.data, file = "train_data.csv")
write.csv(test.data, file = "test_data.csv")

train.data <- read.csv(file = "train_data.csv", sep = ",", header = TRUE) 
test.data <- read.csv(file = "test_data.csv", sep = ",", header = TRUE) 

d.tree <- ctree(quality.num ~ 
                  fixed.acidity 
                + volatile.acidity 
                + residual.sugar 
                + chlorides 
                + total.sulfur.dioxide 
                + density
                + pH
                + sulphates
                + alcohol
                , data = train.data)


plot(d.tree, type = "simple")

prob <- predict(d.tree, newdata = test.data, type = "response")

classes <- c()
for (i in prob) {
  if(i >= 0.5) {
    classes <- c(classes,1)
  } else {
    classes <- c(classes,0)
  } 
}

conf.matrix <- table(Actual = test.data[,16], Predicted = classes)
print(conf.matrix)

acc <- (conf.matrix[1,1]+conf.matrix[2,2])/length(test.data[,16])
acc <- round(acc, digits = 4)
print(paste("Accuracy = ",acc))