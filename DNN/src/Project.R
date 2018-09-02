
setwd("/home/roshan/workspace/R Studio/R-Pr/")
library(tensorflow)
use_virtualenv("/home/roshan/.virtualenvs/r-tensorflow")
library(reticulate)
wn <- import("warnings")
wn$filterwarnings("ignore")
library(keras)
library(caTools)
library(ggplot2)
library(corrplot)

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
Loading Existing training and testing data sets

train.data <- read.csv(file = "train_data.csv", sep = ",", header = TRUE) 
test.data <- read.csv(file = "test_data.csv", sep = ",", header = TRUE) 

train.data <- train.data[,-1]
test.data <- test.data[,-1]

x_train <- as.matrix(train.data[,c(2,3,5,6,8:12)])
y_train <- train.data[,16]
x_test  <- as.matrix(test.data[,c(2,3,5,6,8:12)])
y_test  <- test.data[,16]

y_train <- to_categorical(y_train,2)
y_test <- to_categorical(y_test,2)

model <- NULL
model <- keras_model_sequential()
model %>%
  layer_dense(units = 256, input_shape = c(9), activation = "relu") %>%
  layer_dropout(0.2) %>%
  layer_dense(units = 128, activation = "relu") %>%
  layer_dropout(0.3) %>% 
  layer_dense(units = 2,  activation = "softmax") 

model %>% compile(
  loss = "binary_crossentropy",
  optimizer = optimizer_adam(0.001),
  metrics = c('accuracy')
)

model %>% fit(
  x_train, y_train, 
  validation_split = 0.3, 
  epochs = 100, 
  batch_size = 87
)

save_model_hdf5(model, "model_name.h5")

model <- load_model_hdf5("model8_tracc_0.7850.h5")

model %>% evaluate(x_test,y_test)

pred <- model %>% predict_classes(x_test)
table(Actual = test.data[,16], Predicted = pred)