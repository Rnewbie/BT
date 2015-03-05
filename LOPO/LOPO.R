## No corr cutoff
Unsuredata <- read.csv("~/Desktop/PythonKid/BT/LOPO/Unsuredata.csv", header =TRUE)
data <- Unsuredata[-1]
data <- data.frame(data)
x <- createFolds(data$Activity, k = 4)
fold1 <- data[x[[1]], ]
fold2 <- data[x[[2]], ]
fold3 <- data[x[[3]], ]
fold4 <- data[x[[4]], ]
cv <- lapply(x, function(x) {
  train <- data[x, ]
  test <- data[-x, ]
  model <- randomForest(Activity~., data = train, ntree= 500)
  pred <- predict(model, test)
  actual <- test$Activity
  matrix <- table(pred, actual)
  return(matrix)
})
cv
## Trying to get things  done
Unsuredata <- read.csv("~/Desktop/PythonKid/BT/LOPO/Unsuredata.csv", header =TRUE)
data <- Unsuredata[-1]
data <- data.frame(data)
Test1 <- data[1:72, ]
Train1 <- data[73:288, ]
Test2 <- data[73:144, ]
raw2.1 <- data[1:72, ]; raw2.2 <- data[145:288, ]
Train2 <- rbind(raw2.1, raw2.2)
Test3 <- data[145:216, ]
raw3.1 <- data[1:144, ]; raw3.2 <- data[217:288, ]
Train3 <- rbind(raw3.1, raw3.2)
Test4 <- data[217:288, ]
Train4 <- data[1:216, ]
Train4 <- data[1:216, ]
## Fold1
model1 <- randomForest(Activity~., data = Train1, ntree= 500)
pred <- predict(model, Train1)
actual <- Train1$Activity
matrix1 <- table(pred, actual)
## Fold2
model2 <- randomForest(Activity~., data = Train2, ntree= 500)
pred <- predict(model, Train2)
actual <- Train2$Activity
matrix2 <- table(pred, actual)
## Fold3
model3 <- randomForest(Activity~., data = Train3, ntree= 500)
pred <- predict(model, Train3)
actual <- Train3$Activity
matrix3 <- table(pred, actual)
## Fold 4
model4 <- randomForest(Activity~., data = Train4, ntree= 500)
pred <- predict(model, Train4)
actual <- Train4$Activity
matrix4 <- table(pred, actual)

## Cross Validationg MAX CODE
library(plyr)
library(randomForest)

Unsuredata <- read.csv("~/Desktop/PythonKid/BT/LOPO/Unsuredata.csv", header =TRUE)
data <- Unsuredata[-1]
data <- data.frame(data)
k = 4 
data$id <- sample(1:k, nrow(data), replace = TRUE)
list <- 1:k
prediction <- data.frame()
testsetCopy <- data.frame()
progress.bar <- create_progress_bar("text")
progress.bar$init(k)

for (i in 1:k){
  trainingset <- subset(data, id %in% list[-i])
  testset <- subset(data, id %in% c(i))
  mymodel <- randomForest(trainingset$Activity ~ ., data = trainingset, ntree = 500)
  temp <- as.data.frame(predict(mymodel, testset[,-1]))
  prediction <- rbind(prediction, temp)
  testsetCopy <- rbind(testsetCopy, as.data.frame(testset[,1]))
  
  progress.bar$step()
}

result <- cbind(prediction, testsetCopy)
names(result) <- c("Predicted", "Actual")
table(result)

