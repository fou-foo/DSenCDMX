rm(list=ls())
t1 <- Sys.time()
########## Aux functions 
createPartition <- function(data_, p=0.7){
  t <- unique(data_$device)
  n <- length(t)
  n.p <- round(n*p, 0)
  t.sample <- sample(t, n.p)
  train.index <- which( data_$device %in% t.sample)
  return(train.index)
}
#requiere(doParallel) ugly parallel in R but useful  
#cl <- makePSOCKcluster(5)
#registerDoParallel(cl)
setwd("C:/Users/usuario/Desktop/GitHub/DSenCDMX/AWS/Intento1")
data.raw <- read.csv(file='device_failure.csv') # few records kernels function works fine 
sum(is.na(data.raw)) # not nulls thank you :D
sapply(data.raw, class)
require(lubridate) # easy handle datetimes
require(dplyr) # like SQL in R, and also load pipe operator 
require(ggplot2) # easy, fast  adn nice plots 
data.raw %>% arrange(device, date) %>% mutate(date = ymd(date) ) -> data.raw
names(data.raw)
data.raw %>% group_by(device) %>% arrange(device, date) %>%
  mutate( l.attribute1 = lag(attribute1), 
          l.attribute2 = lag(attribute2), 
          l.attribute3 = lag(attribute3), 
          l.attribute4 = lag(attribute4), 
          l.attribute5 = lag(attribute5), 
          l.attribute6 = lag(attribute6), 
          l.attribute7 = lag(attribute7), 
          l.attribute8 = lag(attribute8), 
          l.attribute9 = lag(attribute9)) -> data.raw 
sum(data.raw$failure)
data.raw <- na.omit(data.raw)
# only each device has 0 or 1 failure, and if has a failure it's the last row
n.fails.index <- which(data.raw$failure==1) #only 106 failures 
nn.fails <- data.raw[ rep(n.fails.index, each=9) + -4:4, ]

View(nn.fails)
data.raw %>% filter(failure==1) %>% group_by(device) %>% summarise(n=n()) -> t
summary(t$n)
table(data.raw$failure) / dim(data.raw)[1]
summary(data.raw)
## Focus on success stories
#########################
device.with.failures <- unique(data.raw$device[n.fails.index] )
data.sample <- data.raw[ data.raw$device %in% device.with.failures, ]

write.csv(data.sample, row.names = FALSE, file='failures_SUPER.csv')
ggplot(data.sample, aes(date, attribute1, color= device, alpha=.03 )) + 
  geom_line() +   theme_minimal() + 
  theme(legend.position="none") 
ggplot(data.sample, aes(attribute1, fill =  as.character(failure), alpha=.01)) +
  geom_density()  + theme_minimal()
ggplot(data.sample, aes(attribute2, fill =  as.character(failure), alpha=.01)) +
  geom_density()  + theme_minimal()
ggplot(data.sample, aes(attribute3, fill =  as.character(failure), alpha=.01)) +
  geom_density()  + theme_minimal()
ggplot(data.sample, aes(attribute4, fill =  as.character(failure), alpha=.01)) +
  geom_density()  + theme_minimal()
ggplot(data.sample, aes(attribute5, fill =  as.character(failure), alpha=.01)) +
  geom_density()  + theme_minimal()
ggplot(data.sample, aes(attribute6, fill =  as.character(failure), alpha=.01)) +
  geom_density()  + theme_minimal()
ggplot(data.sample, aes(attribute7, fill =  as.character(failure), alpha=.01)) +
  geom_density()  + theme_minimal()
ggplot(data.sample, aes(attribute8, fill =  as.character(failure), alpha=.01)) +
  geom_density()  + theme_minimal()
ggplot(data.sample, aes(attribute9, fill =  as.character(failure), alpha=.01)) +
  geom_density()  + theme_minimal()
index.columns <- c(2, 3, 4, 7, 8, 9:18) + 3 
# log features selected 
data.sample[, names(data.sample)[index.columns]] <-  
  log(data.sample[, names(data.sample)[index.columns]] + 1 )
# standar features 
index.columns <- grep('attr', names(data.sample))
summary(data.sample)
index.columns <- grep('attr', names(data.sample))

for ( i in index.columns ){
  temp <- data.sample[, names(data.sample)[i]]
  data.sample[, names(data.sample)[i]] <- scale(temp)
}
ggplot(data.sample, aes(attribute1, fill =  as.character(failure), alpha=.01)) +
  geom_density() + theme_minimal()
ggplot(data.sample, aes(attribute2, fill =  as.character(failure), alpha=.01)) +
  geom_density()  + theme_minimal()
ggplot(data.sample, aes(attribute3, fill =  as.character(failure), alpha=.01)) +
  geom_density()  + theme_minimal()
ggplot(data.sample, aes(attribute4, fill =  as.character(failure), alpha=.01)) +
  geom_density()  + theme_minimal()
ggplot(data.sample, aes(attribute5, fill =  as.character(failure), alpha=.01)) +
  geom_density()  + theme_minimal()
ggplot(data.sample, aes(attribute6, fill =  as.character(failure), alpha=.01)) +
  geom_density()  + theme_minimal()
ggplot(data.sample, aes(attribute7, fill =  as.character(failure), alpha=.01)) +
  geom_density()  + theme_minimal()
ggplot(data.sample, aes(attribute8, fill =  as.character(failure), alpha=.01)) +
  geom_density()  + theme_minimal()
ggplot(data.sample, aes(attribute9, fill =  as.character(failure), alpha=.01)) +
  geom_density()  + theme_minimal()
############### Split data 
#nn.fails[, c(4, 6) ] <- NULL
require(caret)
set.seed(0)
data.sample$failure <- factor(data.sample$failure)
levels(data.sample$failure) <- c('NoFailure', 'Failure')
train.index <- createPartition(data.sample)
data.sample$date <- data.sample$device <- NULL
train <- data.sample[train.index, ]
test <- data.sample[-train.index, ]
table(train$failure)/dim(train)[1]
####################   train 
require(MLmetrics)
f1 <- function (data, lev = NULL, model = NULL) {
  precision <- posPredValue(data$pred, data$obs, positive = "Failure")
  recall  <- sensitivity(data$pred, data$obs, positive = "Failure")
  f1_val <- (2 * precision * recall) / (precision + recall)
  names(f1_val) <- c("F1")
  f1_val
} 
fit.control <- trainControl( method = "repeatedcv", number = 10, repeats = 5,
                             allowParallel = TRUE, classProbs = TRUE,
                             summaryFunction = f1,  sampling =  "up")
require(gbm)
require(e1071) # svm impmelemntation 
require(kernlab) # kernels implementation 
require(LiblineaR)
#install.packages('xgboost')
require(xgboost)
#install.packages('LiblineaR')
set.seed(0)
gbmFit1 <- train(failure ~ ., data = train, method = "gbm", trControl = fit.control,
                 verbose = TRUE)
gbmFit1
#install.packages('xgboost')
require(xgboost)
xgb.Fit1 <- train(failure ~ ., data = train, method = "xgbTree",# tuneLength = 5, search= 'random', 
                  trControl = fit.control,
                  verbose = TRUE)
xgb.Fit1

require(randomForest)
require(inTrees)
rf.Fit1 <- train(failure ~ ., data = train, method = "rfRules", trControl = fit.control,
                 verbose = TRUE)
rf.Fit1
rlg.Fit1 <- train(failure ~ ., data = train, method = "regLogistic", 
                  trControl = fit.control, verbose = TRUE)
rlg.Fit1
resamps <- resamples(list(GBM = gbmFit1, XGB = xgb.Fit1,
                          RF = rf.Fit1, RLG=rlg.Fit1  ))
resamps
summary(resamps)
summary(diff(resamps))
rlg.Fit1$bestTune
confusionMatrix(predict(rf.Fit1$finalModel,train), train$failure)
confusionMatrix(predict(gbmFit1$finalModel,train, n.trees= 150,
                        single.tree=FALSE, type='response'), train$failure)
confusionMatrix(predict(rlg.Fit1$finalModel,train)$predictions, train$failure)
hist(predict(gbmFit1$finalModel,train, n.trees= 150, single.tree=FALSE, type='response'))
t2 <- Sys.time()
t2 - t1
confusionMatrix(predict(xgb.Fit1$finalModel,as.matrix(train[, 2:10])) ), train$failure)
hist(predict(xgb.Fit1$finalModel,as.matrix(train[, 2:10])) )  
t2 <- Sys.time()
t2 - t1
confusionMatrix(predict(rf.Fit1$finalModel,test), test$failure)
confusionMatrix(predict(rlg.Fit1$finalModel,test)$predictions, test$failure)
























fit.control <- trainControl( method = "repeatedcv", number = 30, repeats = 10,
                             allowParallel = TRUE, classProbs = TRUE, search = 'grid', 
                             summaryFunction = f1,  sampling =  "down")
rlg.Fit2 <- train(failure ~ ., data = train,  method = "regLogistic", 
                  trControl = fit.control,
                  verbose = TRUE)
rlg.Fit2
confusionMatrix(predict(rlg.Fit2$finalModel,train)$predictions, train$failure)
dt <- as.data.frame(rlg.Fit2$finalModel$W[1, ] )
names(dt) <- c('W')
dt$variable <- rownames(dt)
ggplot(dt, aes(variable, W, fill = variable )) + geom_bar(stat = 'identity') + 
  theme_minimal() + 
  ###############################################3
  ###############################################
set.seed(0)
gbmFit1 <- train(failure ~ ., data = test, method = "gbm", trControl = fit.control,
                 verbose = TRUE)
gbmFit1
lda.Fit1 <- train(failure ~ ., data = test, method = "lda", trControl = fit.control,
                  verbose = TRUE)
lda.Fit1
rf.Fit1 <- train(failure ~ ., data = test, method = "rf", trControl = fit.control,
                 verbose = TRUE)
rf.Fit1
rlg.Fit1 <- train(failure ~ ., data = test, method = "regLogistic", 
                  trControl = fit.control, verbose = TRUE)
rlg.Fit1
resamps <- resamples(list(GBM = gbmFit1, LDA = lda.Fit1,
                          RF = rf.Fit1, RLG=rlg.Fit1  ))
resamps
summary(resamps)
summary(diff(resamps))
confusionMatrix(predict(rlg.Fit1$finalModel,test)$predictions, test$failure)

rlg.Fit2 <- train(failure ~ ., data = test,  method = "regLogistic", 
                  trControl = fit.control,
                  verbose = TRUE)
rlg.Fit2
confusionMatrix(predict(rlg.Fit2$finalModel,test)$predictions, test$failure)
dt <- as.data.frame(rlg.Fit2$finalModel$W[1, ] )
names(dt) <- c('W')
dt$variable <- rownames(dt)
ggplot(dt, aes(variable, W, fill = variable )) + geom_bar(stat = 'identity') + 
  theme_minimal() + 
  
  
  
  ## When you are done:
  stopCluster(cl)
