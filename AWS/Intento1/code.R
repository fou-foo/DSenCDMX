packs <- c('lubridate', 'dplyr', 'ggplot2', 'caret', 'MLmetrics', 'gbm', 'e1071', 'LiblineaR', 
           'xgboost', 'randomForest', 'ROCR')
index <- packs %in% row.names(installed.packages())
if (any(!index)){
  sapply(packs[!index], FUN=install.packages )
}
require(lubridate) # easy handle datetimes
require(dplyr) # like SQL in R, and also load pipe operator 
require(ggplot2) # easy, fast  and nice plots 
require(caret) # a toolbox 
require(MLmetrics) # metric in one line 
require(gbm) # boosting alg.
require(e1071) # numerical rutines for svm implementation 
require(LiblineaR) # another implementation for RLogReg
require(xgboost) # The implementation 
require(randomForest) # nice RF implementation 
require(doParallel) #ugly parallel in R but useful  

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
cl <- makePSOCKcluster(5)
registerDoParallel(cl)
setwd("C:/Users/usuario/Desktop/GitHub/DSenCDMX/AWS/Intento1")
data.raw <- read.csv(file='device_failure.csv') # few records kernels function works fine 
sum(is.na(data.raw)) # not nulls thank you :D
sapply(data.raw, class)

data.raw %>% arrange(device, date) %>% mutate(date = ymd(date) ) -> data.raw
names(data.raw)
# only each device has 0 or 1 failure, and if has a failure it's the last row
n.fails.index <- which(data.raw$failure==1) #only 106 failures 
nn.fails <- data.raw[ rep(n.fails.index, each=9) + -4:4, ]

#View(nn.fails)
data.raw %>% filter(failure==1) %>% group_by(device) %>% summarise(n=n()) -> t
summary(t$n)
table(data.raw$failure) / dim(data.raw)[1]
summary(data.raw)
## Focus on success stories
#########################
device.with.failures <- unique(data.raw$device[n.fails.index] )
data.sample <- data.raw[ data.raw$device %in% device.with.failures, ]

write.csv(data.sample, row.names = FALSE, file='failures.csv')
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
index.columns <- c(2, 3, 4, 7, 8, 9) + 3 
# log features selected 
data.sample[, names(data.sample)[index.columns]] <-  
  log(data.sample[, names(data.sample)[index.columns]] + 1 )
# standar features 
index.columns <- grep('attr', names(data.sample))
summary(data.sample)
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
set.seed(0)
data.sample$failure <- factor(data.sample$failure)
levels(data.sample$failure) <- c('NoFailure', 'Failure')
train.index <- createPartition(data.sample)
data.sample$date <- data.sample$device <- NULL
train <- data.sample[train.index, ]
test <- data.sample[-train.index, ]
table(train$failure)/dim(train)[1]
####################   train 
f1 <- function (data, lev = NULL, model = NULL) {
  precision <- posPredValue(data$pred, data$obs, positive = "Failure")
  recall  <- sensitivity(data$pred, data$obs, positive = "Failure")
  f1_val <- (2 * precision * recall) / (precision + recall)
  names(f1_val) <- c("F1")
  f1_val
} 
fit.control <- trainControl( method = 'repeatedcv', number = 10, repeats = 3, 
                             allowParallel = TRUE, classProbs = TRUE,
                             summaryFunction = f1,  sampling =  "up")

#install.packages('LiblineaR')
set.seed(0)
gbmFit1 <- train(failure ~ ., data = train, method = "gbm", trControl = fit.control,
                 verbose = TRUE)
gbmFit1
#install.packages('xgboost')
xgb.Fit1 <- train(failure ~ ., data = train, method = "xgbTree", #tuneLength = 5, search= 'random', 
                  trControl = fit.control,
                  verbose = TRUE)
xgb.Fit1
rf.Fit1 <- train(failure ~ ., data = train, method = "rf", trControl = fit.control,
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
confusionMatrix(predict(rf.Fit1$finalModel,train), train$failure)
confusionMatrix(predict(rlg.Fit1$finalModel,train)$predictions, train$failure)
confusionMatrix(predict(xgb.Fit1, train), train$failure)
t2 <- Sys.time()
t2 - t1
t3 <- Sys.time()
set.seed(0)
tune_grid <- expand.grid(nrounds=c(100,300), max_depth = c(4:7), eta = c(0.05, 1),   gamma = c(0.01),
                         colsample_bytree = c(0.75), subsample = c(0.50),  min_child_weight = c(0))

xgb_fit <- train(failure ~., data = train, method = "xgbTree",
                trControl= fit.control,
                tuneGrid = tune_grid,
                tuneLength = 10)
tune_grid <- expand.grid(.mtry = (1:16)) 
rf_fit <- train(failure ~., data = train, method = "rf",
                trControl= fit.control,
                tuneGrid = tune_grid,
                tuneLength = 10)

t4 <- Sys.time()
t4 - t1
confusionMatrix(predict(rf_fit$finalModel, test), test$failure)
confusionMatrix(predict(xgb_fit, test), test$failure)

set.seed(825)
gbmFit2 <- train(Class ~ ., data = training, 
                 method = "gbm", 
                 trControl = fitControl, 
                 verbose = FALSE, 
                 ## Now specify the exact models 
                 ## to evaluate:
                 tuneGrid = gbmGrid)
gbmFit2
fit.control <- trainControl( method = "repeatedcv", number = 30, repeats = 10,
                             allowParallel = TRUE, classProbs = TRUE, search = 'grid', 
                             summaryFunction = f1,  sampling =  "down")
rf.Fit2 <- train(failure ~ ., data = train,  method = "rf", 
                  trControl = fit.control,
                 verbose = TRUE)
rf.Fit2
confusionMatrix(predict(rf.Fit2$finalModel,train), train$failure)
confusionMatrix(predict(rf.Fit2$finalModel, test), test$failure)

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
