rm(list=ls())
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


ggplot(nn.fails, aes(date, attribute1, color= device, alpha=.03 )) + 
  geom_line() +   theme_minimal() + 
  theme(legend.position="none") 
ggplot(nn.fails, aes(attribute1, fill =  as.character(failure), alpha=.01)) +
  geom_density()  + theme_minimal()
ggplot(nn.fails, aes(attribute2, fill =  as.character(failure), alpha=.01)) +
  geom_density()  + theme_minimal()
ggplot(nn.fails, aes(attribute3, fill =  as.character(failure), alpha=.01)) +
  geom_density()  + theme_minimal()
ggplot(nn.fails, aes(attribute4, fill =  as.character(failure), alpha=.01)) +
  geom_density()  + theme_minimal()
ggplot(nn.fails, aes(attribute5, fill =  as.character(failure), alpha=.01)) +
  geom_density()  + theme_minimal()
ggplot(nn.fails, aes(attribute6, fill =  as.character(failure), alpha=.01)) +
  geom_density()  + theme_minimal()
ggplot(nn.fails, aes(attribute7, fill =  as.character(failure), alpha=.01)) +
  geom_density()  + theme_minimal()
ggplot(nn.fails, aes(attribute8, fill =  as.character(failure), alpha=.01)) +
  geom_density()  + theme_minimal()
ggplot(nn.fails, aes(attribute9, fill =  as.character(failure), alpha=.01)) +
  geom_density()  + theme_minimal()
index.columns <- c(2, 3, 4, 7, 8, 9) + 3 
# log features selected 
nn.fails[, names(nn.fails)[index.columns]] <-  
  log(nn.fails[, names(nn.fails)[index.columns]] + 1 )
# standar features 
index.columns <- grep('attr', names(data.raw))
summary(nn.fails)
for ( i in index.columns ){
  temp <- nn.fails[, names(nn.fails)[i]]
  nn.fails[, names(nn.fails)[i]] <- scale(temp)
}
ggplot(nn.fails, aes(attribute1, fill =  as.character(failure), alpha=.01)) +
  geom_density() + theme_minimal()
ggplot(nn.fails, aes(attribute2, fill =  as.character(failure), alpha=.01)) +
  geom_density()  + theme_minimal()
ggplot(nn.fails, aes(attribute3, fill =  as.character(failure), alpha=.01)) +
  geom_density()  + theme_minimal()
ggplot(nn.fails, aes(attribute4, fill =  as.character(failure), alpha=.01)) +
  geom_density()  + theme_minimal()
ggplot(nn.fails, aes(attribute5, fill =  as.character(failure), alpha=.01)) +
  geom_density()  + theme_minimal()
ggplot(nn.fails, aes(attribute6, fill =  as.character(failure), alpha=.01)) +
  geom_density()  + theme_minimal()
ggplot(nn.fails, aes(attribute7, fill =  as.character(failure), alpha=.01)) +
  geom_density()  + theme_minimal()
ggplot(nn.fails, aes(attribute8, fill =  as.character(failure), alpha=.01)) +
  geom_density()  + theme_minimal()
ggplot(nn.fails, aes(attribute9, fill =  as.character(failure), alpha=.01)) +
  geom_density()  + theme_minimal()
############### Split data 
require(caret)
set.seed(0)
nn.fails$failure <- factor(nn.fails$failure)
levels(nn.fails$failure) <- c('NoFailure', 'Failure')
train.index <- createPartition(nn.fails)
nn.fails$date <- nn.fails$device <- NULL
train <- nn.fails[train.index, ]
test <- nn.fails[-train.index, ]
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
fit.control <- trainControl( method = "repeatedcv", number = 10, repeats = 2,
                             allowParallel = TRUE, classProbs = TRUE,
                             summaryFunction = f1,  sampling =  "down")
require(gbm)
require(e1071) # svm impmelemntation 
require(kernlab) # kernels implementation 
require(LiblineaR)
require(DMwR)
require(ROSE)

#install.packages('ROSE')
#install.packages('DMwR')
#install.packages('LiblineaR')
set.seed(0)
gbmFit1 <- train(failure ~ ., data = train, method = "gbm", trControl = fit.control,
                 verbose = TRUE)
gbmFit1
lda.Fit1 <- train(failure ~ ., data = train, method = "lda", 
                      trControl = fit.control,
                      verbose = TRUE)
lda.Fit1
require(randomForest)
rf.Fit1 <- train(failure ~ ., data = train, method = "rf", trControl = fit.control,
                 verbose = TRUE)
rf.Fit1
rlg.Fit1 <- train(failure ~ ., data = train, method = "regLogistic", 
                  trControl = fit.control, verbose = TRUE)
rlg.Fit1
resamps <- resamples(list(GBM = gbmFit1, LDA = lda.Fit1,
                          RF = rf.Fit1, RLG=rlg.Fit1  ))
resamps
summary(resamps)
summary(diff(resamps))
rlg.Fit1$bestTune
confusionMatrix(predict(rlg.Fit1$finalModel,train)$predictions, train$failure)
rlgGrid <-  expand.grid(X1 = c(.1, .6, 1.1, 1.6, 2.1), 
                        X2 = c('L1', 'L2_dual', 'L2_primal', 'L2'), 
                        X3   = c(.001, .010, .1, .2))
#names(rlgGrid) <- c('X1', 'X2' , 'X3')
nrow(rlgGrid)
set.seed(0)
rlg.Fit2 <- train(failure ~ ., data = train,  method = "regLogistic", 
                  trControl = fit.control,
                 verbose = FALSE,   tuneGrid = rlgGrid)
rlg.Fit2
dt <- as.data.frame(rlg.Fit1$finalModel$W[1, ] )
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




## When you are done:
stopCluster(cl)
