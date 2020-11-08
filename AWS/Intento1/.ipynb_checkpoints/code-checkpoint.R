rm(list=ls())
library(doParallel)
cl <- makePSOCKcluster(5)
registerDoParallel(cl)
setwd("C:/Users/usuario/Desktop/GitHub/DSenCDMX/AWS/Intento1")
data.raw <- read.csv(file='device_failure.csv') # few records kernels function works fine 
sum(is.na(data.raw)) # not nulls thank you :D
sapply(data.raw, class)
require(lubridate) # easy handle datetimes
require(dplyr) # like SQL in R, and also load pipe operator 
require(ggplot2) # easy, fast  adn nice plots 
data.raw %>% arrange(device, date) %>% mutate(date = ymd(date) ) -> data.raw
names(data.raw)
#View(data.raw)
n.fails.index <- which(data.raw$failure==1)
nn.fails <- data.raw[ rep(n.fails.index, each=9) + -4:4, ]
#View(nn.fails)
data.raw %>% filter(failure==1) %>% group_by(device) %>% summarise(n=n()) -> t
summary(t$n)
data <- data.raw
table(data$failure) / dim(data)[1]
summary(data)
createPartition <- function(data, p=0.8){
  t <- unique(data$device)
  n <- length(t)
  n.p <- round(n*p, 0)
  t.sample <- sample(t, n.p)
  train.index <- which( data$device %in% t.sample)
  return(train.index)
}

ggplot(data, aes(date, attribute1, color= device, alpha=.03 )) + geom_line() +   theme_minimal() + 
  theme(legend.position="none") 
ggplot(data, aes(attribute1, fill =  as.character(failure), alpha=.01)) +
  geom_density()  + theme_minimal()
ggplot(data, aes(attribute2, fill =  as.character(failure), alpha=.01)) +
  geom_density()  + theme_minimal()
ggplot(data, aes(attribute3, fill =  as.character(failure), alpha=.01)) +
  geom_density()  + theme_minimal()
ggplot(data, aes(attribute4, fill =  as.character(failure), alpha=.01)) +
  geom_density()  + theme_minimal()
ggplot(data, aes(attribute5, fill =  as.character(failure), alpha=.01)) +
  geom_density()  + theme_minimal()
ggplot(data, aes(attribute6, fill =  as.character(failure), alpha=.01)) +
  geom_density()  + theme_minimal()
ggplot(data, aes(attribute7, fill =  as.character(failure), alpha=.01)) +
  geom_density()  + theme_minimal()
ggplot(data, aes(attribute8, fill =  as.character(failure), alpha=.01)) +
  geom_density()  + theme_minimal()
ggplot(data, aes(attribute9, fill =  as.character(failure), alpha=.01)) +
  geom_density()  + theme_minimal()
index.columns <- c(2, 3, 4, 7, 8, 9) + 3 
# log features selected 
data[, names(data)[index.columns]] <-  log(data[, names(data)[index.columns]] + 1 )
# standar features 

index.columns <- grep('attr', names(data.raw))
summary(data)

for ( i in index.columns )
{
  temp <- data[, names(data)[i]]
  data[, names(data)[i]] <- scale(temp)
}
ggplot(data, aes(attribute1, fill =  as.character(failure), alpha=.01)) +
  geom_density() + theme_minimal()
ggplot(data, aes(attribute2, fill =  as.character(failure), alpha=.01)) +
  geom_density()  + theme_minimal()
ggplot(data, aes(attribute3, fill =  as.character(failure), alpha=.01)) +
  geom_density()  + theme_minimal()
ggplot(data, aes(attribute4, fill =  as.character(failure), alpha=.01)) +
  geom_density()  + theme_minimal()
ggplot(data, aes(attribute5, fill =  as.character(failure), alpha=.01)) +
  geom_density()  + theme_minimal()
ggplot(data, aes(attribute6, fill =  as.character(failure), alpha=.01)) +
  geom_density()  + theme_minimal()
ggplot(data, aes(attribute7, fill =  as.character(failure), alpha=.01)) +
  geom_density()  + theme_minimal()
ggplot(data, aes(attribute8, fill =  as.character(failure), alpha=.01)) +
  geom_density()  + theme_minimal()
ggplot(data, aes(attribute9, fill =  as.character(failure), alpha=.01)) +
  geom_density()  + theme_minimal()
############### Split data 
require(caret)
set.seed(0)
data$failure <- factor(data$failure)
levels(data$failure) <- c('NoFailure', 'Failure')
train.index <- createPartition(data)
data$date <- data$device <- NULL
train <- data[train.index, ]
test <- data[-train.index, ]
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
fit.control <- trainControl( method = "cv", number = 5, repeats = 2,
                             allowParallel = TRUE, classProbs = TRUE,
                             summaryFunction = f1)
require(gbm)
require(e1071) # svm impmelemntation 
require(kernlab) # kernels implementation 
gbmFit1 <- train(failure ~ ., data = train, method = "gbm", trControl = fit.control,
                 verbose = TRUE)
gbmFit1
svm.polyFit1 <- train(failure ~ ., data = train, method = "svmPoly", trControl = fit.control,
                 verbose = TRUE)
svm.polyFit1
library(parallel)
require(randomForest)
rf.Fit1 <- train(failure ~ ., data = train, method = "rf", trControl = fit.control,
                 verbose = TRUE)
rf.Fit1
rlg.Fit1 <-train(failure ~ ., data = train, method = "regLogistic", trControl = fit.control,
                 verbose = TRUE)



resamps <- resamples(list(GBM = gbmFit1, SVM = svm.polyFit1,
                          RF = rf.Fit1, RLG=rlg.Fit1  ))
resamps
summary(resamps)
theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)
bwplot(resamps, layout = c(3, 1))




## When you are done:
stopCluster(cl)
