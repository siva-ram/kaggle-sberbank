
train <- read.csv("F:\\kaggle\\train.csv", header=T, na.strings = "NA")
test <- read.csv("F:\\kaggle\\test.csv", header=T, na.strings = "NA")
macro <- read.csv("F:\\kaggle\\macro.csv", header=T, na.strings = "NA")

train$id <- NULL
test$id <- NULL
macro$id <- NULL


train_merge <- merge(train,macro,by="timestamp")
test_merge <- merge(test,macro,by="timestamp")
#train_merge$na_count = rowSums(is.na(train_merge))

#train_merge(train_merge$na_count )
test_merge$timestamp <-NULL
train_merge$timestamp <-NULL


library(rpart)
#all variables included
LGDrpart <- rpart(price_doc~.,data = train_merge,cp=0.0001)


plotcp(LGDrpart)

LGDprune <- prune.rpart(LGDrpart, cp=0.0025) 

predict(LGDprune,test_merge)


write.table(data.table(id=test_merge$id, price_doc=predict(LGDprune,test_merge)), "submission.csv", sep=",", dec=".", quote=FALSE, row.names=FALSE)

ds=train_merge[,sapply(train_merge, function(x) sum(is.na(x)))/length(train_merge$price_doc)<.2]
#random forest
gc()
ds <-ds[, sapply(ds, function(col) (is.numeric(col) || length(unique(col)) < 25))]
summary(ds)
library(randomForest)
rf_imputed<-rfImpute(price_doc~.,ds)
rf <- randomForest(price_doc~., data=dat2,importance=TRUE, ntree=50)
summary(rf)

Mode(train_merge$thermal_power_plant_raion)
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

library(plyr)
impute.med <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
impute.mod <- function(x) replace(x, is.na(x), Mode(x))
dat2 <- sapply(ds, function(x){
  if(is.numeric(x)){
    impute.med(x)
  } else {
    impute.mod(x)
  }
}
)
colnames(dat2)
View(test_merge[, names(test_merge) %in% colnames(dat2)])

dat2_test<-  sapply(test_merge[, names(test_merge) %in% colnames(dat2)], function(x){
  if(is.numeric(x)){
    impute.med(x)
  } else {
    impute.mod(x)
  }
}
)


imp <- importance(rf, type=1)
featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])
library(ggplot2)
p <- ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
  geom_bar(stat="identity", fill="#53cfff") +
  coord_flip() + 
  theme_light(base_size=20) +
  xlab("") +
  ylab("Importance") + 
  ggtitle("Random Forest Feature Importance\n") +
  theme(axis.text=element_text(size=12),plot.title=element_text(size=18))
p


predict(rf,as.matrix(test_merge[, names(test_merge) %in% colnames(dat2)]))
length(test_merge$id)

length(predict(LGDprune,test_merge))
length(predict(LGDprune,data=test_merge))



?rfImpute

str(train_merge)
model.lm<- lm(price_doc~.,data=train_merge[, sapply(train_merge, is.numeric)])


cor(train_merge[, sapply(train_merge, is.numeric)])

plot(train_merge$full_sq,train_merge$price_doc)


col_names <- sapply(train_merge, function(col) length(unique(col)) > 0)

train_merge[ , -col_names]


(l <- sapply(train_merge, function(x) is.factor(x)))
m <- train_merge[, l]
ifelse(n <- sapply(m, function(x) length(levels(x))) == 1, "DROP", "NODROP")





