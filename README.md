# ANN-prediction-models
ANN models to understand and predict fibrotic remodeling of the heart

## ANN model to predict loss of female sex hormones and pressure overload group status

#data importing and proprocessing 

library(neuralnet)

library(dplyr)

library(caret)

#setting work directory

setwd("D:/MI/work folder/dataset")

#reading data set file 

datf <- read.csv("ECM-2021-30-04-AA.csv")

#removing incomplete data (that cannot be imputed)

datf <- datf[-c(7, 20, 27), ]

View(datf)

#Replacing any missing values with NA 

datf <- replace(datf, datf == 0, NA)

#Within group mean imputation 

datf$DUSP4_18S1e5_mr_RV<-ave(datf$DUSP4_18S1e5_mr_RV,datf$group,FUN=function(x) 

  ifelse(is.na(x), mean(x,na.rm=TRUE), x))

datf$ColI_18S1e5_mr._LV<-ave(datf$ColI_18S1e5_mr._LV,datf$group,FUN=function(x) 

  ifelse(is.na(x), mean(x,na.rm=TRUE), x))

datf$Col_ug.mg_pr_LV <-ave(datf$Col_ug.mg_pr_LV,datf$group,FUN=function(x) 

  ifelse(is.na(x), mean(x,na.rm=TRUE), x))

#testing existence of any missing values 

test <- length(TRUE[is.na(datf)])

test

#normalizing

normalize <- function(x)

{

  return((x- min(x)) /(max(x)-min(x)))
  
}

datf[3:length(datf)] <- as.data.frame(lapply(datf[3:length(datf)], normalize))

View(datf)

#making a small dataset with only predictors 

dats <- data.frame(datf$group, datf$ColI_18S1e5_mr_RV, datf$ColIII_18S1e5_mr_RV,
                   datf$TIMP1_18S1e5_mr._LV, datf$JNK_pr._LV, datf$Col_ug.mg_pr_LV,
                   datf$Col_ug.mg_pr_RV, datf$MMP14_ACTIVITY_LV_90, datf$MMP14_ACTIVITY_LV_120, datf$ColI_18S1e5_mr._LV)
                   
colnames(dats) <- c("group", "ColI_18S1e5_mr_RV", "ColIII_18S1e5_mr_RV",
                    "TIMP1_18S1e5_mr._LV", "JNK_pr._LV", "Col_ug.mg_pr_LV", "Col_ug.mg_pr_RV",
                    "MMP14_ACTIVITY_LV_90", "datf$MMP14_ACTIVITY_LV_120", "ColI_18S1e5_mr._LV")
View(dats)

# setting 5 fold cross validation 

K <- 5

index <- 1:nrow(dats)

for(i in 1:length(unique(dats$group))){

  i_loc <- which(dats$group==unique(dats$group)[i])
  
  set.seed(i)
  
  folds <- caret::createFolds(1:length(i_loc),5)
  
  for (j in 1:K) {
    index[i_loc[unlist(folds[j])]] <- j
  }
}

# modeling
#splitting dataset into test and training dataset 

confusion_list <- rep(list(matrix(NA,4,4)),5)

for(k in 1:K){
  test_<-dats[which(index==k),]
  
  train_ <- dats[-which(index==k),]
}
  
## create models
names <- colnames(train_)

names <- names[2:length(names)]

View(names)

  f <- as.formula(paste("group ~", paste(names, collapse = " + ")))
  
 nn <- neuralnet(f,data=train_, hidden=c(7),act.fct = "logistic",linear.output = FALSE)
 
  plot(nn)  
  
# predict
  k <- 1
  
  pred <- predict(nn, test_)
  
  pred_label <- names(table(unique(test_$group)))[apply(pred, 1, which.max)]
  
  confusion_list[[k]] <- table(test_$group, pred_label)
  
  View(confusion_list[[k]])
  
![1](https://user-images.githubusercontent.com/84159789/118224699-4fd67880-b449-11eb-985c-094264fdaf6f.png)

![conf](https://user-images.githubusercontent.com/84159789/118224398-b018ea80-b448-11eb-9265-035349433f32.PNG) model accuracy was 81.2% 

