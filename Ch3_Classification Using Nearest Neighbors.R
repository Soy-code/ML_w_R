##### ch3: Lazy Learning - Classification Using Nearest Neighbors #####
#### Example - diagnosing breast cancer with the k-nn algorithm ####

## step2 - exploring and preparing the data
wbcd = read.csv("C:/Rdata/ML/ch3/wisc_bc_data.csv", stringsAsFactors = FALSE)
str(wbcd)
wbcd = wbcd[-1]
table(wbcd$diagnosis) 
wbcd$diagnosis = factor(wbcd$diagnosis, levels = c("B", "M"), 
                        labels = c("Benign", "Malignant"))
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])

## Transformation - normalizing numeric data 
normalize = function(x) { return((x - min(x)) / (max(x) - min(x))) }
normalize(c(1, 2, 3, 4, 5))
normalize(c(10, 20, 30, 40 , 50))
wbcd_n = as.data.frame(lapply(wbcd[2:31], normalize))
summary(wbcd_n$area_mean)

## Data preparation - creating training and test datasets
wbcd_train = wbcd_n[1:469, ]  
wbcd_test = wbcd_n[470:569, ] 
wbcd_train_labels = wbcd[1:469, 1]
wbcd_test_labels = wbcd[470:569, 1]

## Step3 - training a model on the data
install.packages("class")
library(class)
wbcd_test_pred = knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k = 21)

## Step4 - evaluating model performance
library(gmodels)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq = FALSE)

## Step5 - improving model performance
wbcd_z = as.data.frame(scale(wbcd[-1]))
summary(wbcd_z$area_mean)
wbcd_train = wbcd_z[1:469, ]
wbcd_test = wbcd_z[470:569, ]
wbcd_train_labels = wbcd[1:469, 1]
wbcd_test_labels = wbcd[470:569, 1]
wbcd_test_pred = knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k = 21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq = FALSE)

## Testing alternative values of k 
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=1)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=5)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=11)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=15)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=27)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)
