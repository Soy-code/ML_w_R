setwd('E:\\BITAmin\\Machine Learning with R, Second Edition_Code\\Chapter 06')
launch=read.csv('challenger.csv', stringsAsFactors = T)
str(launch)

b=cov(launch$temperature, launch$distress_ct) / var(launch$temperature) ; b
a <- mean(launch$distress_ct) - b * mean(launch$temperature)
a

r <- cov(launch$temperature, launch$distress_ct) / (sd(launch$temperature) * sd(launch$distress_ct))
r
cor(launch$temperature, launch$distress_ct)
r * (sd(launch$distress_ct) / sd(launch$temperature))

model <- lm(distress_ct ~ temperature, data = launch)
model
summary(model)

reg <- function(y, x) {
  x <- as.matrix(x)
  x <- cbind(Intercept = 1, x)
  b <- solve(t(x) %*% x) %*% t(x) %*% y
  colnames(b) <- "estimate"
  print(b)
}
str(launch)
reg(y = launch$distress_ct, x = launch[2])
reg(y = launch$distress_ct, x = launch[2:4])
model <- lm(distress_ct ~ temperature + field_check_pressure + flight_num, data = launch)
model


####
insurance <- read.csv("insurance.csv", stringsAsFactors = TRUE)
str(insurance)
summary(insurance$expenses)
hist(insurance$expenses)
table(insurance$region)
cor(insurance[c("age", "bmi", "children", "expenses")])  
pairs(insurance[c("age", "bmi", "children", "expenses")])
library(psych)
pairs.panels(insurance[c("age", "bmi", "children", "expenses")])
ins_model <- lm(expenses ~ age + children + bmi + sex + smoker + region,
                data = insurance)
summary(ins_model)
ins_model <- lm(expenses ~ ., data = insurance)
ins_model
summary(ins_model)

insurance$age2 <- insurance$age^2
insurance$bmi30 <- ifelse(insurance$bmi >= 30, 1, 0)
ins_model2 <- lm(expenses ~ age + age2 + children + bmi + sex +
                   bmi30*smoker + region, data = insurance)
summary(ins_model2)
### update 함수 ex)update(ins.reg, ~., -sex)
### AIC 값이 낮을수록 예측력이 높다. 
### step(ins.reg)


### 
tee <- c(1, 1, 1, 2, 2, 3, 4, 5, 5, 6, 6, 7, 7, 7, 7)
at1 <- c(1, 1, 1, 2, 2, 3, 4, 5, 5)
at2 <- c(6, 6, 7, 7, 7, 7)
bt1 <- c(1, 1, 1, 2, 2, 3, 4)
bt2 <- c(5, 5, 6, 6, 7, 7, 7, 7)

sdr_a <- sd(tee) - (length(at1) / length(tee) * sd(at1) + length(at2) / length(tee) * sd(at2))
sdr_b <- sd(tee) - (length(bt1) / length(tee) * sd(bt1) + length(bt2) / length(tee) * sd(bt2))

sdr_a
sdr_b


wine <- read.csv("whitewines.csv")
str(wine)
hist(wine$quality)
summary(wine)

wine_train <- wine[1:3750, ]
wine_test <- wine[3751:4898, ]

library(rpart)
## rpart 회귀나무를 생성하는 함수.
m.rpart <- rpart(quality ~ ., data = wine_train)
m.rpart
summary(m.rpart)
##alcohol이 잘 설명하는 변수이다.(?)

library(rpart.plot)
rpart.plot(m.rpart, digits = 3)
rpart.plot(m.rpart, digits = 4, fallen.leaves = TRUE, type = 3, extra = 101)
p.rpart <- predict(m.rpart, wine_test)
summary(p.rpart)
summary(wine_test$quality)
cor(p.rpart, wine_test$quality)

MAE <- function(actual, predicted) {
  mean(abs(actual - predicted))  
}
MAE(p.rpart, wine_test$quality)
mean(wine_train$quality) 
MAE(5.87, wine_test$quality)
library(RWeka)
m.m5p <- M5P(quality ~ ., data = wine_train)
m.m5p
summary(m.m5p)
p.m5p <- predict(m.m5p, wine_test)
summary(p.m5p)
cor(p.m5p, wine_test$quality)
MAE(wine_test$quality, p.m5p)
## 상관관계 증가, 평균오차 감소 -> 더 좋아짐.


# ---------------------------------- example -------------------------------------#
football=read.csv("FM2019.csv")
set.seed(123)
N=nrow(football)
str(football)
sampling=sample(N, N*0.7 )
ft_train=football[sampling, ]
ft_test=football[-sampling, ]
install.packages("rpart")
library(rpart)
m.part = rpart(Performance~., data = ft_train)
m.part
rpart.plot(m.part, digits=3)
p.rpart=predict(m.part, ft_test)
summary(p.rpart)
summary(ft_test$Performance)
cor(p.rpart, ft_test$Performance)
MAE=function(actual, predict) { mean(abs(actual-predict))}
MAE(ft_test$Performance, p.rpart)
mean(ft_train$Performance)
MAE(68.13, ft_test$Performance)

library(RWeka)
m.m5p=M5P(Performance~., data=ft_train)
m.m5p  ## num5 참ㄱ
summary(m.m5p)

p.m5p = predict(m.m5p, ft_test)
summary(p.m5p)
cor(p.m5p, ft_test$Performance)

cor(p.m5p, ft_test$Performance)
cor(p.rpart, ft_test$Performance)
