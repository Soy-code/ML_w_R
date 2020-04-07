##### ch2: Managing and Understanding Data #####
#### R data structures ####

## Vectors 
subject_name = c("John Doe", "Jane Doe", "Steve Graves") # character
temperature = c(98.1, 98.6, 101.4)  # double
flu_status = c(FALSE, FALSE, TRUE) # logical
temperature[2]
temperature[2:3]
temperature[-2]
temperature[c(T, T, F)]

## Factors 
gender = factor(c("MALE", "FEMALE", "MALE"))
gender
blood = factor(c("O", "AB", "A"), levels = c("A", "B", "AB", "O"))
blood[1:2]
symptoms = factor(c("SEVERE", "MILD", "MODERATE"), 
                  levels = c("MILD", "MODERATE", "SEVERE"), 
                  order = TRUE)
symptoms
symptoms > "MODERATE"

## Lists 
subject_name[1]
temperature[1]
flu_status[1]
gender[1]
blood[1]
symptoms[1]
subject1 = list(fullname = subject_name[1], temperature = temperature[1], 
                flu_status = flu_status[1], gender = gender [1], 
                blood = blood[1], symptoms = symptoms[1])
subject1
subject1[2]
subject1[[2]]
subject1$temperature
subject1[c("temperature", "flu_status")]

## Data frames
pt_data = data.frame(subject_name, temperature, flu_status, gender, blood, 
                     symptoms, stringsAsFactors = FALSE)  
pt_data
pt_data$subject_name
pt_data[c("temperature", "flu_status")]
pt_data[1, 2]  
pt_data[c(1,3), c(2,4)]  
pt_data[, 1]
pt_data[1, ]
pt_data[ , ]
pt_data[c(1, 3), c("temperature", "gender")]   
pt_data[-2, c(-1, -3, -5, -6)]

## Matrixes and arrays 
m = matrix(c(1, 2, 3, 4), nrow = 2)
m      
m = matrix(c(1, 2, 3, 4), ncol = 2)
m       
m = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
m
m = matrix(c(1, 2, 3, 4, 5, 6), ncol = 2)
m
m[1, ]
m[, 1]

#### Managing data with R ####

## Saving, loading, and removing R data structures 
save(x, y, z, file = "mydata.RData") 
load("mydata.RData") 
ls()
rm(m, subject1)
rm(list=ls())

## Importing and saving data from CSV files 
pt_data = read.csv("pt_data.csv", stringsAsFactors = FALSE)
mydata = read.csv("mydata.csv", stringsAsFactors = FALSE, header = FALSE)
write.csv(pt_data, file = "pt_data.csv", row.names = FALSE)

#### Exploring and understanding data ####
## Exploring the structure of data
usedcars = read.csv("C:/Rdata/ML/ch2/usedcars.csv", stringsAsFactors = FALSE)
str(usedcars)

## Exploring numeric variables 
summary(usedcars$year)
summary(usedcars[c("price", "mileage")])

## Measuring the central tendency - mean and median 
(36000 + 44000 + 56000) / 3   
mean(c(36000, 44000, 56000))
median(c(36000, 44000, 56000))

## Measuring spread - quartiles and the five number summary 
range(usedcars$price)
diff(range(usedcars$price))
IQR(usedcars$price)
quantile(usedcars$price)
quantile(usedcars$price, probs = c(0.01, 0.99))
quantile(usedcars$price, seq(from = 0, to = 1, by = 0.20))

## visualizing numeric variables - boxplots 
boxplot(usedcars$price, main = "Boxplot of Used car Prices", ylab = "Price ($)")
boxplot(usedcars$mileage, main = "Boxplot of Used car Mileage", ylab = "Odometer (mi.)")

## visualizing numeric variables - histograms 
hist(usedcars$price, main = "Histogram of Used Car Prices", xlab = "Price ($)")
hist(usedcars$mileage, main = "Histogram of Used Car Mileage", xlab = "Odometer (mi.)")


## Measuring spread - variance and standard deviation 
var(usedcars$price)
sd(usedcars$price)
var(usedcars$mileage)
sd(usedcars$mileage)

## Exploring categorical variables 
table(usedcars$year)
table(usedcars$model)
table(usedcars$color)
model_table = table(usedcars$model)
prop.table(model_table)
color_table = table(usedcars$color)
color_pct = prop.table(color_table) * 100
round(color_pct, digits = 1)

#### Exploring relationships between variables ####
## Visualizing relationships - scatterplot 
plot(x = usedcars$mileage, y= usedcars$price, main = "Scatterplot of Price vs. Mileage", 
     xlab = "Used Car Odometer (mi.)", ylab = "Used Car Price ($)")

## two-way cross-tabulation
usedcars$conservative = usedcars$ color %in% c("Black", "Gray", "Silver", "White")
table(usedcars$conservative)
install.packages("gmodels")
library(gmodels)
CrossTable(x = usedcars$model, y = usedcars$conservative)
