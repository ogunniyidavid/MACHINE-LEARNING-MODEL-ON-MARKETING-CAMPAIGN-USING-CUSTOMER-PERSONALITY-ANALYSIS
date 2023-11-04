# i will install some packages we will need for this project
install.packages('tidyverse')
install.packages('dslabs')
install.packages("lubridate")
install.packages('corrplot')
install.packages("gridExtra")
install.packages("GGally")
install.packages("knitr")
install.packages('naniar')
install.packages("caret")
install.packages("ggthemes")
install.packages("tidyr")
install.packages("cluster")
install.packages("ggplot2")
install.packages("corrgram")
install.packages("cowplot")
install.packages("caret")
install.packages("rpart.plot")
install.packages("e1071")

library(readr)
library(dplyr)
library(naniar)
library(lubridate)
library(caret)
library(corrplot)
library(tidyr)
library(cluster)
library(ggplot2)
library(corrgram)
library(ggthemes)
library(cowplot)
library(corrplot)
library(rpart.plot)

#importing our csv file into the studio

customers_data <- read_delim("C:/Users/B1102372/OneDrive - Teesside University/Documents/CIS4047/MY ICA/marketing_campaign.csv", 
                                 delim = "\t", escape_double = FALSE, 
                                 trim_ws = TRUE)
View(customers_data)



# checking for missing value
n_miss(customers_data)

# checking the variables with the missing value
miss_var_summary(customers_data)

# removing the missing values
customers_data = na.omit(customers_data)
dim(customers_data)


# Getting Age from Year_Birth
customers_data = customers_data %>%
  mutate(Age = 2021 - Year_Birth)

# visualizing the Age column
customers_data %>%
  ggplot(aes(1:length(ID), Age)) +
  geom_point() +
  theme_bw() +
  labs(x = "ID")

# setting the age to maximum of 95 and in the process removing the three outliers in Variable Age
customers_data = customers_data %>%
  filter(Age < 95)

# visualizing the income column
customers_data %>%
  ggplot(aes(1:length(ID), Income)) +
  geom_point() +
  theme_bw() +
  labs(x = "ID")

# Removing the outlier from the Income variable by putting a cap of 300,000
customers_data = customers_data %>%
  filter(Income < 300000)


# modifying The Recency column to Active(0-30days) and Not active(31-100)
customers_data = customers_data %>%
  mutate(Recency = ifelse(Recency <= 30,
                          "Active",
                          "Not Active") )


# converting the marital status into two categories(single and partner).
customers_data = customers_data %>%
  mutate(Marital_Status = replace(Marital_Status, Marital_Status =="Divorced" | Marital_Status == "Widow" | Marital_Status == "Alone" | Marital_Status == "Absurd" | Marital_Status == "YOLO", "Single"))

customers_data = customers_data %>%
  mutate(Marital_Status = replace(Marital_Status, Marital_Status == "Together" | Marital_Status == "Married", "Partner"))


# converting Education into two categories too
customers_data = customers_data %>%
  mutate(Education = replace(Education, Education == "2n Cycle" | Education == "Basic", "Non-Graduate"))

customers_data = customers_data %>%
  mutate(Education = replace(Education, Education == "PhD" | Education == "Graduation" | Education == "Master", "Graduate"))

# creating a total spending for each household by adding the number of products bought on wines,fruits,meats,fish,sweet and Gold together
customers_data = customers_data %>%
  mutate(Total_spending = MntWines+MntFruits+MntMeatProducts+MntFishProducts+MntSweetProducts+MntGoldProds)


# getting the number of children in each household and convert into two categories
customers_data = customers_data %>%
  mutate(Children_in_household = Kidhome + Teenhome)



# removing some columns that are not needed for our project
customers_data = customers_data %>%
  select(-ID, -Year_Birth, -Teenhome, -Dt_Customer, -Kidhome, -Z_CostContact, -Z_Revenue)



#  doing data visualization and checking how data is distributed
mar_plot = ggplot(data = customers_data, aes(Marital_Status, fill = Marital_Status))
mar_plot + geom_histogram(stat = "count")

Edu_plot =  ggplot(data = customers_data, aes(Education, fill = Education))
Edu_plot + geom_histogram(stat = "count")

# products bought distribution visualization
customers_data %>% 
  pivot_longer(
    cols = starts_with("Mnt")
  ) %>% 
  select(name, value) %>% 
  ggplot(aes(value, fill = name)) + 
  geom_histogram() + 
  facet_wrap(vars(name), scales = "free") + 
  labs(x = "",
       y = "", 
       subtitle = "Products Purchased distribution")


# Response, our target column distribution visualization

Resp_plot =  ggplot(data = customers_data, aes(Response, fill = Response))
Resp_plot + geom_histogram(stat = "count")



# some attributes Vs response visualization

plot_grid(
  ggplot(customers_data)+geom_bar(aes(x=Education,y=Response),stat = "identity"),
  ggplot(customers_data)+geom_bar(aes(x=Marital_Status,y=Response),stat = "identity"),
  ggplot(customers_data)+geom_bar(aes(x=Children_in_household,y=Response),stat = "identity"),
  ggplot(customers_data)+geom_bar(aes(x=Recency,y=Response),stat = "identity"),
  ggplot(customers_data)+geom_bar(aes(x=Response,y=Complain),stat = "identity")
)


# converting the factor columns, we want to use for our training to numeric.
# starting with the Education column,then the Marital status, Children in household, Recency, Total spending.
customers_data = customers_data %>%
  mutate(Education = recode(Education,
                            "Graduate" = 1,
                            "Non-Graduate" = 0))

customers_data = customers_data %>%
  mutate(Marital_Status = recode(Marital_Status,
                                 "Single" = 1,
                                 "Partner" = 2))

customers_data = customers_data %>%
  mutate(Recency = recode(Recency,
                          "Not Active" = 0,
                          "Active" = 1))




# checking the data type of all attributes
sapply(customers_data, class)




# doing the correlation matrix on our cleaned dataset

customers_data_cor = cor(customers_data[,1:25])
corrplot(customers_data_cor, method = "circle")


# Now we split the dataset into two set, training set and testing set
# The target variable is Response, and we will allocate 70% for training and 30% testing
intrain = createDataPartition(y = customers_data$Response, p=0.7, list = FALSE)
training = customers_data[intrain,]
testing = customers_data[-intrain,]

# Viewing the columns and rows we have for training and testing
dim(training)
dim(testing)

summary(customers_data)


# training the data for Svm 

training[["Response"]] = factor(training[["Response"]])

trctrl = trainControl(method = "repeatedcv", number = 10, repeats = 3)


svm_Linear = train(Response ~., data = training, method = "svmLinear",trControl=trctrl,preProcess = c("center", "scale"),tuneLength = 10)

# testing the data
svm_Linear
svm_test = predict(svm_Linear, newdata = testing)
svm_test

# the confusion matrix for svm model
svm_cm = print(confusionMatrix(table(svm_test, testing$Response)))


# Tuning 
grid = expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
svm_Linear_Grid <- train(Response ~., data = training, method = "svmLinear",trControl=trctrl,preProcess = c("center", "scale"),tuneGrid = grid,tuneLength= 1)

svm_Linear_Grid
plot(svm_Linear_Grid)

# testing the tuned data
svm_test_grid = predict(svm_Linear_Grid, newdata = testing)
svm_test_grid

# confusion matrix of the tuned svm
svm_tune_cm = print(confusionMatrix(table(svm_test_grid, testing$Response)))




# Decision tree model
customers_data$Response = as.factor(customers_data$Response)
set.seed(1) 
inTrain = createDataPartition(customers_data$Response, p = .7)[[1]]
# Assign the 70% of observations to training data 
training <- customers_data[inTrain,] 
# Assign the remaining 30% of observations to testing data 
testing <- customers_data[-inTrain,] 
# Setting the seed (in order all results to be fully reproducible) and apply a prediction #Model with all variables
set.seed(2)
model.all <- train(Response ~ ., method="rpart", data = training)
# Applying the prediction
prediction <- predict(model.all, newdata= testing)
# Checking the accuracy of the prediction model by printing the confusion matrix
dt_cm = print(confusionMatrix(prediction, testing$Response), digits=4)


tree = rpart(Response~., data = customers_data, cp=.05)
#Plotting the Classification Tree
rpart.plot(tree, box.palette = "RdBu", shadow.col = "gray", nn = TRUE, main = "Classification Tree of Marketing Campaingn")



# Random forest model
rf.fit <- train(Response~., data = customers_data, method = "rf", trainControl = trainControl)

rf.predict <- predict(rf.fit, newdata = testing)

rf_cm = print(confusionMatrix(rf.predict, testing$Response), digits=4)



# comparing the models getting the data from confusion matrix
svm_data = c(svm_cm$byClass)
svm_tune_data = c(svm_tune_cm$byClass)
dt_data = c(dt_cm$byClass)
rf_data = c(rf_cm$byClass)


# new data frame for the models
modeldf = data.frame( SVM=c(svm_data), SVMs=c(svm_tune_data), DT=c(dt_data), RF=c(rf_data))


library(RColorBrewer)

#plotting performance for each model
barplot(t(as.matrix(modeldf)), beside=TRUE,col=brewer.pal(4, 'Spectral') ,
        legend=c('SVM', 'SVMs', 'DT', 'RF'), args.legend = list(x="topright"), ylim = c(0,1.5),
        names.arg = rownames(modeldf), main = 'Predicting Performance for the models', las = 2)



