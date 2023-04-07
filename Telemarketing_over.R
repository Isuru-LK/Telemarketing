# Import libraries
library(tidyverse)
library(stringr)

# Load the dataset
data <- read.csv("C:\\Users\\Isuru\\Desktop\\SEM 06\\Applied stat\\Final presentation\\train.csv",sep = ";")
data
nrow(data)
head(data,5)
summary(data)
str(data)

# Check for missing values
sum(is.na(data))
#(No missing values)

#Check for Outlier(For numerical variables)-----------------------------------

#Before that convert integers to numerics which need to check outliers
data$age <- as.numeric(data$age)
str(data$age)

boxplot(data$age) #Outliers in age

summary(data$age)
data <- data[-which(data$age > 70),]
summary(data$age)

boxplot(data$emp.var.rate) #no outliers in emp.var.rate

boxplot(data$cons.price.idx) #no outliers in cons.price.idx

boxplot(data$cons.conf.idx) #one oulier in cons.conf.idx
summary(data$cons.conf.idx) #check mean value
data <- data[-which(data$cons.conf.idx > -30),] #Remove outliers
summary(data$cons.conf.idx) #check whether removed outliers effect to the mean, if it is not, fine

boxplot(data$euribor3m) #no outliers in euribor3m

boxplot(data$nr.employed) #no outliers in nr.employed

#Check for duplicate data
duplicated(data)

#Convert categorical variables into factors
data$job <- as.factor(data$job)
data$marital <- as.factor(data$marital)
data$education <- as.factor(data$education)
data$default <- as.factor(data$default)
data$housing <- as.factor(data$housing)
data$loan <- as.factor(data$loan)
data$contact <- as.factor(data$contact)
data$month <- as.factor(data$month)
data$day_of_week <- as.factor(data$day_of_week)
data$poutcome <- as.factor(data$poutcome)
data$y <- as.factor(data$y)

str(data)

#Convert integers into numerics
data$campaign <- as.numeric(data$campaign)
data$pdays <- as.numeric(data$pdays)
data$previous <- as.numeric(data$previous)

str(data)

#check If this data set balance or not
table(data$y)

#---------------------------- OVERSAMPLING-------------------------------------

# Determine number of observations in majority class
majority_class_size <- sum(data$y == "no")
# Randomly replicate observations from the minority class
minority_class_replicates <- data[data$y == "yes", ][sample(1:nrow(data[data$y == "yes", ]), size = majority_class_size, replace = TRUE), ]
# Combine the minority class replicates with the majority class
balanced_data <- rbind(minority_class_replicates, data[data$y == "no", ])

table(balanced_data$y)

#Data set balance
nrow(balanced_data)
duplicate_rows <- duplicated(balanced_data)
duplicate_rows
table(duplicate_rows) #No duplicates

#Data preparation part is over 
#Checked missing values/outliars
#Set integers into numerics
#Set categorical variables into factors

#Univariate Analysis--------------------------------------------------------

#Bar chart for "Marital"
library(ggplot2)

ggplot(balanced_data, aes(x = marital)) +
  geom_bar(fill = "darksalmon",
           color="darkred") +
  labs(x = "Marital status",
       y = "Frequency",
       title = "Bar Chart of Marital status")

#month
ggplot(balanced_data, aes(x = month)) +
  geom_bar(fill = "skyblue",
           color="darkblue") +
  labs(x = "Months",
       y = "Frequency",
       title = "The month when the last contact was made with the customer")

#pie chart for "Education"
table(balanced_data$education)
count<-c(4229,2590,6426,11620,35,6489,16185,2290)
label<-c("basic.4y","basic.6y","basic.9y","high.school","illiterate","professional.course","university.degree","unknown")
library(plotrix)

piepercent<- round(100 * count / sum(count), 1)

pie3D(count, labels = piepercent,
      main = "Pie Chart of Education level", col = rainbow(length(count)))

legend("top", label,
       cex = 0.5, fill = rainbow(length(count)))


#Bar chart of pdays

balanced_data$pdays <- as.factor(balanced_data$pdays)

ggplot(balanced_data, aes(x = pdays)) +
  geom_bar(fill = "darksalmon",
           color="darkred") +
  labs(x = "Pdays",
       y = "Frequency",
       title = "Bar Chart of Pdays")
table(balanced_data$pdays)

#Histogram of pdays

ggplot(balanced_data, aes(x = pdays)) +
  geom_histogram(fill = "turquoise",
                 color = "turquoise4",
                 bins = 100) + # Bins means no of bars
  labs(title="Number of days that passed by after the client was last contacted",
       x = "Number of days")



#Histogram of emp.var.rate

ggplot(balanced_data, aes(x = emp.var.rate)) +
  geom_histogram(fill = "turquoise",
                 color = "turquoise4",
                 bins = 40) + # Bins means no of bars
  labs(title="Histogram of Employment variation rate",
       x = "Employment variation rate")
#boxplot 
boxplot(balanced_data$emp.var.rate, main = "Box Plot of Employment variation rate", xlab = "Employment Variation Rate", ylab = "Distribution", 
        col = "red" )

#Histogram of nr.employed

ggplot(balanced_data, aes(x = previous)) +
  geom_histogram(fill = "turquoise",
                 color = "turquoise4",
                 bins = 40) + # Bins means no of bars
  labs(title="Histogram of the avg. number of employees",
       x = "The avg. number of employees")


#---------------------Bivariate Analysis------------------------------------

#marital vs y
ggplot(balanced_data,
       aes(x = marital,
           fill = y)) +
  geom_bar(position = "dodge")+
  labs(title = "Response variable vs Marital Variable")

#education vs y
ggplot(balanced_data,
       aes(x = previous,
           fill = y)) +
  geom_bar(position = "dodge")+
  labs(title = "Response variable vs The number of contacts performed before this campaign",x="The number of contacts performed before this campaign")

#pdays vs y
balanced_data$pdays <- as.factor(balanced_data$pdays)
ggplot(balanced_data,
       aes(x = month,
           fill = y)) +
  geom_bar(position = "dodge")+
  labs(title = "Response variable vs Pdays Variable")


#emp.var.rate vs y


ggplot(balanced_data,
       aes(x = y,
           y = emp.var.rate)) +
  geom_boxplot() +
  labs(title = "Response According to employment variation rate")

#nr.employed vs y


ggplot(balanced_data,
       aes(x = y,
           y = nr.employed)) +
  geom_boxplot() +
  labs(title = "Response According to number of employees")

#---------------------Principal Component Analysis---------------------------------------

#Principal component analysis (PCA) allows us to summarize and to visualize the information in a 
#data set containing individuals/observations described by multiple inter-correlated quantitative 
#variables.

library("factoextra")
balanced_data
nrow(balanced_data)
str(balanced_data)
sum(is.na(balanced_data))


#There is 09 Quantitative variables

quanti <- subset(balanced_data,select = c("age","campaign","pdays","previous","emp.var.rate","cons.price.idx","cons.conf.idx","euribor3m","nr.employed"))

# Calculate the correlation matrix
cor_mat <- cor(quanti)

# Print the correlation matrix
print(cor_mat)

#pdays vs previous are highly correlated
#emp.var.rate vs euribor3m vs nr.employed are highly correlated

#Standarize Variables
stand <- scale(quanti)

#PCA only quantitative variables

pca <- princomp(stand,cor = T)
summary(pca)  

#to determine the number of principal components

screeplot(pca,type = "lines")
#we select first 5 components because its explain 90% of the variation

pca$loadings[,1:5]



#Logistic regression apply for PC (only quantitative variables)

pca_components <- predict(pca, newdata = quanti)[, 1:5]
pca_components

# assume pca_components is a matrix
pca_components_df <- as.data.frame(pca_components)

install.packages("caret")
library(caret)

# Combine the principal components and response variable into a new data frame
pca_data <- data.frame(y = balanced_data$y, pca_components)
pca_data

# Split the data into training and test sets
set.seed(123)
train_index <- createDataPartition(pca_data$y, p = 0.7, list = FALSE)
train_data <- pca_data[train_index, ]
test_data <- pca_data[-train_index, ]

nrow(pca_data) #Total data -49864
nrow(train_data) #Train data -34906
nrow(test_data) #Test data -14958

#Build Model
logit_model <- glm(y ~ ., data = train_data, family = "binomial")

#to generate predictions on the test data
test_predictions <- predict(logit_model, newdata = test_data, type = "response")
summary(logit_model)

roc_data <- roc(test_data$y, test_predictions)

# Finally, plot the ROC curve
plot(roc_data, col = "blue", main = "ROC Curve", print.auc = TRUE)
#compute the confusion matrix
confusion_matrix <- table(test_data$y, ifelse(test_predictions > 0.5, "Yes", "No"))
confusion_matrix

# compute accuracy
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)
accuracy

#-----------------------------Model 02-------------------------------------------------------

#Without PCA build the model

balanced_data
str(balanced_data)


# Convert categorical variables to factors
factor_vars <- c("job", "marital", "education", "default", "housing", "loan", "contact", "month", "day_of_week", "poutcome")
balanced_data[factor_vars] <- lapply(balanced_data[factor_vars], as.factor)

# Convert response variable to a factor
balanced_data$y <- as.factor(balanced_data$y)

quanti <- subset(balanced_data,select = c("age","campaign","pdays","previous","emp.var.rate","cons.price.idx","cons.conf.idx","euribor3m","nr.employed"))
# Create dummy variables for categorical variables
telemarketing_dummies <- model.matrix(~.-1, data = balanced_data[, c(factor_vars, "age","campaign","pdays","previous","emp.var.rate","cons.price.idx","cons.conf.idx","euribor3m","nr.employed")])


# Combine dummy variables with numerical variables
telemarketing_combined <- cbind(y = balanced_data$y,telemarketing_dummies, quanti)

colnames(telemarketing_combined)[duplicated(names(telemarketing_combined))] <- paste0(colnames(telemarketing_combined)[duplicated(names(telemarketing_combined))], "_2")
# Split the data into training and testing sets
set.seed(123)
train_indices <- sample(1:nrow(telemarketing_combined), 0.7 * nrow(telemarketing_combined))
train_data <- telemarketing_combined[train_indices, ]
test_data <- telemarketing_combined[-train_indices, ]

# Build the logistic regression model
logit_model <- glm(y ~ ., data = train_data, family = binomial)
summary(logit_model)

nrow(train_data)
nrow(balanced_data)

# Make predictions on the test set
predicted_probabilities <- predict(logit_model, newdata = test_data, type = "response")

roc_data <- roc(test_data$y, predicted_probabilities)

# Finally, plot the ROC curve
plot(roc_data, col = "blue", main = "ROC Curve", print.auc = TRUE)

#compute the confusion matrix
confusion_matrix <- table(test_data$y, ifelse(predicted_probabilities > 0.5, "Yes", "No"))
confusion_matrix

# compute accuracy
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)
accuracy


#----------Model 03----------------------------------------------

balanced_data

# Convert categorical variables to factors
factor_vars <- c("job", "marital", "education", "default", "housing", "loan", "contact", "month", "day_of_week", "poutcome")
balanced_data[factor_vars] <- lapply(balanced_data[factor_vars], as.factor)

# Convert response variable to a factor
balanced_data$y <- as.factor(balanced_data$y)

quanti <- subset(balanced_data,select = c("age","campaign","pdays","previous","emp.var.rate","cons.price.idx","cons.conf.idx","euribor3m","nr.employed"))
# Create dummy variables for categorical variables
telemarketing_dummies <- model.matrix(~.-1, data = balanced_data[, c(factor_vars, "age","campaign","pdays","previous","emp.var.rate","cons.price.idx","cons.conf.idx","euribor3m","nr.employed")])

# Calculate the correlation matrix
cor_mat <- cor(telemarketing_dummies)

# Print the correlation matrix
print(cor_mat)

#Standarize Variables
stand3 <- scale(telemarketing_dummies)

pca3 <- prcomp(stand3,cor = T)
#to determine the number of principal components

screeplot(pca3,type = "lines")
#we select first 5 components because its explain 90% of the variation

pca3$loadings[,1:6]

#Logistic regression apply for PC (only quantitative variables)

pca_components3 <- predict(pca3, newdata = telemarketing_dummies)[, 1:29]
pca_components3

# assume pca_components is a matrix
pca_components_df3 <- as.data.frame(pca_components3)

install.packages("caret")
library(caret)

str(balanced_data)
# Combine the principal components and response variable into a new data frame
pca_data3 <- data.frame(y = balanced_data$y, pca_components3)
pca_data3

colnames(pca_data3)[duplicated(names(pca_data3))] <- paste0(colnames(pca_data3)[duplicated(names(pca_data3))], "_2")
# Split the data into training and testing sets
set.seed(123)
train_indices <- sample(1:nrow(pca_data3), 0.7 * nrow(pca_data3))
train_data3 <- pca_data3[train_indices, ]
test_data3 <- pca_data3[-train_indices, ]

duplicated(train_data3)


# Build the logistic regression model
logit_model3 <- glm(y ~ ., data = train_data3, family = "binomial")

nrow(train_data)
nrow(balanced_data)

# Make predictions on the test set
predicted_probabilities3 <- predict(logit_model3, newdata = test_data3, type = "response")
library(pROC)
roc_data <- roc(test_data3$y, predicted_probabilities3)

# Finally, plot the ROC curve
plot(roc_data, col = "blue", main = "ROC Curve", print.auc = TRUE)

#compute the confusion matrix
confusion_matrix3 <- table(test_data3$y, ifelse(predicted_probabilities3 > 0.5, "Yes", "No"))
confusion_matrix3

# compute accuracy
accuracy3 <- sum(diag(confusion_matrix3))/sum(confusion_matrix3)
accuracy3
