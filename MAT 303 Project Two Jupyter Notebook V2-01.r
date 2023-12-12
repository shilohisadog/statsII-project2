
print("This step will first install three R packages. Please wait until the packages are fully installed.")
print("Once the installation is complete, this step will print 'Installation complete!'")

install.packages("ResourceSelection")
install.packages("pROC")
install.packages("rpart.plot")

print("Installation complete!")

heart_data <- read.csv(file="heart_disease.csv", header=TRUE, sep=",")

# Converting appropriate variables to factors  
heart_data <- within(heart_data, {
   target <- factor(target)
   sex <- factor(sex)
   cp <- factor(cp)
   fbs <- factor(fbs)
   restecg <- factor(restecg)
   exang <- factor(exang)
   slope <- factor(slope)
   ca <- factor(ca)
   thal <- factor(thal)
})

head(heart_data, 10)

print("Number of variables")
ncol(heart_data)

print("Number of rows")
nrow(heart_data)

# Loading Project two data set
heart_data <- read.csv(file="heart_disease.csv", header=TRUE, sep=",")

# Converting appropriate variables to factors  
heart_data <- within(heart_data, {
   target <- factor(target)
   sex <- factor(sex)
   cp <- factor(cp)
   fbs <- factor(fbs)
   restecg <- factor(restecg)
   exang <- factor(exang)
   slope <- factor(slope)
   ca <- factor(ca)
   thal <- factor(thal)
})

head(heart_data, 10)

print("Number of variables")
ncol(heart_data)

print("Number of rows")
nrow(heart_data)

# Create the complete model
logit <- glm(target ~ age + trestbps + exang + thalach, data = heart_data, family = "binomial")
summary(logit)

# Predict default or no_default for the data set using the model
default_model_data <- heart_data[c('age', 'trestbps', 'exang', 'thalach')]
pred <- predict(logit, newdata=default_model_data, type='response')

# If the predicted probability of target is >=0.50 then predict heart disease (target='1'), otherwise predict no heart 
# disease (target='0') 
depvar_pred = as.factor(ifelse(pred >= 0.5, '1', '0'))

# Goodness of Fit calculations
library(ResourceSelection)

# print("Hosmer-Lemeshow Goodness of Fit Test")
hl = hoslem.test(logit$y, fitted(logit), g=50)
hl

#  This step will print the Wald-based 95% confidence intervals for the slope parameters.
conf_int <- confint.default(logit, level=0.95)
round(conf_int,4)

# This creates the confusion matrix
conf.matrix <- table(heart_data$target, depvar_pred)[c('0','1'),c('0','1')]
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ": default=")
colnames(conf.matrix) <- paste("Prediction", colnames(conf.matrix), sep = ": default=")

# Print nicely formatted confusion matrix
print("Confusion Matrix")
format(conf.matrix,justify="centre",digit=2)

# ROC code
library(pROC)

labels <- heart_data$target
predictions <- logit$fitted.values

roc <- roc(labels ~ predictions)

# print("ROC Curve")
# True Positive Rate (Sensitivity) and False Positive Rate (1 - Specificity)
plot(roc, legacy.axes = TRUE)

print("Area Under the Curve (AUC)")
round(auc(roc),4)

print("Prediction: age is 50, resting bp of 122, has exercise induced angina, and max heart rate of 140")
newdata1 <- data.frame(age=50, trestbps=122, exang="1", thalach=140)
pred1 <- predict(logit, newdata1, type='response')
round(pred1, 4)

print("Prediction: age is 50, resting bp of 130, does not have exercise induced angina, and max heart rate of 165")
newdata2 <- data.frame(age=50, trestbps=130, exang="0", thalach=165)
pred2 <- predict(logit, newdata2, type='response')
round(pred2, 4)



# Loading Project two data set
heart_data <- read.csv(file="heart_disease.csv", header=TRUE, sep=",")

# Converting appropriate variables to factors  
heart_data <- within(heart_data, {
   target <- factor(target)
   sex <- factor(sex)
   cp <- factor(cp)
   fbs <- factor(fbs)
   restecg <- factor(restecg)
   exang <- factor(exang)
   slope <- factor(slope)
   ca <- factor(ca)
   thal <- factor(thal)
})

head(heart_data, 10)

print("Number of columns")
ncol(heart_data)

print("Number of rows")
nrow(heart_data)

# Create the complete model
logit <- glm(target ~ age + trestbps + cp + thalach + I(age^2) + I(age*thalach), data = heart_data, family = "binomial")
summary(logit)

# Predict default or no_default for the data set using the model
default_model_data <- heart_data[c('age', 'trestbps', 'cp', 'thalach')]

# Calculate the quadratic and interaction terms
default_model_data$age_squared <- default_model_data$age^2
default_model_data$interaction_term <- default_model_data$age * default_model_data$thalach

pred <- predict(logit, newdata=default_model_data, type='response')

# If the predicted probability of target is >=0.50 then predict heart disease (target='1'), otherwise predict no heart 
# disease (target='0') 
depvar_pred = as.factor(ifelse(pred >= 0.5, '1', '0'))

# Goodness of Fit calculations
library(ResourceSelection)

# print("Hosmer-Lemeshow Goodness of Fit Test")
hl = hoslem.test(logit$y, fitted(logit), g=50)
hl

#  This step will print the Wald-based 95% confidence intervals for the slope parameters.
conf_int <- confint.default(logit, level=0.95)
round(conf_int,4)

# This creates the confusion matrix
conf.matrix <- table(heart_data$target, depvar_pred)[c('0','1'),c('0','1')]
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ": default=")
colnames(conf.matrix) <- paste("Prediction", colnames(conf.matrix), sep = ": default=")

# Print nicely formatted confusion matrix
print("Confusion Matrix")
format(conf.matrix,justify="centre",digit=2)

# ROC code
library(pROC)

labels <- heart_data$target
predictions <- logit$fitted.values

roc <- roc(labels ~ predictions)

# print("ROC Curve")
# True Positive Rate (Sensitivity) and False Positive Rate (1 - Specificity)
plot(roc, legacy.axes = TRUE)

print("Area Under the Curve (AUC)")
round(auc(roc),4)

print("Prediction: age is 50, resting bp of 115, no chest pain (cp=0), and max heart rate of 133")
newdata1 <- data.frame(age=50, trestbps=122, cp="0", thalach=133)
pred1 <- predict(logit, newdata1, type='response')
round(pred1, 4)

print("Prediction: age is 50, resting bp of 125, typicl angina (cp=1), and max heart rate of 155")
newdata2 <- data.frame(age=50, trestbps=125, cp="1", thalach=155)
pred2 <- predict(logit, newdata2, type='response')
round(pred2, 4)



set.seed(6522048)
library(randomForest)

# Loading Project two data set
heart_data <- read.csv(file="heart_disease.csv", header=TRUE, sep=",")

vars <- c('target', 'age','sex','cp', 'trestbps', 'chol', 'restecg', 'exang', 'ca')

heart_data <- heart_data[vars]

head(heart_data, 10)

print("Total number of columns")
ncol(heart_data)

print("Total number of rows")
nrow(heart_data)

# Partition the data set into training and testing data
samp.size = floor(0.85*nrow(heart_data))

# Training set
print("Number of rows for the training set")
train_ind = sample(seq_len(nrow(heart_data)), size = samp.size)
train.data = heart_data[train_ind,]
nrow(train.data)

# Testing set 
print("Number of rows for the testing set")
test.data = heart_data[-train_ind,]
nrow(test.data)

# Graph the training and testing error against the number of trees using a 
# classification random forest model for the presence of heart disease (target) 
# using variables age (age), sex (sex), chest pain type (cp), resting blood pressure (trestbps), 
# cholesterol measurement (chol), resting electrocardiographic measurement (restecg), exercise-induced angina (exang),
# and number of major vessels (ca). Use a maximum of 150 trees. 

# checking
#=====================================================================
train = c()
test = c()
trees = c()

for(i in seq(from=1, to=150, by=1)) {
    
    trees <- c(trees, i)
    
    model_rf3 <- randomForest(target ~ age + sex + cp + trestbps + chol + restecg + exang + ca, data=train.data, ntree = i)
    
    train.data.predict <- predict(model_rf3, train.data, type = "response")  # Use type = "response" to get probabilities
    train_error = mean(train.data$target != (train.data.predict > 0.5))  # Apply a threshold (e.g., 0.5) to classify
    
    train <- c(train, train_error)

    test.data.predict <- predict(model_rf3, test.data, type = "response")
    test_error = mean(test.data$target != (test.data.predict > 0.5))
    
    test <- c(test, test_error)
}

plot(trees, train,type = "l",ylim=c(0,1.0),col = "red", xlab = "Number of Trees", ylab = "Classification Error")
lines(test, type = "l", col = "blue")
legend('topright',legend = c('training set','testing set'), col = c("red","blue"), lwd = 2 )

# Create a classification random forest model with 20 trees, the optimal number of trees determined from the previous step

model_rf4 <- randomForest(target ~ age + sex + cp + trestbps + chol + restecg + exang + ca, data=train.data, ntree = 20)

# Training set confusion matrix and metrics
print("Confusion Matrix: TRAINING set based on random forest model with 20 trees")
train.data.predict <- predict(model_rf4, train.data, type = "response")
train.data.predict_class <- as.factor(train.data.predict > 0.5)  

conf.matrix_train <- table(train.data$target, train.data.predict_class)
colnames(conf.matrix_train) <- c('Predicted 0', 'Predicted 1')
rownames(conf.matrix_train) <- c('Actual 0', 'Actual 1')

# Print confusion matrix for training set
print(conf.matrix_train)

# Testing set confusion matrix and metrics
print("Confusion Matrix: TESTING set based on random forest model with 20 trees")
test.data.predict <- predict(model_rf4, test.data, type = "response")  
test.data.predict_class <- as.factor(test.data.predict > 0.5)  

conf.matrix_test <- table(test.data$target, test.data.predict_class)
colnames(conf.matrix_test) <- c('Predicted 0', 'Predicted 1')
rownames(conf.matrix_test) <- c('Actual 0', 'Actual 1')

# Print confusion matrix for testing set
print(conf.matrix_test)



set.seed(6522048)
library(randomForest)

# Loading Project two data set
heart_data <- read.csv(file="heart_disease.csv", header=TRUE, sep=",")

head(heart_data, 10)

print("Total number of columns")
ncol(heart_data)

print("Total number of rows")
nrow(heart_data)

# Partition the data set into training and testing data
samp.size = floor(0.80*nrow(heart_data))

# Training set
print("Number of rows for the training set")
train_ind = sample(seq_len(nrow(heart_data)), size = samp.size)
train.data = heart_data[train_ind,]
nrow(train.data)

# Testing set 
print("Number of rows for the testing set")
test.data = heart_data[-train_ind,]
nrow(test.data)

# Fit a random forest regression model
model_rf5 <- randomForest(thalach ~ age + sex + cp + trestbps + chol + restecg + exang + ca, data = train.data, ntree = 2)

# Root mean squared error
RMSE = function(pred, obs) {
    return(sqrt( sum( (pred - obs)^2 )/length(pred) ) )
}

print("======================================================================================================================")
print('Root Mean Squared Error: TRAINING set based on random forest regression model built using 2 trees')
pred <- predict(model_rf5, newdata=train.data, type='response')
RMSE(pred, train.data$thalach)


print("======================================================================================================================")
print('Root Mean Squared Error: TESTING set based on random forest regression model built using 2 trees')
pred <- predict(model_rf5, newdata=test.data, type='response')
RMSE(pred, test.data$thalach)


# checking
#=====================================================================
train = c()
test = c()
trees = c()

for(i in seq(from=1, to=80, by=1)) {
    trees <- c(trees, i)
    model_rf6 <- randomForest(thalach ~ age + sex + cp + trestbps + chol + restecg + exang + ca, data=train.data, ntree = i)
    
    pred <- predict(model_rf6, newdata=train.data, type='response')
    rmse_train <-  RMSE(pred, train.data$thalach)
    train <- c(train, rmse_train)
    
    pred <- predict(model_rf6, newdata=test.data, type='response')
     rmse_test <-  RMSE(pred, test.data$thalach)
    test <- c(test, rmse_test)
}
 
plot(trees, train,type = "l",ylim=c(0,80),col = "red", xlab = "Number of Trees", ylab = "Root Mean Squared Error")
lines(test, type = "l", col = "blue")
legend('topright',legend = c('training set','testing set'), col = c("red","blue"), lwd = 2 )

# Fit the random forest regression model with the number of trees (15) determined by the previous step
model_rf7 <- randomForest(thalach ~ age + sex + cp + trestbps + chol + restecg + exang + ca, data = train.data, ntree = 15)

# Root mean squared error
RMSE = function(pred, obs) {
    return(sqrt( sum( (pred - obs)^2 )/length(pred) ) )
}

print("======================================================================================================================")
print('Root Mean Squared Error: TRAINING set based on random forest regression model built using 15 trees')
pred <- predict(model_rf7, newdata=train.data, type='response')
RMSE(pred, train.data$thalach)


print("======================================================================================================================")
print('Root Mean Squared Error: TESTING set based on random forest regression model built using 15 trees')
pred <- predict(model_rf7, newdata=test.data, type='response')
RMSE(pred, test.data$thalach)


