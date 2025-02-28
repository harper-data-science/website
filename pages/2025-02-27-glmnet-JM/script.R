####
## Title: HARUG R Implementation of the glmnet package
## Author: J. Mhango
## Date: 2025-02-27
####

# Load necessary libraries
library(caret)
library(glmnet)


# Load the data
data <- read.csv("orf_2011_220225.csv")
y<-as.factor(data$Orf.yes.no)
data<-na.omit(data[,c(7:21)])
data[, sapply(data, is.numeric)] <- scale(data[, sapply(data, is.numeric)])
data$yvar<-y

set.seed(123)



## Data partitioning ----

# Partition the data into training (80%) and testing (20%) sets
trainIndex <- createDataPartition(data$yvar, p = 0.8, list = FALSE)
trainData <- data[trainIndex, ]
testData  <- data[-trainIndex, ]


## Setup the training parameter space and cross validation settings ----

# Define cross-validation method (10-fold CV)
trainControlObj <- trainControl(method = "cv", number = 10)

# Set up a grid of lambda values for tuning; alpha is fixed to 0 for ridge regression
ridgeGrid <- expand.grid(alpha = 1, 
                         lambda = seq(0.001, 1, length = 100))


## Now train the model ----

# Train the ridge regression model using caret's train() function with the 'glmnet' method
set.seed(123)
ridgeModel <- train(yvar ~ ., 
                    data = trainData, 
                    method = "glmnet", 
                    trControl = trainControlObj, 
                    tuneGrid = ridgeGrid)

# Print the trained model and tuning results
print(ridgeModel)

## Predict ----

# Make predictions on the test set
predictions <- predict(ridgeModel, newdata = testData)
confusionMatrix(predictions,testData$yvar)

## Draw the confmat ----

# Load necessary libraries
library(caret)
library(ggplot2)
library(reshape2)  # for melting the confusion matrix table

# Generate the confusion matrix using caret's confusionMatrix function
confmat <- confusionMatrix(predictions, testData$yvar)

# Print the confusion matrix object
print(confmat)

# Convert the table from the confusion matrix into a data frame for ggplot
cm_df <- as.data.frame(confmat$table)

# Plot the confusion matrix as a heatmap
ggplot(cm_df, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), color = "white", size = 6) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Confusion Matrix", x = "Predicted", y = "Actual") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5))


varImp(ridgeModel)

## Model ----

# Extract the coefficients for the best lambda value selected during training
best_lambda <- ridgeModel$bestTune$lambda
coef_values <- coef(ridgeModel$finalModel, s = best_lambda)
print(coef_values)