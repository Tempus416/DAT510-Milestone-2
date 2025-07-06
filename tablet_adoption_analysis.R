# Load packages
library(readr)
library(rpart)
library(rpart.plot)
library(ggplot2)

# Load datasets
training <- read_csv("Chapter10DataSet_Training.csv")
scoring <- read_csv("Chapter10DataSet_Scoring.csv")

# CHECK FOR MISSING VALUES (ADD HERE)
cat("Missing values in training data:\n")
sapply(training, function(x) sum(is.na(x)))
cat("\nMissing values in scoring data:\n")
sapply(scoring, function(x) sum(is.na(x)))

# Drop User_ID
training$User_ID <- NULL
scoring$User_ID <- NULL

# Convert relevant columns to factors
categorical_cols <- c("Gender", "Marital_Status", "Website_Activity",
                      "Browsed_Electronics_12Mo", "Bought_Electronics_12Mo",
                      "Bought_Digital_Media_18Mo", "Bought_Digital_Books",
                      "Payment_Method", "Tablet_Adoption")

training[categorical_cols] <- lapply(training[categorical_cols], as.factor)

# Train decision tree
tree_model <- rpart(Tablet_Adoption ~ ., data = training, method = "class",
                    control = rpart.control(cp = 0.01, maxdepth = 5))

# EVALUATE MODEL PERFORMANCE (ADD HERE)
print(tree_model)
cat("\nModel complexity and cross-validation results:\n")
printcp(tree_model)

# Plot the tree
rpart.plot(tree_model, type = 2, extra = 104, fallen.leaves = TRUE,
           main = "Decision Tree for Tablet Adoption")

# Ensure factor format in scoring data
categorical_predictors <- categorical_cols[1:8]
scoring[categorical_predictors] <- lapply(scoring[categorical_predictors], as.factor)

# Predict both class and probabilities
predictions <- predict(tree_model, newdata = scoring, type = "class")
prob_predictions <- predict(tree_model, newdata = scoring, type = "prob")

scoring$Predicted_Adoption <- predictions
scoring <- cbind(scoring, prob_predictions)  # Adds probability columns# Predict
predictions <- predict(tree_model, newdata = scoring, type = "class")
scoring$Predicted_Adoption <- predictions

# Export results
write_csv(scoring, "Scoring_With_Predictions.csv")

# Bar chart of predicted categories
ggplot(scoring, aes(x = Predicted_Adoption)) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Predicted Adoption Categories",
       x = "Adoption Group", y = "Number of Customers")

# Histogram of Age by predicted category
ggplot(scoring, aes(x = Age, fill = Predicted_Adoption)) +
  geom_histogram(binwidth = 5, position = "dodge", color = "black") +
  theme_minimal() +
  labs(title = "Age Distribution by Predicted Adoption Group",
       x = "Age", y = "Count")
