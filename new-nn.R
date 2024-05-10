install.packages("tseries")

library(tseries)
library(ggplot2)
library(readxl)

# Load the data from the Excel file
exchange_data <- read_excel("ExchangeUSD.xlsx")

# Check the current column names
names(exchange_data)

# Change the column names
names(exchange_data) <- c("Date", "Wdy", "Rate")

# check for missing values
sum(is.na(exchange_data))

# Looking if there is a trend going on
plot(exchange_data$Rate)

# Create a bar plot for the distribution of weekdays
ggplot(exchange_data, aes(x = factor(Wdy, levels = c("Mon", "Tue", "Wed", "Thu", "Fri")))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Weekdays", x = "Weekday", y = "Frequency")

# Augmented Dickey-Fuller (ADF) test if stationary
adf.test(exchange_data$Rate, alternative = "stationary")
#When you specified alternative = "stationary", the p-value was 0.1863, which is greater than the typical significance level of 0.05. This suggests that you fail to reject the null hypothesis, indicating that the series may not be stationary.

# Augmented Dickey-Fuller (ADF) test if non stationary/ explosive
adf.test(exchange_data$Rate, alternative = "explosive")
#When you specified alternative = "explosive", the p-value was 0.8137. This suggests even stronger evidence against stationarity, indicating that the series is likely non-stationary with an explosive behavior.

# this is geometric
# Autocorrelation Function (ACF)
acf(exchange_data$Rate)

# this is p significant lags
# 
pacf(exchange_data$Rate)

################### concluded that the better way is AR (AutoRegressive) aproach over MA (Moving Average) ################

# Import neuralnet library
library(neuralnet)
require(RSNNS)
require(quantmod)

# Experiment with various time delays (up to t-4)
delay=2

create_input_data <- function(data, delay) {
  io_matrix_list <- list()
  
  for (i in 1:delay) {
    inputs <- matrix(NA, nrow = length(data)- i, ncol = i)
    
    for (j in 1:(length(data)- i)) {
      inputs[j,] <- data[j:(j + i - 1)]
    }
    
    outputs <- data[(i + 1) : (length(data))]
    
    input_df <- as.data.frame(inputs)
    colnames(input_df) <- paste0("input_", 1:i)
    
    output_df <- as.data.frame(outputs)
    
    io_matrix_df <- cbind(input_df, output_df)
    
    io_matrix_list[[i]] <- io_matrix_df 
  }
  return(io_matrix_list)
}

# Function to normalize data for each input/output matrix
normalize_io_matrix <- function(io_matrix) {
  normalized_matrix <- apply(io_matrix, 2, function(x) (x - min(x)) / (max(x) - min(x)))
  return(normalized_matrix)
}

# Function to create input/output matrices for different time delays
create_normalized_io_matrices <- function(data, delay) {
  normalized_io_matrix_list <- list()
  
  for (i in 1:delay) {
    inputs <- matrix(NA, nrow = length(data)- i, ncol = i)
    
    for (j in 1:(length(data)- i)) {
      inputs[j,] <- data[j:(j + i - 1)]
    }
    
    outputs <- data[(i + 1) : (length(data))]
    
    input_df <- as.data.frame(inputs)
    colnames(input_df) <- paste0("input_", 1:i)
    
    output_df <- as.data.frame(outputs)
    
    io_matrix_df <- cbind(input_df, output_df)
    
    # Normalize the input/output matrix
    normalized_io_matrix <- normalize_io_matrix(io_matrix_df)
    
    normalized_io_matrix_list[[i]] <- normalized_io_matrix 
  }
  return(normalized_io_matrix_list)
}

head(exchange_data$Rate)
# Create normalized input/output matrices for different time delays
normalized_io_matrices <- create_normalized_io_matrices(exchange_data$Rate, delay)

sum(is.na(normalized_io_matrices))

str(normalized_io_matrices)

# Split data into training and testing sets (400 training, 100 testing)
train_size <- 400
test_size <- 100

train_data <- normalized_io_matrices[[1]][1:train_size, ]
# View(train_data)

# Extracting test data from the first element of normalized_io_matrices
test_data <- normalized_io_matrices[[1]][(train_size + 1):nrow(normalized_io_matrices[[1]]), ]
# View(test_data)

train_data <- create_input_data(train_data, delay)
# View(train_data[delay])

# train the model
model <- neuralnet(outputs ~ ., 
                   data = train_data[delay], 
                   hidden = c(12,7),
                   linear.output = FALSE,
                   act.fct = "tanh",
                   learningrate = 0.1
)

plot(model)

# Check the dimensions of test_data
dim(test_data)

# Check the structure of test_data to ensure it matches the input format expected by the model
str(test_data)

test_data <- create_input_data(test_data, delay)

# Select the data frame from test_data
test_data_subset <- test_data[[delay]]

str(test_data_subset[[1]])

length(model$model.list$variables)
str(test_input)

# Remove the last column (outputs) from test_data_subset to get test_input
test_input <- test_data_subset[, -ncol(test_data_subset)]

# Convert test_input into a data frame with one column named "input_1"
test_input <- data.frame(input_1 = test_input)


# Check the structure of test_input
str(test_input)

# Make predictions on the input features
test_predictions <- predict(model, test_input)

# Check the dimensions of the predictions
dim(test_predictions)


# Check summary statistics of test_predictions and test_data$outputs
# summary(test_predictions)
# summary(test_data[[1]]$outputs)

# Calculate performance metrics
mae <- mean(abs(test_predictions - test_data[[delay]]$outputs))
mse <- mean((test_predictions - test_data[[delay]]$outputs)^2)
rmse <- sqrt(mse)

# Calculate MAPE
mape <- mean(abs(test_data[[delay]]$outputs - test_predictions) / test_data[[delay]]$outputs) * 100

# Calculate SMAPE
smape <- mean(2 * abs(test_data[[delay]]$outputs - test_predictions) / (abs(test_data[[delay]]$outputs) + abs(test_predictions))) * 100


# Define a threshold for error tolerance
error_threshold <- 0.05

# Calculate the number of correct predictions based on the error threshold
num_correct_predictions <- sum(abs(test_predictions - test_data[[delay]]$outputs) < error_threshold)

# Calculate the total number of predictions
total_predictions <- length(test_predictions)

# Calculate accuracy as a percentage
accuracy <- (num_correct_predictions / total_predictions) * 100

# Print the performance metrics
cat("Mean Absolute Error (MAE):", mae, "\n")
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("Mean Absolute Percentage Error (MAPE):", rmse, "\n")
cat("Symmetric Mean Absolute Percentage Error (SMAPE):", rmse, "\n")

# Print the accuracy
cat("Accuracy:", accuracy, "%\n")



# Visualize results
plot(test_data[[delay]]$outputs, test_predictions,
     xlab = "Actual", ylab = "Predicted",
     main = "Actual vs. Predicted",
     col = "blue", pch = 20)
abline(0, 1, col = "red")