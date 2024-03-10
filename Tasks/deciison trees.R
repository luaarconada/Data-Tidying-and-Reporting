# Load required libraries
library(rpart)

# Load data
load(file = "qmnist_nist.RData")
rm(test_nist)

# Sample a subset of training data
set.seed(123)  # for reproducibility
train_sample <- sample(nrow(train_nist$px), 1000)  # adjust sample size as needed

# Decision tree model
dt_classifier <- function(test_img, train_sample) {
  # Create a new data frame with only the 'px' and 'digit' columns
  train_data <- train_nist[train_sample, c("px", "digit")]
  
  # Convert 'px' column to a matrix
  train_px <- matrix(unlist(train_data$px), ncol = 28*28, byrow = TRUE)
  
  # Convert test_img to a matrix
  test_px <- matrix(test_img, ncol = 28*28)
  
  # Create the model frame with both train and test data
  model_data <- rbind(train_px, test_px)
  colnames(model_data) <- paste0("V", 1:ncol(model_data))
  model_data <- as.data.frame(model_data)
  model_data$digit <- as.factor(c(train_data$digit, NA))  # Add NA for the test image
  
  # Build the decision tree model
  dt_model <- rpart(digit ~ ., data = model_data, method = "class")
  
  # Predict the digit for the test image
  pred <- predict(dt_model, newdata = model_data[nrow(model_data), -ncol(model_data)], type = "class")
  
  return(pred)
}

# Example usage:
# Assuming 'test_img' contains the pixel values of the test image
# and 'train_sample' is the list of indexes of the train partition
# predicted_digit <- dt_classifier(test_img, train_sample)

library(png)

# Read the image file
img <- readPNG("test-0.png")  # Replace "path/to/image.png" with the path to your image file

# Convert pixel values to a vector
pixel_values <- as.vector(img)

# Display the dimensions of the image and the pixel values
print(dim(img))  # Dimensions of the image
print(pixel_values)  # Pixel values
dim(pixel_values)

# Assuming 'pixel_values' contains the pixel values of the test image
# and 'train_sample' is the list of indexes of the train partition
predicted_digit <- dt_classifier(pixel_values, train_sample)
predicted_digit <- as.numeric(predicted_digit)[1]
predicted_digit
class(predicted_digit)
# Visualize the decision tree
library(rpart.plot)
rpart.plot(dt_model)

