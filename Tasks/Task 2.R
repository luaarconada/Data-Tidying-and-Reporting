# Required libraries
library(shiny)
library(png)
library(class)
library(e1071)
library(nnet)
library(rpart)


# Load data
load(file = "qmnist_nist.RData")
rm(test_nist)  # Remove test_nist object to save memory and to prove we do not use it


# Visualization helper function to display digits
show_digit <- function(x, col = gray(255:1 / 255), ...) {
  l <- sqrt(length(x))  # Calculate the length of the vector x
  image(matrix(as.numeric(x), nrow = l)[, l:1], col = col, ...)  # Display the digit image
}


# Sample a subset of training data
set.seed(123)  # Set seed for reproducibility
train_sample <- sample(nrow(train_nist$px), 1000)  # Sample 1000 rows from the training data



## Classifiers

# Averager Classifier with Euclidean distance
# Create average images for each digit
avg_train_images <- sapply(0:9, function(d) {
  colMeans(train_nist$px[train_nist$digit == d, ])  # Calculate the average image for each digit
})

# Classifier function for Averager
classifier <- function(vec_img) {
  # Calculate Euclidean distances between the test image and average images
  distances <- apply(avg_train_images, 2, function(avg_img) sqrt(sum((avg_img - vec_img)^2)))
  # Predict the digit with the minimum distance
  digit <- which.min(distances) - 1
  return(digit)
}


# K-Nearest Neighbors (KNN) Classifier
knn_classifier <- function(test_img, k_value) {
  # Calculate Euclidean distances between test image and training images
  dists <- apply(train_nist$px[train_sample, ], 1, function(train_img) sqrt(sum((train_img - test_img)^2)))
  # Find k nearest neighbors
  closest_digit <- train_nist$digit[train_sample][order(dists)[1:k_value]]
  # Predict the digit with the highest frequency among the closest neighbors
  return(as.numeric(names(sort(table(closest_digit), decreasing = TRUE)[1])))
}


# Decision tree classifier with tuned parameters
dt_classifier <- function(test_img, train_sample, cp_value) {
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
  
  # Ensure digit labels are properly constrained to the range of 0 to 9
  model_data$digit <- as.factor(ifelse(c(train_data$digit, NA) == 10, 0, c(train_data$digit, NA)))
  
  # Replace NA values with a placeholder value (e.g., -999)
  model_data[is.na(model_data)] <- -999
  
  # Build the decision tree model with tuned parameters
  dt_model <- rpart(digit ~ ., data = model_data, method = "class", control = rpart.control(cp = cp_value))
  
  # Predict the digit for the test image
  pred <- predict(dt_model, newdata = model_data[nrow(model_data), -ncol(model_data)], type = "class")
  
  # Replace placeholder value with NA in predictions
  pred[pred == -999] <- NA
  
  return(pred)
}


# Define confidence level function for KNN classifier
knn_confidence <- function(test_img, k_value) {
  # Calculate Euclidean distances between test image and training images
  dists <- apply(train_nist$px[train_sample, ], 1, function(train_img) sqrt(sum((train_img - test_img)^2)))
  # Find k closest digits
  closest_digits <- train_nist$digit[train_sample][order(dists)[1:k_value]]
  # Get the digit with the highest frequency
  majority_digit <- as.numeric(names(sort(table(closest_digits), decreasing = TRUE)[1]))
  # Calculate confidence level
  confidence <- sum(closest_digits == majority_digit) / k_value
  return(confidence)
}


# Define UI for the Shiny app
ui <- fluidPage(
  
  # Header Section
  fluidRow(
    column(width = 10, offset = 1,
           h3("Prediction of digits", align = "center", style = "font-weight: bold; font-size: 22px;"),
           # Description of the app's functionality
           p("This app allows you to upload an image containing a handwritten digit.", style = "font-size: 16px;"),
           p("It accepts the following file types: PNG and JPEG. If using different ones, an error message will appear when trying to classify. The same will happen if the size of the files is bigger than 10MB.", style = "font-size: 16px;"),
           p("You can choose between different classifiers to predict the digit in the uploaded image and the prediction will appear at the bottom of the screen. If you don't choose a classifier previous to trying to classify, an error will raise. You can choose between the following classifiers:", style = "font-size: 16px;"),
           p("   a. The 'Averager' classifier calculates the average image for each digit in the training set and assigns the test image to the digit whose average image it most closely resembles.", style = "font-size: 16px;"),
           p("   b. The 'KNN' classifier uses the K-Nearest Neighbors algorithm to find the closest training images to the test image and predicts the majority digit among the closest neighbors. It also computes the confidence level of the prediction in this model", style = "font-size: 16px;"),
           p("   c. The 'Decision Tree' classifier utilizes a decision tree model to make predictions based on the pixel values of the test image. It takes a couple of seconds to make the prediction.", style = "font-size: 16px;"),
    )
  ),
  
  # Main Content Section
  fluidRow(
    column(width = 4, offset = 1,
           # File Upload Section
           fileInput("file1", "Choose an image file",
                     accept = c("image/png", "image/jpeg")),
           # Classifier Selection Section
           selectInput("classifier", "Select Classifier:",
                       choices = c("Select Classifier", "Averager", "KNN", "Decision Tree"), selected = "Select Classifier"), # Set selected to "Select Classifier"
           # Classify Button
           actionButton("classify", "Classify"),
           br(),
           # Classifier Explanation Section
           h4("How does this classifier work?", style = "font-weight: bold; font-size: 18px;"),
           uiOutput("classifier_explanation"), # Dynamic UI element for classifier explanation
           br(),
           # Confidence Explanation Section
           h4("How does it compute the confidence level?", style = "font-weight: bold; font-size: 18px;"),
           uiOutput("confidence_explanation"),
           br()
    ),
    column(width = 4,
           # Input Image Preview Section
           plotOutput("input_image", hover = "hover")  # Previsualization of input image
    )
  ),
  
  # Sidebar Section
  fluidRow(
    column(width = 4, offset = 1,
           # Parameter Tuning for KNN
           conditionalPanel(
             condition = "input.classifier == 'KNN'",
             sliderInput("k_value", "Number of Neighbors (K):", min = 1, max = 20, value = 5)
           ),
           # Parameter Tuning for Decision Tree
           conditionalPanel(
             condition = "input.classifier == 'Decision Tree'",
             sliderInput("cp_value", "Complexity Parameter (CP):", min = 0, max = 0.1, value = 0.001, step = 0.001)
           )
    ),
    # Result Section
    column(width = 6,
           hr(),
           h5("Classified Digit:", style = "font-size: 16px;"), # Header for predicted digit output
           verbatimTextOutput("result"), # Display predicted digit
           hr(),
           h5("Confidence:", style = "font-size: 16px;"), # Header for confidence level output
           verbatimTextOutput("confidence") # Display confidence intervals
    )
  )
)


# Define server logic
server <- function(input, output, session) {
  # Reactive value for train_sample
  train_sample <- reactiveVal()
  
  # Reactive expression to monitor changes in the uploaded file
  observe({
    req(input$file1) # Ensure that file input is provided
    invalidateLater(100)  # Delay to allow for file upload processing
  })
  
  # Display uploaded image
  output$input_image <- renderPlot({
    req(input$file1) # Ensure that file input is provided
    img <- readPNG(input$file1$datapath) # Read the uploaded image
    par(mar = c(1, 1, 1, 1)) # Set plot margins
    show_digit(c(255 * t(img)), main = "Preview of the input digit") # Display the uploaded image
    hover <- input$hover
    if (!is.null(hover)) {
      x <- floor(hover$x)
      y <- floor(hover$y)
      text(x, y, paste("x:", x, ", y:", y), adj = c(0, 0)) # Display coordinates on hover
    }
  }, height = 400)
  
  # Render classifier explanation based on selected classifier
  output$classifier_explanation <- renderUI({
    req(input$classifier) # Ensure that classifier input is provided
    explanation <- switch(input$classifier,
                          "Averager" = "This classifier calculates the average image for each digit in the training set and assigns the test image to the digit whose average image it most closely resembles.",
                          "KNN" = "This classifier uses the K-Nearest Neighbors algorithm to find the closest training images to the test image and predicts the majority digit among the closest neighbors.",
                          "Decision Tree" = "This classifier utilizes a decision tree model to make predictions based on the pixel values of the test image. It takes a couple of seconds to make the prediction.",
                          "Select Classifier" = "Please select a classifier from the dropdown menu.")
    tagList(
      p(explanation, style = "font-size: 16px;") # Display classifier explanation
    )
  })
  
  # Render confidence explanation based on selected classifier
  output$confidence_explanation <- renderUI({
    req(input$classifier) # Ensure that classifier input is provided
    explanation <- switch(input$classifier,
                          "Averager" = "Not applicable for Averager",
                          "KNN" = "This classifier computes confidence by considering the proportion of the 5 nearest neighbors that agree with the predicted digit.",
                          "Decision Tree" = "Not applicable for Decision Tree",
                          "Select Classifier" = "Please select a classifier from the dropdown menu.")
    tagList(
      p(explanation, style = "font-size: 16px;") # Display confidence explanation
    )
  })
  
  # Classify uploaded image on button click
  observeEvent(input$classify, {
    if (input$classifier == "Select Classifier") { # Check if the default option is selected
      showNotification("Please select a classifier.", duration = 5, type = "warning") # Show warning message
      return()
    }
    
    if (is.null(input$file1$datapath)) { # Check if file is uploaded
      showNotification("No file selected. Please choose an image file.", duration = 5, type = "warning") # Show warning message
      return()
    }
    
    if (file.info(input$file1$datapath)$size > 10e6) { # Check file size
      showNotification("File size limit exceeded. Please choose an image file smaller than 10MB.", duration = 5, type = "warning") # Show warning message
      return()
    }
    
    tryCatch({
      img <- readPNG(input$file1$datapath) # Read the uploaded image
      test_img <- c(255 * t(img)) # Prepare image for classification
      
      if (input$classifier == 'Decision Tree') {
        digit <- dt_classifier(test_img, train_sample(), input$cp_value) # Classify using decision tree
        digit <- as.numeric(digit)[1]  # Convert to numeric
        output$result <- renderText({  # Render predicted digit
          paste("Predicted Digit:", digit)
        })
      } else if (input$classifier == 'KNN') {
        digit <- knn_classifier(test_img, input$k_value) # Classify using KNN
        output$result <- renderText({ # Render predicted digit
          paste("Predicted Digit:", digit)
        })
      } else {
        digit <- classifier(test_img) # Classify using averager classifier
        output$result <- renderText({ # Render predicted digit
          paste("Predicted Digit:", digit)
        })
      }
      
      # Confidence level
      confidence <- switch(input$classifier,
                           "Averager" = NA,
                           "KNN" = knn_confidence(test_img, input$k_value),
                           "Decision Tree" = NA)
      output$confidence = renderText({
        paste("Confidence Level:", confidence) # Render confidence level
      })
    }, error = function(e) {
      showNotification("Error processing the image. Please try again with a valid image file.", duration = 5, type = "error") # Show error message
    })
  })
  
  # Reset output when a new classifier is selected or when the image is changed
  observeEvent(input$file1, {
    output$result <- renderText({ NULL }) # Reset predicted digit output
    output$confidence <- renderText({ NULL }) # Reset confidence level output
  })
  
  # Reset output when a new classifier is selected
  observeEvent(input$classifier, {
    output$result <- renderText({ NULL }) # Reset predicted digit output
    output$confidence <- renderText({ NULL }) # Reset confidence level output
  })
  
  # Reset output when slider values change
  observeEvent(input$k_value, {
    output$result <- renderText({ NULL }) # Reset predicted digit output
    output$confidence <- renderText({ NULL }) # Reset confidence level output
  })
  
  observeEvent(input$cp_value, {
    output$result <- renderText({ NULL }) # Reset predicted digit output
    output$confidence <- renderText({ NULL }) # Reset confidence level output
  })
  
  # Assign train_sample
  observe({
    train_sample(sample(nrow(train_nist$px), 1000))  # Adjust sample size as needed
  })
}


# Run the application
shinyApp(ui = ui, server = server)
