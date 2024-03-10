# Required library
library(png)
# Load data
load(file = "qmnist_nist.RData")
# Visualization helper
show_digit <- function(x, col = gray(255:1 / 255), ...) {
  l <- sqrt(length(x))
  image(matrix(as.numeric(x), nrow = l)[, l:1], col = col, ...)
}
## Classifier
# Create average images
avg_train_images <- sapply(0:9, function(d) {
  colMeans(train_nist$px[train_nist$digit == d, ])
})
# Classifier function
classifier <- function(vec_img) {
  which.min(colMeans((avg_train_images - vec_img)ˆ2)) - 1
}
# Visualize average train images
par(mfrow = c(2, 5), mar = rep(0, 4))
for (d in 1:10) {
  show_digit(avg_train_images[, d], axes = FALSE)
}
## Create test images to upload to the app
# Save images from the test dataset using writePNG()
for (i in 0:9) {
  # Matrix with 0-1 entries
  1
  img_vec <- test_nist$px[which(test_nist$digit == i)[1], ] / 255
  img_mat <- matrix(as.numeric(img_vec), nrow = 28, ncol = 28,
                    byrow = TRUE) # Saves it with the right orientation
  # Save image
  writePNG(image = img_mat, target = paste0("test-", i, ".png"))