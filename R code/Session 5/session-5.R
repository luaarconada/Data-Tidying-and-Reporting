
## ----------------------------------------------------------------------------
## Name: Fast modeling using AutoML
## Description: Script for Session 5 of "Data Tidying and Reporting"
## License: © All rights reserved
## Author: Eduardo García-Portugués
## Version: 1.2
## ----------------------------------------------------------------------------

## ----h2o-install, eval = FALSE----------------------------------------------
## install.packages("h2o")

## ----h2o-init, cache = TRUE, warning = FALSE--------------------------------
library(h2o)
h2o.init() # Start H2O cluster
h2o.no_progress() # Turn off progress bars for notebook readability

## ----reg-sim-1, fig.asp = 1/3, out.width = '100%', cache = TRUE-------------
# Simulate data from a linear model
n_reg <- 1e4
p_reg <- 10
set.seed(12313)
x_reg <- matrix(rnorm(p_reg * n_reg), nrow = n_reg, ncol = p_reg)
beta_reg <- seq(-5, 5, l = p_reg)
beta_reg <- sign(beta_reg) * beta_reg^2
y_reg <- x_reg %*% beta_reg + rnorm(n_reg)
data_reg <- data.frame("y" = y_reg, "x" = x_reg)

# Visualize relations of Y with (X1, ..., Xp)
pairs(data_reg, horInd = 1, verInd = 2:(p_reg / 2 + 1), pch = 16, cex = 0.1)
pairs(data_reg, horInd = 1, verInd = (p_reg / 2 + 2):(p_reg + 1), pch = 16, cex = 0.1)

## ----reg-sim-2, cache = TRUE------------------------------------------------
# Send data to local H2O cluster
data_reg <- as.h2o(data_reg)

# Data summary -- perhaps too verbose
h2o.describe(data_reg)

# Data head
h2o.head(data_reg)

# Split into train (80%) and test (20%)
splits_reg <- h2o.splitFrame(data = data_reg, ratios = 0.8, seed = 42)
train_reg <- splits_reg[[1]]
test_reg <- splits_reg[[2]]

# Identify the names of the response and predictors
resp_reg <- "y"
pred_reg <- setdiff(names(train_reg), resp_reg)

# Run AutoML with the default models and no verbosity (for notebook readability!)
aml_reg <- h2o.automl(x = pred_reg, y = resp_reg, training_frame = train_reg, leaderboard_frame = test_reg,
                      seed = 42, max_runtime_secs = 60, max_runtime_secs_per_model = 5, verbosity = NULL)
# Much more settings: ?h2o.automl

## ----reg-sim-3, cache = TRUE------------------------------------------------
# Get the leaderboard of models
lb_reg <- aml_reg@leaderboard # or h2o.get_leaderboard(object = aml_reg)
names(lb_reg)[2] <- "mrd" # Rename mean_residual_deviance to shorten output
print(lb_reg[, -6], n = nrow(lb_reg)) # Exclude final column to fit the table in one page
# As expected GLM (linear model) ranks best

# Extract the best model using the default metric
leader_reg <- aml_reg@leader # or h2o.get_best_model(aml_reg)

# Get the best model using a non-default metric
(leader_reg <- h2o.get_best_model(aml_reg, criterion = "rmse"))

# Get the best model from a specific family
(dl_reg <- h2o.get_best_model(aml_reg, algorithm = "DeepLearning"))
(xg_reg <- h2o.get_best_model(aml_reg, algorithm = "XGBoost"))

# Access the model_id's
aml_reg@leaderboard$model_id

# Extract a specific model through its model_id (the third best model)
(id_reg <- aml_reg@leaderboard$model_id[3, 1])
(third_reg <- h2o.getModel(id_reg))

# Evaluate the performance of the model in new data
h2o.performance(leader_reg, newdata = test_reg)

## ----reg-sim-4, cache = TRUE, results = 'hide'------------------------------
# Save several models
path_reg <- paste0(getwd(), "/models_regression")
h2o.saveModel(object = leader_reg, path = path_reg, filename = "GLM_reg", force = TRUE)
h2o.saveModel(object = dl_reg, path = path_reg, filename = "DL_reg", force = TRUE)
h2o.saveModel(object = xg_reg, path = path_reg, filename = "XG_reg", force = TRUE)
rm(leader_reg, dl_reg, xg_reg)

# Load models
leader_reg <- h2o.loadModel(path = paste0(path_reg, "/GLM_reg"))
dl_reg <- h2o.loadModel(path = paste0(path_reg, "/DL_reg"))
xg_reg <- h2o.loadModel(path = paste0(path_reg, "/XG_reg"))

## ----reg-sim-5, fig.asp = 1 / 3, cache = TRUE, out.width = '100%'-----------
# Use the leader model to make predictions in the test dataset
h2o.predict(object = leader_reg, newdata = test_reg) # or predict(leader_reg, test_reg)

# Compare different model predictions for new data
m_reg <- 1e3
new_reg <- data.frame(y = NA, x = matrix(rnorm(p_reg * m_reg), nrow = m_reg, ncol = p_reg))
pred_leader <- h2o.predict(object = leader_reg, newdata = as.h2o(new_reg))
pred_dl <- h2o.predict(object = dl_reg, newdata = as.h2o(new_reg))
pred_xg <- h2o.predict(object = xg_reg, newdata = as.h2o(new_reg))
par(mfrow = c(1, 3))
hist(as.matrix(pred_leader), main = "leader")
hist(as.matrix(pred_dl), main = "dl")
hist(as.matrix(pred_xg), main = "xg")

## ----reg-sim-6, cache = TRUE------------------------------------------------
# Explain a single H2O model (e.g., best deep learning model from AutoML) using a given dataset
# (test_reg in this case)
ex_dl_reg <- h2o.explain(object = dl_reg, newdata = test_reg)

# The output of h2o.explain() is a long list of plots
ex_dl_reg

# Options are described in ?h2o.explain. For example, one could select only specific variables to explain
# with "columns" or establish the number of features to be analysed with "top_n_features"

# These plots can be done individually via:
# h2o.residual_analysis_plot()
# h2o.varimp_plot()
# h2o.pd_plot()
# h2o.ice_plot()

## ----reg-sim-7, cache = TRUE------------------------------------------------
h2o.shap_summary_plot(xg_reg, test_reg)

## ----reg-sim-8, cache = TRUE------------------------------------------------
# Explain leader model and compare with all AutoML models
ex_reg <- h2o.explain(object = aml_reg, newdata = test_reg)
ex_reg

# Explain a model
# h2o.varimp_heatmap()
# h2o.model_correlation_heatmap()
# h2o.pd_multi_plot()

## ----reg-sim-9, cache = TRUE------------------------------------------------
# Close H2O
h2o.shutdown(prompt = FALSE)

## ----reg-real-1, cache = TRUE-----------------------------------------------
# Read data
df_pwr <- read.csv(file = "powerplant_output.csv")

## ----reg-real-2, message = FALSE, cache = TRUE------------------------------
# Start H2O
h2o.init()
h2o.no_progress()

# Load data into H2O
df_pwr <- as.h2o(df_pwr)

# Summarize the dataset
h2o.describe(df_pwr)

# Set the response
resp_pwr <- "HourlyEnergyOutputMW"

# AutoML without test dataset (cross-validation metrics employed). The default is nfolds = 5. Note
# we do not need to specify x: the predictors are all the variables that are not named resp. We
# exclude "DeepLearning" algorithms as those are more time-consuming.
aml_pwr <- h2o.automl(y = resp_pwr, training_frame = df_pwr, max_runtime_secs = 60, nfolds = 10,
                      seed = 1, exclude_algos = "DeepLearning", verbosity = NULL)

# Leaderboard
lb_pwr <- aml_pwr@leaderboard
names(lb_pwr)[2] <- "mrd" # Rename mean_residual_deviance to shorten output
print(lb_pwr[, -6], n = nrow(lb_pwr)) # Exclude final column to fit the table in one page

# Prediction in the average instances
avg_pwr <- h2o.mean(df_pwr, return_frame = TRUE)
h2o.predict(object = aml_pwr@leader, newdata = avg_pwr)

# Explanation
ex_pwr <- h2o.explain(object = aml_pwr, newdata = df_pwr)
ex_pwr

# Close H2O
h2o.shutdown(prompt = FALSE)

## ----binary-sim-1, cache = TRUE, fig.asp = 1/3, out.width = '100%'----------
# Some test data from a logistic model
n_bin <- 1e3
p_bin <- 5
set.seed(12313)
x_bin <- matrix(rnorm(p_bin * n_bin, sd = 2), nrow = n_bin, ncol = p_bin)
beta_bin <- 1:p_bin - 2
y_bin <- rbinom(n = n_bin, size = 1, prob = 1 / (1 + exp(15 - (x_bin %*% beta_bin))))
y_bin <- as.factor(y_bin)
data_bin <- data.frame("y" = y_bin, "x" = x_bin)

# Distribution of y_bin
table(y_bin)

# Plot classification problem
pairs(data_bin, horInd = 1, verInd = 2:(p_bin + 1), pch = 16, cex = 0.5)

## ----binary-sim-2, message = FALSE, cache = TRUE----------------------------
# Start H2O
h2o.init()
h2o.no_progress()

# Send data to local H2O cluster
data_bin <- as.h2o(data_bin)

# Identify response
resp_bin <- "y"

# Important: response should be a factor
data_bin[, resp_bin] <- as.factor(data_bin[, resp_bin])

# Run AutoML with cross-validation metrics based on 10 cross-validation folds
aml_bin <- h2o.automl(y = resp_bin, training_frame = data_bin, nfolds = 10, seed = 42,
                      include_algos = c("GLM", "DRF", "StackedEnsemble"), verbosity = NULL)

## ----binary-sim-3, cache = TRUE---------------------------------------------
# Leaderboard
lb_bin <- h2o.get_leaderboard(aml_bin)
names(lb_bin)[5] <- "mpce" # Rename mean_per_class_error to shorten output
print(lb_bin[, -6], n = nrow(lb_bin)) # Exclude final column to fit the table in one page

# Get the best model among families
(gl_bin <- h2o.get_best_model(aml_bin, algorithm = "GLM"))
(en_bin <- h2o.get_best_model(aml_bin, algorithm = "StackedEnsemble"))

## ----binary-sim-4, cache = TRUE---------------------------------------------
# AutoML with balanced classes
aml_bin2 <- h2o.automl(y = resp_bin, training_frame = data_bin, nfolds = 10, seed = 42,
                       include_algos = c("GLM", "DRF", "StackedEnsemble"), balance_classes = TRUE,
                       verbosity = NULL)

# Leaderboard
lb_bin2 <- h2o.get_leaderboard(aml_bin2)
names(lb_bin2)[5] <- "mpce" # Rename mean_per_class_error to shorten output
print(lb_bin2[, -6], n = nrow(lb_bin2)) # Exclude final column to fit the table in one page

## ----binary-sim-5, cache = TRUE---------------------------------------------
# Predictions for new data for two models -- the predictions are given in terms of labels and probabilities
m_bin <- 1e3
new_bin <- data.frame("y" = NA, "x" = matrix(rnorm(p_bin * m_bin, sd = 2), nrow = m_bin, ncol = p_bin))
pred_gl_bin <- h2o.predict(object = gl_bin, newdata = as.h2o(new_bin))
pred_en_bin <- h2o.predict(object = en_bin, newdata = as.h2o(new_bin))
h2o.head(pred_gl_bin)
h2o.head(pred_en_bin)

# Computing the label assignments and checking the similarity
labels_gl_bin <- as.matrix(pred_gl_bin$predict)
labels_en_bin <- as.matrix(pred_en_bin$predict)
table(labels_gl_bin, labels_en_bin)

## ----binary-sim-6, cache = TRUE---------------------------------------------
# Explain leader model and compare with all AutoML models
ex_bin <- h2o.explain(object = aml_bin, newdata = data_bin)
ex_bin

## ----binary-sim-7, cache = TRUE---------------------------------------------
h2o.shutdown(prompt = FALSE)

## ----binary-real-1, cache = TRUE--------------------------------------------
# Read data
df_bck <- read.csv(file = "product_backorders.csv")

## ----binary-real-2, message = FALSE, cache = TRUE---------------------------
# Start H2O
h2o.init()
h2o.no_progress()

# Load data into H2O
df_bck <- as.h2o(df_bck)

# Summarize the dataset
h2o.describe(df_bck)

# Set the response and predictors
resp_bck <- "went_on_backorder"
pred_bck <- setdiff(names(df_bck), c(resp_bck, "sku")) # sku is a unique identifier

# Ensure response is a factor
df_bck[, resp_bck] <- as.factor(df_bck[, resp_bck])

# AutoML without test dataset (cross-validation metrics employed and nfold = 5 as default)
aml_bck <- h2o.automl(x = pred_bck, y = resp_bck, training_frame = df_bck,
                      max_models = 10, seed = 1, verbosity = NULL)

# Leaderboard
lb_bck <- aml_bck@leaderboard
names(lb_bck)[5] <- "mpce" # Rename mean_per_class_error to shorten output
print(lb_bck[, -6], n = nrow(lb_bck)) # Exclude final column to fit the table in one page

# Prediction of the first five rows
h2o.predict(object = aml_bck@leader, newdata = df_bck[1:5, ])

# Explanation
ex_bck <- h2o.explain(object = aml_bck, newdata = df_bck)
ex_bck

# Extract the GLM to help interpretation
id_bck <- as.data.frame(aml_bck@leaderboard$model_id)[, 1]
glm_bck <- h2o.getModel(grep("GLM", id_bck, value = TRUE)[1])
# glm_bck # The default printing method gives a lot of clutter

# The block slot @model contains the interesting information on the model -- it is a long list!
str(glm_bck@model, 1)

# Access the GLM coefficients
glm_bck@model$coefficients_table

## ----binary-real-3, cache = TRUE--------------------------------------------
# Extract the StackedEnsemble_AllModels model
se_bck <- h2o.getModel(grep("StackedEnsemble_AllModels", id_bck, value = TRUE)[1])

# Get the metalearner model within the stacked ensemble
metalearner_bck <- se_bck@model$metalearner_model
metalearner_bck@model$coefficients_table

# Variable importance of the metalearner -- gives the base learner contributions to the ensemble
h2o.varimp(metalearner_bck)
h2o.varimp_plot(metalearner_bck)

## ----binary-real-4, cache = TRUE--------------------------------------------
h2o.shutdown(prompt = FALSE)

## ----multi-sim-1, cache = TRUE----------------------------------------------
# Simulate data from a multinomial model
n_mul <- 1e3
set.seed(12313)
th <- runif(n_mul, min = 0, max = 2 * pi)
r <- rgamma(n_mul, shape = 5, rate = 3)
x_mul <- r * cbind(cos(th), sin(th))
beta_mul <- cbind(c(2, 2), c(-2, -2), c(2, -2), c(-2, 2), c(0, 0))
prob_mul <- exp(x_mul %*% beta_mul)
prob_mul <- cbind(prob_mul[, -5] / (1 + rowSums(prob_mul[, -5])), 1 / (1 + rowSums(prob_mul[, -5])))
y_mul <- sapply(1:n_mul, function(i) sample(x = 1:5, size = 1, prob = prob_mul[i, ]))
data_mul <- data.frame("y" = y_mul, "x" = x_mul)

# Distribution of y_mul
table(y_mul)

# Plot classification problem
plot(x_mul, col = y_mul, pch = 16)

## ----multi-sim-2, message = FALSE, cache = TRUE-----------------------------
# Start H2O
h2o.init()
h2o.no_progress()

# Send data to local H2O cluster
data_mul <- as.h2o(data_mul)

# Identify predictors and response
resp_mul <- "y"
pred_mul <- setdiff(names(data_mul), resp_mul)

# Response should be a factor
data_mul[, resp_mul] <- as.factor(data_mul[, resp_mul])

# Split into train (80%) and test (20%)
splits_mul <- h2o.splitFrame(data = data_mul, ratios = 0.8, seed = 42)
train_mul <- splits_mul[[1]]
test_mul <- splits_mul[[2]]

# Run AutoML
aml_mul <- h2o.automl(x = pred_mul, y = resp_mul, training_frame = train_mul, leaderboard_frame = test_mul,
                      include_algos = c("GLM", "XGBoost", "DeepLearning"), max_runtime_secs = 60,
                      max_runtime_secs_per_model = 5, verbosity = NULL)

## ----multi-sim-3, cache = TRUE----------------------------------------------
# Leaderboard
lb_mul <- h2o.get_leaderboard(aml_mul)
print(lb_mul, n = nrow(lb_mul))

## ----multi-sim-4, cache = TRUE----------------------------------------------
# Probability of correct classification by pure chance, taking into
# account there are unweighted classes (tossing coin example adapted)
# P[X = Y] = \sum_{i=1}^N P[X = y_i | Y = y_i] * P[Y = y_i] = \sum{_i=1}^N p_i^2
probs <- table(as.matrix(data_mul$y))
probs <- probs / sum(probs)
sum(probs^2)

# Mean-per-class error by "weighted guessing"
# (1/N) \sum_{i=1}^N (1 - P[X = x_i])
mean(1 - probs)

## ----multi-sim-5, cache = TRUE----------------------------------------------
# Predict using the leader model -- the predictions are given in terms of labels and probabilities
pred_mul <- h2o.predict(object = aml_mul, newdata = test_mul)
h2o.head(pred_mul)

# Checking the accuracy of the label assignments with the real labels
labels_mul <- as.matrix(pred_mul$predict)
table(labels_mul, as.matrix(test_mul$y))

## ----multi-sim-6, cache = TRUE----------------------------------------------
ex_mul <- h2o.explain(object = aml_mul, newdata = test_mul)
ex_mul

## ----multi-sim-7, cache = TRUE----------------------------------------------
h2o.shutdown(prompt = FALSE)

## ----multi-real-1, cache = TRUE---------------------------------------------
# Load data
load("MNIST-tSNE.RData")

# Plot t-SNE scores
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)
plot(MNIST$y_tsne, col = rainbow(10)[MNIST$labels + 1], pch = 16, cex = 0.25)
legend("bottomright", col = rainbow(10), pch = 16, legend = 0:9)

## ----multi-real-2, message = FALSE, cache = TRUE----------------------------
# Start H2O
h2o.init()
h2o.no_progress()

# Load data into H2O
df_mni <- as.h2o(MNIST)

# Ensure labels is a factor
df_mni[, "labels"] <- as.factor(df_mni[, "labels"])

# Split into train (80%) and test (20%)
splits_mni <- h2o.splitFrame(data = df_mni, ratios = 0.8, seed = 42)
train_mni <- splits_mni[[1]]
test_mni <- splits_mni[[2]]

# Set the response
resp_mni <- "labels"
pred_mni <- c("y_tsne.1", "y_tsne.2")

# AutoML
aml_mni <- h2o.automl(x = pred_mni, y = resp_mni, training_frame = train_mni, leaderboard_frame = test_mni,
                      max_runtime_secs = 600, max_runtime_secs_per_model = 60, seed = 42, verbosity = NULL)

# Leaderboard
lb_mni <- aml_mni@leaderboard
print(lb_mni, n = nrow(lb_mni))

## ----multi-real-3, cache = TRUE---------------------------------------------
# Mean-per-class error by "weighted guessing"
probs <- table(as.matrix(df_mni$labels))
probs <- probs / sum(probs)
mean(1 - probs)

# Probability of correct classification by pure chance
sum(probs^2)

## ----multi-real-4, cache = TRUE---------------------------------------------
# Predict using the leader model -- the predictions are given in terms of labels and probabilities
pred_mni <- h2o.predict(object = aml_mni, newdata = test_mni)
h2o.head(pred_mni)

# Checking the accuracy of the label assignments with the real labels
labels_mni <- as.matrix(pred_mni$predict)
table(labels_mni, as.matrix(test_mni$labels))

## ----multi-real-5, cache = TRUE---------------------------------------------
h2o.shutdown(prompt = FALSE)
