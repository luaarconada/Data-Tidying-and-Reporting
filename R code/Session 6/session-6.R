## ----tidymodels, cache = TRUE, warning = FALSE------------------------------------------------------------
library(tidymodels)

## ----broom-1, cache = TRUE--------------------------------------------------------------------------------
# Load dataset
data(airquality)

# The standard summary for lm()
lm_fit <- lm(Ozone ~ ., data = airquality)
summary(lm_fit)

# tidy() tidies the information of the *components* of the model (that is, the
# predictors) into a clean tibble
(tidy_lm <- tidy(lm_fit))

# Easy and standardized access to the elements (notice how the names given by
# tidy() are not lm()-specific)
tidy_lm$term

# If we want a pure data frame because we do not like tibbles
as.data.frame(tidy_lm)

# glance() tidies the information of the *entire* model into a clean tibble
(glance_lm <- glance(lm_fit))
# We have the standard measures of goodness-of-fit and other statistics
# nobs(lm_fit) reports the data used in the model after removing NAs

# A simple check
BIC(lm_fit) - glance_lm$BIC

# augment() complements the original dataset with model information associated
# with the observations: fitted values, residuals, standard errors, etc.
# New columns begin with a "." to avoid overwriting columns in the original data
(aug_lm <- augment(lm_fit))

# To adequately understand the new columns, we can check ?augment.lm:
# .fitted: fitted or predicted value.
# .lower,.upper: lower/upper bound on interval for fitted values.
# .se.fit: standard errors of fitted values. Used for confidence intervals.
# .resid: residuals, difference between observed and fitted values.
# .hat: diagonal of the hat matrix. NOT predictions.
# .sigma: estimated residual standard deviation when corresponding observation
#         is dropped from model.
# .cooksd: cooks distance.
# .std.resid: standardised residuals (residuals divided by their standard
#             errors).

# By default, confidence intervals and standard errors are not computed. We
# can ask to do so as follows
(aug_lm <- augment(lm_fit, interval = "confidence", se_fit = TRUE))
# If interval = "prediction", the confidence intervals will be for the
# conditional *response*, not for the conditional *expectation*
# See ?predict.lm

# We can also augment a *different* dataset formed by new observations. For
# example, one designed to determine the marginal effects of the variable Temp
# conditionally on the median of the remaining predictors.

# Create a data frame with Temp ranging from min to max, the remaining
# predictors fixed to their medians, and the response excluded
N <- 100
temp_effects <- airquality %>%
  select(-Ozone) %>%
  summarise(across(.cols = everything(), .fns = median, na.rm = TRUE)) %>%
  slice(rep(1:n(), each = N))
temp_effects$Temp <- seq(min(airquality$Temp, na.rm = TRUE),
                         max(airquality$Temp, na.rm = TRUE), l = N)

# Augment temp_effects running predictions and confidence intervals
(aug_newdata <- augment(lm_fit, newdata = temp_effects,
                        interval = "confidence"))

# Plot marginal effects
ggplot(data = aug_newdata, mapping = aes(x = Temp, y = .fitted)) +
  geom_line() +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = 0.3)

## ----broom-2, cache = TRUE--------------------------------------------------------------------------------
## aov (and anova)

tidy(aov(lm_fit))
glance(aov(lm_fit))
# augment() does not make too much sense for aov and anova objects, so
# augment(aov(lm_fit)) gives a warning and falls back to the method for lm
# objects

## glm

# Load dataset and select columns related with 1986 metrics to determine the
# factors that make a player be among the top 10% most paid ones
data(Hitters, package = "ISLR")
Hitters <- Hitters %>%
  select(c("AtBat", "Hits", "HmRun", "Runs", "RBI", "Walks", "Division",
           "PutOuts", "Assists", "Errors", "Salary"))
top10 <- Hitters$Salary > quantile(Hitters$Salary, probs = 0.90, na.rm = TRUE)
Hitters$top10 <- top10

# Standard summary for glm()
glm_fit <- glm(top10 ~ . - Salary, data = Hitters, family = "binomial")
summary(glm_fit)

# tidy() and glance()
(tidy_glm <- tidy(glm_fit))
(glance_glm <- glance(glm_fit))

# augment() works as before. With type.predict we can specify if we want
# predictions on the link function space or in the response (probabilities).
# We cannot ask for confidence intervals as those are unsupported for
# predict.glm()
(aug_glm <- augment(glm_fit, type.predict = "response", se_fit = TRUE))

# Let's visualize the marginal effects of Walks, with confidence intervals,
# conditionally on the median/mode of the remaining predictors
median_mode <- function(x)
  ifelse(is.numeric(x), median(x, na.rm = TRUE),
         names(which.max(table(x))))
N <- 100
walks_effects <- Hitters %>%
  select(-top10) %>%
  summarise(across(.cols = everything(), .fns = median_mode)) %>%
  slice(rep(1:n(), each = N))
walks_effects$Walks <- seq(min(Hitters$Walks, na.rm = TRUE),
                           max(Hitters$Walks, na.rm = TRUE), l = N)
aug_newdata <- augment(glm_fit, newdata = walks_effects,
                       type.predict = "link", se_fit = TRUE)

# Plot marginal effects with manually-computed confidence intervals
z <- qnorm(0.975)
logistic <- function(x) 1 / (1 + exp(-x))
ggplot(data = aug_newdata, mapping = aes(x = Walks, y = logistic(.fitted))) +
  geom_line() +
  geom_ribbon(aes(ymin = logistic(.fitted - z * .se.fit),
                  ymax = logistic(.fitted + z * .se.fit)),
              alpha = 0.3)

## glmnet

# Prepare data
Hitters <- na.omit(Hitters)
x <- model.matrix(top10 ~ 0 + . - Salary, data = Hitters)
y <- Hitters$top10

glmnet_fit <- glmnet::glmnet(x = x, y = y, alpha = 0, family = "binomial")
tidy(glmnet_fit)
glance(glmnet_fit)
# augment(glmnet_fit)
# Unfortunately, there is no augment() method for glmnet::glmnet! Perhaps this
# is due to the peculiar behavior of predict.glmnet()?

# We can also tidy() and glance() the output of glmnet::cv.glmnet
lambda <- 10^seq(-4, 2, l = 100)
cv <- glmnet::cv.glmnet(x = x, y = y, alpha = 0, lambda = lambda,
                        family = "binomial")
(tidy_cv <- tidy(cv))
(glance_cv <- glance(cv))

## regsubsets

regsubsets_fit <- leaps::regsubsets(Ozone ~., data = airquality,
                                    method = "exhaustive")
summary(regsubsets_fit)
tidy(regsubsets_fit)
# No glance() nor augment() methods for leaps::regsubsets

## biglm

biglm_fit <- biglm::biglm(Ozone ~ Solar.R + Wind + Temp + Month + Day,
                          data = airquality)
summary(biglm_fit)
tidy(biglm_fit) # Nice because it is homogeneous to tidy(lm)
glance(biglm_fit)
# No augment() method for biglm::biglm

## ----broom-3, cache = TRUE--------------------------------------------------------------------------------
## MASS::fitdistr

fit <- MASS::fitdistr(x = airquality$Temp, densfun = "lognormal")
tidy(fit)
glance(fit)
# No augment() method for MASS::fitdistr

## kmeans

fit_km <- iris %>%
  select(starts_with("Petal")) %>%
  kmeans(centers = 3)
tidy(fit_km)
glance(fit_km)
(aug_km <- augment(fit_km, data = iris))

# Use the augmented dataset to create a quick plot
ggplot(data = aug_km, mapping = aes(x = Petal.Length, y = Petal.Width)) +
    geom_point(aes(col = .cluster, pch = Species))

## prcomp

pca <- iris %>%
  select(where(is.numeric)) %>%
  prcomp(scale. = TRUE)
tidy(pca) # Scores of the data. 4 PCs.
# No glance() method for prcomp
(aug_pca <- augment(pca, data = iris)) # data to include the original data

# Quick ggplot2 with first two scores
ggplot(data = aug_pca, aes(x = .fittedPC1, y = .fittedPC2)) +
  geom_point(aes(col = Species))

## htest

test <- ks.test(x = airquality$Ozone[airquality$Month %in% 1:6],
                y = airquality$Ozone[airquality$Month %in% 7:12])
tidy(test)
glance(test)
# No augment() method for htest

## pairwise.htest

groups <- factor(airquality$Month, labels = month.abb[5:9])
pairs_test <- pairwise.t.test(x = airquality$Ozone, g = groups,
                              pool.sd = FALSE)
tidy(pairs_test)
# No glance() nor augment() methods for pairwise.htest

## power.t.test

# Compute the power for given difference in means delta
pow <- power.t.test(n = length(na.omit(airquality$Ozone)),
                    sd = sd(airquality$Ozone, na.rm = TRUE),
                    delta = seq(0, 30, l = 10), alternative = "one.sided")
tidy(pow)
# No glance() nor augment() methods for power.htest

# Compute sample size required to achieve a certain power for a certain
# difference in means delta
pow <- power.t.test(n = NULL, sd = sd(airquality$Ozone, na.rm = TRUE),
                    delta = 10, power = 0.90)
tidy(pow)

## ----broom-4, cache = TRUE--------------------------------------------------------------------------------
## optim

f <- function(x) 100 * (x[2] - x[1]^2)^2 + (1 - x[1])^2 # Banana function
(opt <- optim(par = c(-1.2, 1), fn = f))
tidy(opt)
glance(opt)

# No methods for nlm() objects

# Make a ggplot2 of cv.glmnet
ggplot(data = tidy_cv, mapping = aes(x = log(lambda))) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.1, col = "gray") +
  geom_vline(xintercept = log(glance_cv$lambda.min), lty = 2) +
  geom_vline(xintercept = log(glance_cv$lambda.1se), lty = 2) +
  geom_point(aes(y = estimate), col = 2) +
  xlab(expression("Log(" * lambda * ")")) + ylab("Binomial Deviance")

## ----rsample-1, cache = TRUE------------------------------------------------------------------------------
# Split with 0.75 of data for training/modeling
(iris_split <- initial_split(iris, prop = 0.75))

# An mc_split object
class(iris_split)

# Access the training/testing data with training()/testing()
training(iris_split) %>% head() # Training part shuffles rows
testing(iris_split) %>% head() # Testing part preserves the original rows order

# It is possible to enforce stratification in one variable to carry out the
# sampling -- this is useful to preserve a relevant distribution of a certain
# variable
iris_split <- initial_split(iris, prop = 0.50, strata = Species)

# Species is perfectly balanced also in the training sample (as the original
# iris data is)
training(iris_split) %>%
  select(Species) %>%
  table()

# We can also stratify by a continuous variable that is discretized in a certain
# amount of bins (determined with quantile(..., probs = (0:breaks) / breaks))
iris_split <- initial_split(iris, prop = 0.50, strata = Petal.Length,
                            breaks = 4)

# The distribution of Petal.Length is maintained in training/testing
breaks <- quantile(iris$Petal.Length, probs = (0:4) / 4)
hist(iris$Petal.Length, breaks = breaks, probability = TRUE, col = NA)
hist(training(iris_split)$Petal.Length, breaks = breaks, probability = TRUE,
     add = TRUE, border = 2, col = NA)
hist(testing(iris_split)$Petal.Length, breaks = breaks, probability = TRUE,
     add = TRUE, border = 3, col = NA)

# initial_time_split() takes the first prop samples for training (in this case,
# all the testing samples are Species == "virginica")
initial_time_split(iris, prop = 0.8) %>%
  testing() %>%
  head()

## ----rsample-2, cache = TRUE, error = TRUE----------------------------------------------------------------
## bootstraps()

# Construct 10 bootstrap samples of the iris dataset. We can also specify
# strata and breaks arguments to carry out a stratified resampling.
(boot_iris <- bootstraps(iris, times = 10))

# A bootstraps object
class(boot_iris)

# The first bootstrap sample
boot_iris$splits[[1]]

# To retrieve the first bootstrap sample we need to call analysis()
analysis(boot_iris$splits[[1]]) %>% head()

# assessment() will give us the observations that did not enter the
# bootstrap sample due to repetitions
assessment(boot_iris$splits[[1]]) %>% head()

# Let's convince ourselves that assessment() actually contains no data from
# analysis() in a bootstrap sample
cases_ana <- rownames(analysis(boot_iris$splits[[1]]))
cases_ass <- rownames(assessment(boot_iris$splits[[1]]))
all(is.na(match(x = cases_ass, table = cases_ana)))

# A quick application: approximate the sampling distribution of the
# sample mean using 5000 bootstrap resamples and purrr::map_dbl()

# Simulate the original sample. Important! Observe it is a data frame. This is
# crucial: the data passed to bootstraps() has to be a data frame, not a vector!
mu <- 1
sigma <- 1
n <- 200
df <- data.frame("x" = rnorm(n = n, mean = mu, sd = sigma))

# Compute bootstrap samples
boot_df <- bootstraps(df, times = 5000)

# Function to compute the bootstrapped statistic
x_bar_star <- function(split) {
  x_star <- analysis(split)$x # Access bootstrap sample
  mean(x_star) # Bootstrapped statistic
}

# Plot the histogram. Note we use purrr::map_dbl() because we want a vector
# returned, not a list.
hist(map_dbl(boot_df$splits, .f = x_bar_star), probability = TRUE,
     main = "Histogram of bootstrapped statistic vs. true density",
     xlab = "Statistic")
curve(dnorm(x, mean = mu, sd = sigma / sqrt(n)), col = 2, add = TRUE)

# Recall an interesting point: the size increment of the bootstrap replicates
# is smaller than the number of bootstrap samples (5000 in this case).
# This is due to an implementation that realizes that bootstrap replicates are
# the same data from the original sample, but accessed randomly.
lobstr::obj_size(boot_df) / lobstr::obj_size(df)

## vfold_cv() and loo_cv()

# Construct 20 10-fold cross-validation splittings
(vcv_iris <- vfold_cv(iris, v = 10, repeats = 20))

# A bootstraps object
class(vcv_iris)

# The first cross-validation split
vcv_iris$splits[[1]]

# We retrieve the 90% of the shuffled data with analysis()
analysis(vcv_iris$splits[[1]]) %>% head()

# We retrieve the remaining 10% of the shuffled data with assessment()
assessment(vcv_iris$splits[[1]]) %>% head()

# We can check that indeed "data = analysis() + assessment()"
cases_ana <- rownames(analysis(vcv_iris$splits[[1]]))
cases_ass <- rownames(assessment(vcv_iris$splits[[1]]))
all(sort(as.numeric(c(cases_ana, cases_ass))) == 1:nrow(iris))

# loo_cv() is just a wrapper for vfold_cv(..., v = n)
loo_iris <- loo_cv(iris, repeats = 20)
analysis(loo_iris$splits[[1]]) %>% nrow()
assessment(loo_iris$splits[[1]]) # Only one observation for assessment

## permutations()

# Compute 10 samples of iris with "Species" being permuted. No stratification
# is possible since the concept does not make sense for permutations.
(per_iris <- permutations(iris, permute = "Species", times = 10))

# Observe how Species is shuffled
analysis(per_iris$splits[[1]]) %>% head()

# There is no assessment data -- the whole sample is used!
assessment(per_iris$splits[[1]])

# We can permute several variables
permutations(iris, permute = starts_with("Sepal"), times = 10) %>%
  magrittr::use_series("splits") %>%
  magrittr::extract2(1) %>%
  analysis() %>%
  head()

# Comparison
head(iris)

# The use of magrittr::use_series() and magrittr::extract2() above is meant to
# illustrate how to combine accesses by $ and [[ with pipes. The first par is
# equivalent to the following code:
# permutations(iris, permute = starts_with("Sepal"), times = 10)$splits[[1]] %>%
#   analysis() %>%
#   head()

## ----rsample-3, cache = TRUE------------------------------------------------------------------------------
# The last sample is "Apparent", which indicates that is the original sample
# (without shuffling) in the analysis *and* assessment parts.
(boot_iris <- bootstraps(iris, times = 5, apparent = TRUE))
all(analysis(boot_iris$splits[[6]]) == iris)
all(assessment(boot_iris$splits[[6]]) == iris)

# Same for permutations
(per_iris <- permutations(iris, permute = Species, times = 5, apparent = TRUE))
all(analysis(per_iris$splits[[6]]) == iris)

## ----rsample-4, cache = TRUE------------------------------------------------------------------------------
## int_pctl()

# Let's use the previous example on the sample mean
mu <- 1
sigma <- 1
n <- 200
df <- data.frame("x" = rnorm(n = n, mean = mu, sd = sigma))

# The function to compute the bootstrapped statistic needs to return a tidy()
# object, i.e., a tibble() containing a "term" (name) and an "estimate"
# (statistic) at least. We can produce that tibble() if there is no tidy()
# method readily associated to our statistic, as in this case. The ...
# arguments are required if using the function for int_bca().
x_bar_star2 <- function(split, ...) {
  x_star <- analysis(split)$x
  tibble("term" = "x_bar_star", "estimate" = mean(x_star))
}

# Compute the bootstraps object and add a column with the bootstrapped
# statistics
boot_df <- bootstraps(df, times = 5000)
boot_df <- boot_df %>% mutate(stats = map(splits, x_bar_star2))
head(boot_df)

# Call to int_pctl() for a 95% confidence
(ci_pctl <- int_pctl(boot_df, statistics = stats, alpha = 0.05))
c(ci_pctl$.lower, ci_pctl$.upper)

# The percentile confidence interval is the same as
boot_stats <- map_dbl(boot_df$stats,
                      .f = function(split) split$estimate)
quantile(boot_stats, probs = c(0.025, 0.975))

# True confidence interval
mu + c(-1, 1) * qnorm(0.975) * sigma / sqrt(n)

## int_bca()

# int_bca() requires apparent = TRUE in the bootstrapped samples
boot_df <- bootstraps(df, times = 5000, apparent = TRUE) %>%
  mutate(stats = map(splits, x_bar_star2))

# We have to pass in addition the function computing the statistic in .fn,;
# this is the same function we use for the bootstrap samples (and it needs to
# have an argument ...)
(ci_bca <- int_bca(boot_df, stats, .fn = x_bar_star2, alpha = 0.05))

# The lengths of both confidence intervals are very similar
ci_pctl$.upper - ci_pctl$.lower
ci_bca$.upper - ci_bca$.lower

## Exercise

# Load data
load("mysterious.RData")

# Filter x by numeric
x <- df %>% select(where(is.numeric))

# Exclude predictors with missing values
x <- x %>% select(!where(anyNA))

# Do a PCA on x and retain 90% of variability
pca <- prcomp(x, scale. = TRUE)
cutoff <- which(cumsum(pca$sdev^2) / sum(pca$sdev^2) >= 0.90)[1]
scores <- pca$x[, 1:cutoff]

# Put in a data frame
df_mys <- data.frame("y" = df$y, "x" = scores)

# CV splittings
vcv_mys <- vfold_cv(data = df_mys, v = 10, repeats = 500, strata = "y")

# c?

# Function to compute cross-validation accuracy
cv_acc <- function(split) {
  df_ana <- analysis(split)
  df_ass <- assessment(split)
  fit <- MASS::lda(y ~ ., data = df_ana)
  return(mean(predict(fit, newdata = df_ass)$class == df_ass$y))
}

# Average classification accuracy on the 500 splittings
lda_acc <- map_dbl(vcv_mys$splits, .f = cv_acc)
mean(lda_acc, na.rm = TRUE)

# Weighted guessing baseline
probs <- table(y) / sum(table(y))
sum(probs^2)

## ----parsnip-1, cache = TRUE, eval = FALSE----------------------------------------------------------------
## install.packages(c("xgboost", "C50", "rpart", "mgcv", "glmnet", "LiblineaR",
##                    "earth", "nnet", "kknn", "ranger", "randomForest",
##                    "flexsurv", "survival", "kernlab", "liquidSVM", "keras",
##                    "mixOmics", "plsmod"))


## ----parsnip-2, cache = TRUE------------------------------------------------------------------------------
# Load dataset -- an iteration on the Boston dataset
data(ames, package = "modeldata")
head(ames)

# Create an initial split stratifying by the response
set.seed(42)
data_split <- initial_split(ames, strata = "Sale_Price", prop = 0.75)
ames_train <- training(data_split)
ames_test <- testing(data_split)

# Create a specification for a random forest model
rf_spec <- rand_forest(mtry = 10, trees = 2000) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("regression")

# Fit the model
rf_fit <- rf_spec %>%
  fit(log10(Sale_Price) ~., data = ames_train)

# Evaluate the fit
test_results <- ames_test %>%
  select(Sale_Price) %>%
  mutate(Sale_Price = log10(Sale_Price)) %>%
  bind_cols(predict(rf_fit, new_data = ames_test))
head(test_results)

# The trend seems to be properly captured
test_results %>%
  gather(model, prediction, -Sale_Price) %>%
  ggplot(aes(x = prediction, y = Sale_Price)) +
  geom_abline(col = "green") +
  geom_point(alpha = 0.5)


## ----parsnip-3, cache = TRUE------------------------------------------------------------------------------
# Load dataset
data(Chicago, package = "modeldata")

# The dataset collects data on the usage of Chicago metro (ridership) by
# measuring the number of people (in thousands) who enter in several stations,
# over time (date)
head(Chicago)

# Filter variables -- total of ridership + usage of two stations
Chicago <- Chicago %>% select(ridership, Clark_Lake, Quincy_Wells)

# Split in train and test: we predict 28 days from the past
n <- nrow(Chicago)
ahead <- 28
chg_train <- Chicago %>% slice(1:(n - ahead))
chg_test <- Chicago %>% slice((n - ahead - 1):n)

# Create a specification for a nearest neighbors model
knn_spec <- nearest_neighbor(neighbors = 7, weight_func = "gaussian") %>%
  set_mode("regression") %>%
  set_engine("kknn")
knn_spec

# Fit the model
knn_fit <- knn_spec %>%
  fit(ridership ~ ., data = chg_train)
knn_fit

# Evaluate the fit
test_results <- chg_test %>%
  select(ridership) %>%
  bind_cols(predict(knn_fit, new_data = chg_test))
head(test_results)

# The trend seems to be properly captured
test_results %>%
  gather(model, prediction, -ridership) %>%
  ggplot(aes(x = prediction, y = ridership)) +
  geom_abline(col = "green") +
  geom_point(alpha = 0.5)


## ----parsnip-4, cache = TRUE------------------------------------------------------------------------------
# Load dataset -- contains measurements from different types of penguins
data(penguins, package = "modeldata")

# Select islands and penguins' bill measurements
penguins <- penguins %>% select(island, starts_with("bill_"))

# Initial split
set.seed(42)
data_split <- initial_split(penguins, prop = 0.75)
penguins_train <- training(data_split)
penguins_test <- testing(data_split)

# We can define the model with specific parameters:
mnr_spec <- multinom_reg(penalty = 0.1) %>%
  set_engine("nnet")
mnr_spec

# Now we create the model fit object:
mnr_fit <- mnr_spec %>%
  fit(island ~ ., data = penguins_train)
mnr_fit

# Prediction in hard class predictions and probabilities
test_results <- bind_cols(
  select(penguins_test, "island"),
  predict(mnr_fit, penguins_test),
  predict(mnr_fit, penguins_test, type = "prob")
)

# Classification accuracy
table(test_results$island, test_results$.pred_class)
mean(test_results$island == test_results$.pred_class, na.rm = TRUE)

# Baseline
probs <- table(penguins$island) / sum(table(penguins$island))
sum(probs^2)






## ----discrim-1, cache = TRUE, eval = FALSE----------------------------------------------------------------
## install.packages(c("earth", "mda", "sda", "sparsediscrim", "klaR",
##                    "naivebayes"))


## ----discrim-2, cache = TRUE------------------------------------------------------------------------------
# Not loaded with tidymodels
library(discrim)

# Fit a Naive Bayes model (which is actually a kernel discriminant
# analysis done by combining univariate kernel density estimators)
nb_mod <- naive_Bayes() %>%
  set_engine("naivebayes") %>%
  fit(Species ~ Sepal.Length + Sepal.Width, data = iris)
nb_mod

# Grid to plot decision regions
iris_grid <- expand.grid(Sepal.Length = seq(3, 9, length = 100),
                         Sepal.Width = seq(1, 5, length = 100))
iris_grid$classes_nb <- predict(nb_mod, iris_grid)$.pred_class

# Draw sample and decision regions
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(aes(col = Species, pch = Species), alpha = 0.75) +
  geom_raster(data = iris_grid,
              aes(x = Sepal.Length, y = Sepal.Width, fill = classes_nb),
              alpha = 0.25) +
  theme_bw() +
  theme(legend.position = "top") +
  coord_equal()


## ----discrim-3, cache = TRUE------------------------------------------------------------------------------
# Fit a discriminant analysis model based on features created by multivariate
# adaptive regression splines
fda_mod <- discrim_flexible(num_terms = 3) %>%
  set_engine("earth") %>%
  fit(Species ~ Sepal.Length + Sepal.Width, data = iris)
fda_mod

# Grid to plot decision regions
iris_grid <- expand.grid(Sepal.Length = seq(3, 9, length = 100),
                         Sepal.Width = seq(1, 5, length = 100))
iris_grid$classes_fda <- predict(fda_mod, iris_grid)$.pred_class

# Draw sample and decision regions
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(aes(col = Species, pch = Species), alpha = 0.75) +
  geom_raster(data = iris_grid,
              aes(x = Sepal.Length, y = Sepal.Width, fill = classes_fda),
              alpha = 0.25) +
  theme_bw() +
  theme(legend.position = "top") +
  coord_equal()






## ----corrr-1, cache = TRUE--------------------------------------------------------------------------------
# Not loaded with tidymodels
library(corrr)

# The correlate() function computes correlations in a tibble format
(cor_air <- correlate(airquality))

# A cor_df object
class(cor_air)

# Access columns/rows of the correlation matrix
cor_air$Ozone
cor_air$Solar.R

# Variables in correlation matrix
cor_air$term

# The method argument allows to have "pearson" (default), "kendall",
# and "spearman" correlations
correlate(airquality, method = "spearman")

# Decoration of correlation matrices for copy-pasting with fashion()
fashion(cor_air)
fashion(cor_air, decimals = 3, leading_zeros = TRUE, na_print = "1")

# rearrange() allows to sort columns of the correlation matrix to aid its
# visualization. It employs the methods listed in ?seriation::seriate (e.g.,
# "PCA" or "HC" for Hierarchical Clustering)
rearrange(cor_air)


## ----corrr-2, cache = TRUE--------------------------------------------------------------------------------
# Plot a correlation matrix using ggplot2
rplot(cor_air, print_cor = TRUE)

# Filter for the lower triangular part
cor_air %>%
  shave() %>%
  rplot(print_cor = TRUE)

# A nice network plot. The position of the nodes is determined by
# multidimensional scaling using cmdscale().
network_plot(cor_air, min_cor = 0.3)
# Notice the absence of paths between nodes with correlation below 0.3

# With another dataset, using the dice() function to filter for the
# desired variables once the correlation matrix has been computed
mtcars %>%
  correlate() %>%
  dice(mpg, wt, am) %>%
  network_plot()


## ----corrr-3, cache = TRUE, warning = FALSE---------------------------------------------------------------
# Suppose we wanted to build a matrix whose entries are some kind of
# non-standard correlations. The first step would be to define a function
# that provides it. For example, we can compute a distance correlation using
# energy::dcor() (this is a "correlation" in [0, 1]).
compute_dcor <- function(x, y) energy::dcor(x, y)

# colpair_map() allows applying a two-sample-to-scalar function and return a
# tidied cor_df object
(dcors <- airquality %>%
    na.omit() %>%
    colpair_map(.f = compute_dcor) %>%
    arrange())

# rpart() is not adequate since dcors is in [0, 1]. But we can reproduce the
# plot rpart is computing using stretch(), which prepares the cor_df object
# to be used in ggplot2
(s_dcors <- stretch(dcors))

# With retract() we can go back to a cor_df (from a tbl_df or data.frame)
retract(s_dcors)

# Custom correlation plot
orders <- dcors$term
s_dcors$x <- factor(s_dcors$x, levels = orders)
s_dcors$y <- factor(s_dcors$y, levels = rev(orders))
s_dcors$size <- s_dcors$r
s_dcors$label = fashion(s_dcors$r)
ggplot(s_dcors, aes(x = x, y = y, color = r, size = size,
                    alpha = size, label = label)) +
  geom_point(shape = 16) +
  geom_text(color = "black", size = 3, show.legend = FALSE) +
  scale_colour_gradientn(limits = c(0, 1),
                         colors = c("white", "indianred2")) +
  labs(x = "", y = "", colour = NULL) +
  guides(size = "none", alpha = "none") +
  theme_classic()


## ----tidypredict-1, cache = TRUE--------------------------------------------------------------------------
# Not loaded with tidymodels
library(tidypredict)

# Load dataset
data(trees)

# Fit a linear model using parsnip
lm_model <- linear_reg() %>%
  set_engine("lm") %>%
   fit(Volume ~ ., data = trees)
tidy(lm_model)
glance(lm_model)

# Simulate a SQL database connection
sql <- dbplyr::simulate_mssql()

# Model translation to SQL, ready to be copied to database
tidypredict_sql(lm_model, con = sql)

# Model formula, directly applicable with mutate() -- this gives a direct
# handle on the prediction formula that is being applied
(lm_formula <- tidypredict_fit(lm_model))

# Create manually a new column by applying the model formula
trees %>%
  mutate("fit" = !!lm_formula) %>%
  head()

# Automatic addition of predictions
trees %>%
  tidypredict_to_column(lm_model) %>%
  head()

# We can also compute confidence intervals for the prediction in linear models
# (and *only* for linear models, as this is a task that requires strong
# distribution assumptions). For that, we use tidypredict_interval() and,
# importantly, parse_model().
tidypredict_sql_interval(parse_model(lm_model), con = sql)

# Interval formula
tidypredict_interval(parse_model(lm_model))

# Importantly, both the formulae above give the semi-length of the confidence
# intervals. That is, one has to subtract and add the fitted value to get
# the lower and upper limits of the confidence interval, respectively.

# We can skip using parsnip and directly lm() (which is indeed simpler, as we
# do not need to apply parse_model(), yet less modular)
lm_model <- lm(Volume ~ ., data = trees)
tidypredict_fit(lm_model)
tidypredict_interval(lm_model)
tidypredict_sql(lm_model, con = sql)
tidypredict_sql_interval(lm_model, con = sql)


## ----tidypredict-2, cache = TRUE--------------------------------------------------------------------------
# Load dataset
backorders <- read.csv(file = "product_backorders.csv")
backorders$went_on_backorder <- factor(backorders$went_on_backorder,
                                       levels = c("Yes", "No"))

# Fit a logistic model using parsnip
log_model <- logistic_reg() %>%
  set_engine("glm") %>%
   fit(went_on_backorder ~ national_inv + lead_time + in_transit_qty +
         forecast_3_month + forecast_6_month,
       data = backorders)
tidy(log_model)
glance(log_model)

# Simulate a SQL database connection
sql <- dbplyr::simulate_mssql()

# Model translation to SQL, ready to be copied to database
tidypredict_sql(log_model, con = sql)

# Model formula, directly applicable with mutate()
(log_formula <- tidypredict_fit(log_model))
backorders %>%
  mutate("fit" = !!log_formula) %>%
  select(went_on_backorder, fit) %>%
  head()

# Automatic addition of predictions
backorders %>%
  tidypredict_to_column(log_model) %>%
  select(went_on_backorder, fit) %>%
  head()

# Skip using parsnip and directly call glm()
log_model <- glm(went_on_backorder ~ national_inv + lead_time + in_transit_qty +
                   forecast_3_month + forecast_6_month, family = "binomial",
                 data = backorders)
tidypredict_fit(log_model)
tidypredict_sql(log_model, con = sql)


## ----tidypredict-3, cache = TRUE--------------------------------------------------------------------------
# Fit an XGBoost model using parsnip
xgb_model <- boost_tree(mode = "classification") %>%
   set_engine("xgboost") %>%
   fit(went_on_backorder ~ national_inv + lead_time + in_transit_qty +
         forecast_3_month + forecast_6_month,
       data = backorders)
# No tidy() nor glance() methods

# Simulate a SQL database connection
sql <- dbplyr::simulate_mssql()

# Very long outputs
# tidypredict_fit(xgb_model)
# tidypredict_sql(xgb_model, con = sql)

# Automatic addition of predictions
backorders %>%
  tidypredict_to_column(model = xgb_model) %>%
  select(went_on_backorder, fit) %>%
  head()

