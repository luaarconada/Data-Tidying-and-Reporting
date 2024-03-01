## ----tidymodels, warning = FALSE, cache = FALSE----------------------------------------------------------------------
library(tidymodels)


## ----broom-1, cache = TRUE-------------------------------------------------------------------------------------------
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

# We can also augment a *different* dataset formed by new observations. For
# example, one designed to determine the marginal effects of the variable Temp
# conditionally on the median of the remaining predictors.

# Create a data frame with Temp ranging from min to max, the remaining
# predictors fixed to their medians, and the response excluded
N <- 100
temp_effects <- airquality |>
  select(-Ozone) |>
  summarise(across(.cols = everything(), .fns = median, na.rm = TRUE)) |>
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


## ----broom-2, cache = TRUE-------------------------------------------------------------------------------------------
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
Hitters <- Hitters |>
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
walks_effects <- Hitters |>
  select(-top10) |>
  summarise(across(.cols = everything(), .fns = median_mode)) |>
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


## ----broom-3, cache = TRUE-------------------------------------------------------------------------------------------
## MASS::fitdistr

fit <- MASS::fitdistr(x = airquality$Temp, densfun = "lognormal")
tidy(fit)
glance(fit)
# No augment() method for MASS::fitdistr

## kmeans

fit_km <- iris |>
  select(starts_with("Petal")) |>
  kmeans(centers = 3)
tidy(fit_km)
glance(fit_km)
(aug_km <- augment(fit_km, data = iris))

# Use the augmented dataset to create a quick plot
ggplot(data = aug_km, mapping = aes(x = Petal.Length, y = Petal.Width)) +
    geom_point(aes(col = .cluster, pch = Species))

## prcomp

pca <- iris |>
  select(where(is.numeric)) |>
  prcomp(scale. = TRUE)
tidy(pca)
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


## ----broom-4, cache = TRUE-------------------------------------------------------------------------------------------
## optim

f <- function(x) 100 * (x[2] - x[1]^2)^2 + (1 - x[1])^2 # Banana function
(opt <- optim(par = c(-1.2, 1), fn = f))
tidy(opt)
glance(opt)

# No methods for nlm() objects






## ----rsample-1, cache = TRUE-----------------------------------------------------------------------------------------
# Split with 0.75 of data for training/modeling
(iris_split <- initial_split(iris, prop = 0.75))

# An mc_split object
class(iris_split)

# Access the training/testing data with training()/testing()
training(iris_split) |> head() # Training part shuffles rows
testing(iris_split) |> head() # Testing part preserves the original rows order

# It is possible to enforce stratification in one variable to carry out the
# sampling -- this is useful to preserve a relevant distribution of a certain
# variable
iris_split <- initial_split(iris, prop = 0.50, strata = Species)

# Species is perfectly balanced also in the training sample (as the original
# iris data is)
training(iris_split) |>
  select(Species) |>
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
initial_time_split(iris, prop = 0.8) |>
  testing() |>
  head()


## ----rsample-2, cache = TRUE, error = TRUE---------------------------------------------------------------------------
## bootstraps()

# Construct 10 bootstrap samples of the iris dataset. We can also specify
# strata and breaks arguments to carry out a stratified resampling.
(boot_iris <- bootstraps(iris, times = 10))

# A bootstraps object
class(boot_iris)

# The first bootstrap sample
boot_iris$splits[[1]]

# To retrieve the first bootstrap sample we need to call analysis()
analysis(boot_iris$splits[[1]]) |> head()

# assessment() will give us the observations that did not enter the
# bootstrap sample due to repetitions
assessment(boot_iris$splits[[1]]) |> head()

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
analysis(vcv_iris$splits[[1]]) |> head()

# We retrieve the remaining 10% of the shuffled data with assessment()
assessment(vcv_iris$splits[[1]]) |> head()

# We can check that indeed "data = analysis() + assessment()"
cases_ana <- rownames(analysis(vcv_iris$splits[[1]]))
cases_ass <- rownames(assessment(vcv_iris$splits[[1]]))
all(sort(as.numeric(c(cases_ana, cases_ass))) == 1:nrow(iris))

# loo_cv() is just a wrapper for vfold_cv(..., v = n)
loo_iris <- loo_cv(iris, repeats = 20)
analysis(loo_iris$splits[[1]]) |> nrow()
assessment(loo_iris$splits[[1]]) # Only one observation for assessment

## permutations()

# Compute 10 samples of iris with "Species" being permuted. No stratification
# is possible since the concept does not make sense for permutations.
(per_iris <- permutations(iris, permute = "Species", times = 10))

# Observe how Species is shuffled
analysis(per_iris$splits[[1]]) |> head()

# There is no assessment data -- the whole sample is used!
assessment(per_iris$splits[[1]])

# We can permute several variables
permutations(iris, permute = starts_with("Sepal"), times = 10) |>
  magrittr::use_series("splits") |>
  magrittr::extract2(1) |>
  analysis() |>
  head()

# Comparison
head(iris)

# The use of magrittr::use_series() and magrittr::extract2() above is meant to
# illustrate how to combine accesses by $ and [[ with pipes. The first par is
# equivalent to the following code:
# permutations(iris, permute = starts_with("Sepal"), times = 10)$splits[[1]] |>
#   analysis() |>
#   head()


## ----rsample-3, cache = TRUE-----------------------------------------------------------------------------------------
# The last sample is "Apparent", which indicates that is the original sample
# (without shuffling) in the analysis *and* assessment parts.
(boot_iris <- bootstraps(iris, times = 5, apparent = TRUE))
all(analysis(boot_iris$splits[[6]]) == iris)
all(assessment(boot_iris$splits[[6]]) == iris)

# Same for permutations
(per_iris <- permutations(iris, permute = Species, times = 5, apparent = TRUE))
all(analysis(per_iris$splits[[6]]) == iris)


## ----rsample-4, cache = TRUE-----------------------------------------------------------------------------------------
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
boot_df <- boot_df |> mutate(stats = map(splits, x_bar_star2))
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
boot_df <- bootstraps(df, times = 5000, apparent = TRUE) |>
  mutate(stats = map(splits, x_bar_star2))

# We have to pass in addition the function computing the statistic in .fn,;
# this is the same function we use for the bootstrap samples (and it needs to
# have an argument ...)
(ci_bca <- int_bca(boot_df, stats, .fn = x_bar_star2, alpha = 0.05))

# The lengths of both confidence intervals are very similar
ci_pctl$.upper - ci_pctl$.lower
ci_bca$.upper - ci_bca$.lower






## ----parsnip-1, cache = TRUE, eval = FALSE---------------------------------------------------------------------------
## install.packages(c("xgboost", "C50", "rpart", "mgcv", "glmnet", "LiblineaR",
##                    "earth", "nnet", "kknn", "ranger", "randomForest",
##                    "flexsurv", "survival", "kernlab", "liquidSVM", "keras",
##                    "mixOmics", "plsmod"))


## ----parsnip-2, cache = TRUE-----------------------------------------------------------------------------------------
# Load dataset -- an iteration on the Boston dataset
data(ames, package = "modeldata")
head(ames)

# Create an initial split stratifying by the response
set.seed(42)
data_split <- initial_split(ames, strata = "Sale_Price", prop = 0.75)
ames_train <- training(data_split)
ames_test <- testing(data_split)

# Create a specification for a random forest model
rf_spec <- rand_forest(mtry = 10, trees = 2000) |>
  set_engine("ranger", importance = "impurity") |>
  set_mode("regression")

# Fit the model
rf_fit <- rf_spec |>
  fit(log10(Sale_Price) ~., data = ames_train)

# Evaluate the fit
test_results <- ames_test |>
  select(Sale_Price) |>
  mutate(Sale_Price = log10(Sale_Price)) |>
  bind_cols(predict(rf_fit, new_data = ames_test))
head(test_results)

# The trend seems to be properly captured
test_results |>
  gather(model, prediction, -Sale_Price) |>
  ggplot(aes(x = prediction, y = Sale_Price)) +
  geom_abline(col = "green") +
  geom_point(alpha = 0.5)


## ----parsnip-3, cache = TRUE-----------------------------------------------------------------------------------------
# Load dataset
data(Chicago, package = "modeldata")

# The dataset collects data on the usage of Chicago metro (ridership) by
# measuring the number of people (in thousands) who enter in several stations,
# over time (date)
head(Chicago)

# Filter variables -- total of ridership + usage of two stations
Chicago <- Chicago |> select(ridership, Clark_Lake, Quincy_Wells)

# Split in train and test: we predict 28 days from the past
n <- nrow(Chicago)
ahead <- 28
chg_train <- Chicago |> slice(1:(n - ahead))
chg_test <- Chicago |> slice((n - ahead - 1):n)

# Create a specification for a nearest neighbors model
knn_spec <- nearest_neighbor(neighbors = 7, weight_func = "gaussian") |>
  set_mode("regression") |>
  set_engine("kknn")
knn_spec

# Fit the model
knn_fit <- knn_spec |>
  fit(ridership ~ ., data = chg_train)
knn_fit

# Evaluate the fit
test_results <- chg_test |>
  select(ridership) |>
  bind_cols(predict(knn_fit, new_data = chg_test))
head(test_results)

# The trend seems to be properly captured
test_results |>
  gather(model, prediction, -ridership) |>
  ggplot(aes(x = prediction, y = ridership)) +
  geom_abline(col = "green") +
  geom_point(alpha = 0.5)


## ----parsnip-4, cache = TRUE-----------------------------------------------------------------------------------------
# Load dataset -- contains measurements from different types of penguins
data(penguins, package = "modeldata")

# Select islands and penguins' bill measurements
penguins <- penguins |> select(island, starts_with("bill_"))

# Initial split
set.seed(42)
data_split <- initial_split(penguins, prop = 0.75)
penguins_train <- training(data_split)
penguins_test <- testing(data_split)

# We can define the model with specific parameters:
mnr_spec <- multinom_reg(penalty = 0.1) |>
  set_engine("nnet")
mnr_spec

# Now we create the model fit object:
mnr_fit <- mnr_spec |>
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


## ----parsnip-5, cache = TRUE-----------------------------------------------------------------------------------------
# Random forest specification for regression problem
rf_spec |> translate()

# k-Nearest neighbor specification for regression problem
knn_spec |> translate()

# Multinomial regression specification for classification problem
mnr_spec |> translate()






## ----recipes-1, cache = TRUE-----------------------------------------------------------------------------------------
# Load dataset -- contains predictors informing on credit payment Status
data(credit_data, package = "modeldata")

# Split in training and testing
set.seed(42)
train_test_split <- initial_split(as_tibble(credit_data))
credit_train <- training(train_test_split)
credit_test <- testing(train_test_split)

# View data
head(credit_train)

# Create the recipe object with recipe(). This is a *specification* of a recipe.
# It means it does not carry out computations, it just states what is going to
# be the response (Status) in the recipe to be applied, what will be the
# predictors (all the variables except Status), and what is the data to be used
# (credit_train)
rec_obj <- recipe(Status ~ ., data = credit_train)
rec_obj

# We add to the recipe rec_obj an *specification* for how to carry out
# imputation of missing values: using a k-NN regression/classifier on all the
# predictors with missing data
imputed <- rec_obj |>
  step_impute_knn(all_predictors())
imputed
# Notice how "Operations" have appeared in the recipe

# We keep growing the recipe by encoding the factors as dummy variables
dummys <- imputed |>
  step_dummy(all_nominal_predictors())
dummys
# Notice how a new row was added to "Operations" respecting the order
# in which the operations were invoked

# Now we add a specification to standardize the numerical predictors -- first
# centering by their means, then scaling by their standard deviations
standardized <- dummys |>
  step_center(all_numeric_predictors()) |>
  step_scale(all_numeric_predictors())
standardized

# We could collapse all the steps at once with pipes
my_rec <- recipe(Status ~ ., data = credit_train) |>
  step_impute_knn(all_predictors()) |>
  step_dummy(all_nominal_predictors()) |>
  step_center(all_numeric_predictors()) |>
  step_scale(all_numeric_predictors())

# Now is the time to *train* the recipe: we learn the parameters in the recipe
# (train k-NN, estimate means and standard deviations)
trained_rec <- prep(my_rec, training = credit_train)
trained_rec
# In "Operations" we see the trained steps and to which variables they have
# been applied

# Now we can finally *apply* the trained recipe: either to the training data or
# to the *new data* present in credit_test -- this will be very useful to
# *preprocess* any data prior to applying the model for prediction
prep_credit_train <- bake(trained_rec, new_data = credit_train)
prep_credit_test <- bake(trained_rec, new_data = credit_test)

# Compare the original dataset and the preprocessed one
credit_train
prep_credit_train # First all the numeric, then all the factors and dummys
# So at the sight of this outcome, it would have been better to move
# step_dummy(all_nominal_predictors()) as the last step!


## ----recipes-2, cache = TRUE-----------------------------------------------------------------------------------------
# Split in training and testing
set.seed(42)
train_test_split <- initial_split(iris)
iris_train <- training(train_test_split)
iris_test <- testing(train_test_split)

rec_iris <- recipe(Species ~ ., data = iris_train) |>
  step_center(all_predictors()) |>
  step_scale(all_predictors()) |>
  prep()

# Standard deviations of transformed training data are 1, as expected
bake(rec_iris, new_data = iris_train) |>
  select(where(is.numeric)) |>
  map_dbl(sd)

# Standard deviations of transformed testing data are not 1 because we
# standardize by the standard deviations of other data
bake(rec_iris, new_data = iris_test) |>
  select(where(is.numeric)) |>
  map_dbl(sd)

# Standard deviations of testing data
iris_test |>
  select(where(is.numeric)) |>
  map_dbl(sd)


## ----recipes-3, cache = TRUE-----------------------------------------------------------------------------------------
grep("step_*", ls("package:recipes"), value = TRUE)


## ----recipes-4, cache = TRUE-----------------------------------------------------------------------------------------
# Load dataset -- contains measurements from different types of penguins
data(penguins, package = "modeldata")

## Imputation

# step_impute_bag() imputes via bagged trees
# step_impute_knn() imputes via k-nearest neighbors
# step_impute_linear() imputes numeric variables via a linear model
# step_impute_mean() and step_impute_median() impute numeric data using the mean/median
# step_impute_mode() imputes nominal data using the most common value

# An example of imputation by median and mode in the penguins dataset
penguins |>
  recipe() |> # No roles declared
  step_impute_median(all_numeric()) |>
  step_impute_mode(all_factor()) |>
  prep() |>
  bake(new_data = penguins)

# Probably a much better job imputing with k-nn
recipe(island ~ ., data = penguins) |>
  step_impute_knn(all_predictors()) |>
  prep() |>
  bake(new_data = penguins)

## Individual transformations

# step_log()/step_sqrt()/step_invert() do log/sqrt/inverse transformations
# step_poly() creates new terms using orthogonal polynomials
# step_BoxCox() and step_YeoJohnson() do transformations for inducing normality
# step_bs() creates new terms with B-splines
# step_mutate() creates a new term based on dplyr::mutate()

# Let's see an example of data normalization using step_YeoJohnson(). First,
# the original data
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram() +
  theme_bw()

# Transformed data
penguins |>
  recipe() |>
  step_naomit(everything(), skip = FALSE) |> # Do not skip the step with bake()
  step_YeoJohnson("body_mass_g") |>
  prep() |>
  bake(new_data = penguins) |>
  ggplot(aes(x = body_mass_g)) +
  geom_histogram() +
  theme_bw()

## Discretization

# step_discretize() discretizes numerical variables into quantiles
# step_cut() cuts a numeric variable into a factor using custom breaks

# step_discretize() is useful to create quick color scales
penguins |>
  recipe() |>
  step_naomit(everything(), skip = FALSE) |>
  step_discretize("body_mass_g", num_breaks = 4) |>
  prep() |>
  bake(new_data = penguins) |>
  ggplot(aes(x = bill_length_mm, y = flipper_length_mm, color = body_mass_g)) +
  geom_point() +
  theme_bw()

## Dummy variables and encodings

# step_regex() detects a regular expression
# step_relevel() relevels factors to a desired level
# step_other() collapses some categorical levels
# step_num2factor() converts numerical variables to factors
# step_bin2factor() creates a factor from a dummy variable
# step_dummy() creates dummy variables

# A simple (but useful) example of step_dummy(). Now the design matrix could be
# used by methods that do not handle factors (e.g., glmnet)
penguins |>
  recipe() |>
  step_dummy(all_factor()) |>
  prep() |>
  bake(new_data = penguins)

## Interactions

# step_interact() creates interaction variables

# The argument "terms" receives a formula to create the interactions
recipe(flipper_length_mm ~ ., data = penguins) |>
  step_interact(terms = ~ bill_depth_mm:bill_length_mm) |>
  prep() |>
  bake(new_data = penguins)

# Create group interactions easily
recipe(flipper_length_mm ~ ., data = penguins) |>
  step_dummy(sex, species, island) %>%
  step_interact(terms = ~ body_mass_g:starts_with("bill_")) |>
  prep() |>
  bake(new_data = penguins)

## Normalization

# step_center() centers variables by their means
# step_scale() divides variables by their standard deviations
# step_normalize() does step_center() + step_scale()
# step_range() normalizes numeric data to be within a pre-defined range

# An example of step_range()
iris |>
  recipe() |>
  step_range(starts_with("Petal"), min = 0, max = 1) |>
  prep() |>
  bake(new_data = iris) |>
  select(starts_with("Petal")) |>
  summary()

## Multivariate transformations

# step_classdist() distances to class centroids
# step_pca() performs Principal Component Analysis (PCA)
# step_pls() performs Partial Least Squares (PLS)
# step_kpca() performs Kernel PCA (KPCA)
# step_spatialsign() transforms the data to have unit norm

# When applying step_pca() it is important to standardize the variables before
# if we want to avoid scaling distortions!
recipe(species ~ ., data = na.omit(penguins)) |>
  step_normalize(all_numeric_predictors()) |>
  step_pca(all_numeric_predictors(), threshold = 0.95) |>
  prep() |>
  bake(new_data = na.omit(penguins)) |>
  ggplot(aes(x = PC1, y = PC2, shape = species, color = sex)) +
  geom_point() +
  theme_bw()

## Filters

# step_corr() removes variables with high correlation
# step_filter_missing() removes variables with missing values
# step_lincomb() removes variables linearly dependent
# step_zv() removes variables with zero variance
# step_nzv() removes variables very sparse and unbalanced
# step_select() allows selecting variables using dplyr syntax

# Let's create a dataset with problems
set.seed(42)
x1 <- rnorm(100)
x2 <- rexp(100)
prob_data <- tibble(
  x1 = x1,
  x2 = x2,
  x3 = rnorm(100, sd = 0.01),
  x4 = rep(0, 100),
  x5 = pmax(2, rnorm(100)),
  x6 = -0.8 * x1 + rnorm(100, sd = 0.01),
  x7 = 0.8 * x1 + rnorm(100),
  x8 = x1 + x2,
  x9 = c(rnorm(50), rep(NA, 50))
)
prob_data

prob_data_rec <- prob_data |>
  recipe() |>
  step_zv(everything()) |>
  step_nzv(everything()) |>
  step_filter_missing(everything(), threshold = 0.1) |>
  step_corr(everything(), threshold = 0.9, method = "spearman") |>
  step_lincomb(everything()) |>
  prep()
prob_data_rec

# Cleaned data
bake(prob_data_rec, new_data = prob_data)

## Row operations

# step_naomit() removes observations with missing values
# step_sample() samples rows using dplyr
# step_shuffle() shuffle variables
# step_slice() filters rows by position using dplyr
# step_filter() filters rows using dplyr

# Shuffling only one column
recipe(Species ~ ., iris) |>
  step_shuffle(Species) |>
  prep() |>
  bake(new_data = iris)

## Others

# step_rename() rename variables using dplyr
# step_intercept() adds intercept column
# step_profile() creates a profiling version of the data (one variable is moved,
# the others are fixed)

# Create a recipe to apply knn regressor later
knn_penguins_rec <- recipe(flipper_length_mm ~ ., data = penguins) |>
  step_impute_knn(all_predictors()) |>
  step_normalize(all_numeric_predictors()) |>
  step_dummy(all_factor_predictors())

# Apply recipe
knn_penguins_prep <- knn_penguins_rec |>
  prep() |>
  bake(new_data = penguins)
knn_penguins_prep

# Create profiled dataset on body_mass reusing recipe
knn_penguins_prof <- knn_penguins_rec |>
  step_profile(-body_mass_g, profile = "body_mass_g", pct = 0.5, index = 1,
               grid = list(pctl = TRUE, len = 200)) |>
  prep() |>
  bake(new_data = penguins)
knn_penguins_prof
# A grid of 200 values is created on the quantiles of body_mas_g. Notice the
# syntax of step_profile(): we have to indicate first the variables to be fixed
# and then the variable to be profiled. index = 1 means that all the continuous
# variables are fixed to their 50% quantiles and the factors are fixed to the
# fitst level (the reference level)

# Fit knn using parsnip
fit_knn_penguins <- nearest_neighbor(neighbors = 10,
                                     weight_func = "gaussian") |>
  set_engine("kknn") |>
  set_mode("regression") |>
   fit(flipper_length_mm ~ ., data = knn_penguins_prep)

# Plot marginal effects of body_mass_g
augment(fit_knn_penguins, new_data = knn_penguins_prof) |>
  ggplot(aes(x = body_mass_g, y = .pred)) +
  geom_line()


## ----recipes-5, error = TRUE, cache = TRUE---------------------------------------------------------------------------
# check_class() checks the variable class
# check_cols() checks if all columns are present
# check_missing() checks for missing values
# check_range() checks range consistency

# Passes
recipe(1 ~ ., data = na.omit(penguins)) |>
  check_missing(everything()) |>
  prep()

# Does not pass
recipe(1 ~ ., data = penguins) |>
  check_missing(everything()) |>
  prep()






## ----yardstick-1, cache = TRUE---------------------------------------------------------------------------------------
# Dataset with a simple two class example
data(two_class_example, package = "modeldata")
two_class_example <- as_tibble(two_class_example)
two_class_example

# Confusion matrix
cm_bin <- conf_mat(two_class_example, truth = truth, estimate = predicted)
cm_bin

# The summary is especially useful
summary(cm_bin)

# Dataset with a multiclass example
data(hpc_cv, package = "modeldata")
hpc_cv <- as_tibble(hpc_cv)
hpc_cv

# Confusion matrix
cm_mul <- conf_mat(hpc_cv, truth = obs, estimate = pred)
cm_mul

# Summary
summary(cm_mul) #%>% slice(1) %>% select(.estimate)

## ----yardstick-2, cache = TRUE, warning = FALSE----------------------------------------------------------------------
# ROC curve
autoplot(roc_curve(two_class_example, truth = truth, Class1))

# Multiclass one-vs-all approach -- one curve per level
hpc_cv |>
  filter(Resample == "Fold01") |>
  roc_curve(obs, VF:L) |>
  autoplot()

# Same as above, but will all of the resamples
hpc_cv |>
  group_by(Resample) |>
  roc_curve(obs, VF:L) |>
  autoplot()


## ----dials-1, cache = TRUE-------------------------------------------------------------------------------------------
# Parameter for lasso
mixture()

# Penalty parameter, in log-scale
penalty()

# grid_regular() creates all possible combinations for some levels
(gr_reg <- grid_regular(mixture(), penalty(), levels = 3))
grid_regular(mixture(), penalty(), levels = c(4, 3)) # Change levels

# grid_random() gives random choices from the ranges of mixture() and penalty()
(gr_rnd <- grid_random(mixture(), penalty(), size = 9))

# grid_max_entropy() is a clever way of choosing parameters in a way that
# explore relevant regions of the parameter space. It is random!
(gr_ent <- grid_max_entropy(mixture(), penalty(), size = 9))

# grid_latin_hypercube() gives random choices from the ranges of mixture() and
# penalty() in a "sudoku"
(gr_lat <- grid_latin_hypercube(mixture(), penalty(), size = 9))

# Plot them together
p <- function(...) {
  ggplot(data = ..., aes(x = mixture, y = penalty)) +
    scale_y_log10() +
    geom_point() +
    theme_bw()
}
ggpubr::ggarrange(p(gr_reg), p(gr_rnd), p(gr_ent), p(gr_lat),
                  nrow = 2, ncol = 2)


## ----dials-2, cache = TRUE-------------------------------------------------------------------------------------------
# Cost complexity parameter for trees, with default parameters
cost_complexity()

# Get and set ranges
cost_complexity() %>% range_get()
cost_complexity() %>% range_set(c(-5, 1))

# Sequences parameters within the range, in the original and transformed space
cost_complexity() %>% value_seq(n = 4)
cost_complexity() %>% value_seq(n = 4, original = FALSE)

# Sample parameters
cost_complexity() %>% value_sample(n = 4)

# Other numeric parameter: kernel smoothness (bandwidth)
smoothness(range = c(0.5, 1.5), trans = NULL)

# A discrete parameter: weight specification
weight_func(values = values_weight_func)

# Redefine values
weight_func() %>% value_set(c("rectangular", "triangular"))
weight_func() %>% value_sample(3)

# The sequence is returned in the order of the levels
weight_func() %>% value_seq(3)


## ----tune-1, cache = TRUE--------------------------------------------------------------------------------------------
# Load dataset -- an iteration on the Boston dataset
data(ames, package = "modeldata")

# Split datasets
set.seed(4595)
data_split <- ames |>
  mutate(Sale_Price = log10(Sale_Price)) |>
  initial_split(strata = Sale_Price)
ames_train <- training(data_split)
ames_test <- testing(data_split)


## ----tune-2, cache = TRUE--------------------------------------------------------------------------------------------
ames_train |>
  select(Sale_Price, Longitude, Latitude) |>
  pivot_longer(cols = c(Longitude, Latitude),
               names_to = "predictor", values_to = "value") |>
  ggplot(aes(x = value, Sale_Price)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se = FALSE) +
  facet_wrap(~ predictor, scales = "free_x")


## ----tune-3, cache = TRUE--------------------------------------------------------------------------------------------
ames_rec <- recipe(Sale_Price ~ Gr_Liv_Area + Longitude + Latitude,
                   data = ames_train) |>
  step_log(Gr_Liv_Area, base = 10) |>
  step_ns(Longitude, deg_free = tune("long_df")) |>
  step_ns(Latitude, deg_free = tune("lat_df"))


## ----tune-4, cache = TRUE--------------------------------------------------------------------------------------------
parameters(ames_rec)


## ----tune-5, cache = TRUE--------------------------------------------------------------------------------------------
ames_param <- ames_rec |>
  parameters() |>
  update(
    long_df = spline_degree(),
    lat_df = spline_degree()
  )
ames_param

## ----tune-6, cache = TRUE--------------------------------------------------------------------------------------------
# Parameter grid
spline_grid <- grid_regular(ames_param, levels = 3)
spline_grid

# Linear model specification -- the link with splines is not yet made
lm_mod <- linear_reg() |>
  set_engine("lm")

# 10-fold cross-validation of the training dataset
set.seed(987)
cv_splits <- vfold_cv(ames_train, v = 10, strata = Sale_Price)


## ----tune-7, cache = TRUE--------------------------------------------------------------------------------------------
# Needs a parsnip specfication, recipe, resamples, and grid
ames_tuned <- tune_grid(object = lm_mod, preprocessor = ames_rec,
                        resamples = cv_splits, grid = spline_grid,
                        control = control_grid(verbose = TRUE))
ames_tuned

# The .metrics column has all of the holdout performance estimates
length(ames_tuned$.metrics)
ames_tuned$.metrics[[1]]

# collect_metrics() gets the average metric value for each parameter combination
estimates <- collect_metrics(ames_tuned)
estimates
# Notice that in .config we can see that we have 9 preprocessors (with tuning
# parameters) and 1 model

# The best RMSE values
rmse_vals <- estimates |>
  filter(.metric == "rmse") |>
  arrange(mean)
rmse_vals

# Plot of RMSEs
autoplot(ames_tuned, metric = "rmse")


## ----tune-8, cache = TRUE--------------------------------------------------------------------------------------------
# parsnip specification
knn_mod <- nearest_neighbor(neighbors = tune(), weight_func = tune()) |>
  set_engine("kknn") |>
  set_mode("regression")

# We bind together in a workflow the model and recipe specification for
# a better optimization later
knn_wflow <- workflow() |>
  add_model(knn_mod) |>
  add_recipe(ames_rec)


## ----tune-9, cache = TRUE--------------------------------------------------------------------------------------------
# Extract parameters from the workflow to update them
knn_param <- knn_wflow |>
  parameters() |>
  update(
    long_df = spline_degree(c(2, 18)),
    lat_df = spline_degree(c(2, 18)),
    neighbors = neighbors(c(3, 50)),
    weight_func = weight_func(values = c("rectangular", "inv",
                                         "gaussian", "triangular"))
  )


## ----tune-10, cache = TRUE-------------------------------------------------------------------------------------------
set.seed(987)
ctrl <- control_bayes(verbose = TRUE) # To show the trace
knn_search <- tune_bayes(object = knn_wflow, resamples = cv_splits, initial = 5,
                         iter = 10, param_info = knn_param, control = ctrl)

# Evolution of the RMSE over iterations
autoplot(knn_search, type = "performance", metric = "rmse")

# The first row is the optimal choice
collect_metrics(knn_search) |>
  filter(.metric == "rmse") |>
  arrange(mean)






## ---- workflows-1, cache = TRUE--------------------------------------------------------------------------------------
# Load data on job attrition
data(attrition, package = "modeldata")

# Model specification
model <- logistic_reg() |>
  set_engine("glm")
model

# Preprocessing recipe
recipe <- recipe(Attrition ~ ., data = attrition) |>
  step_dummy(all_nominal(), -Attrition) |>
  step_corr(all_predictors(), threshold = 0.8)
recipe

# Workflow
wf_recipe <- workflow() |>
  add_recipe(recipe) |>
  add_model(model)
wf_recipe

# Fitting
wf_fit <- wf_recipe |>
  fit(attrition)
wf_fit

# Prediction (uses trained recipe + fitted model)
predict(wf_fit, new_data = head(attrition))

# Augmentation
augment(wf_fit, new_data = head(attrition))

## ----discrim-1, cache = TRUE, eval = FALSE---------------------------------------------------------------------------
## install.packages(c("earth", "mda", "sda", "sparsediscrim", "klaR",
##                    "naivebayes"))


## ----discrim-2, cache = TRUE-----------------------------------------------------------------------------------------
# Not loaded with tidymodels
library(discrim)

# Fit a Naive Bayes model (which is actually a kernel discriminant
# analysis done by combining univariate kernel density estimators)
nb_mod <- naive_Bayes() |>
  set_engine("naivebayes") |>
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


## ----discrim-3, cache = TRUE-----------------------------------------------------------------------------------------
# Fit a discriminant analysis model based on features created by multivariate
# adaptive regression splines
fda_mod <- discrim_flexible(num_terms = 3) |>
  set_engine("earth") |>
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






## ----embed-1, cache = TRUE, eval = FALSE-----------------------------------------------------------------------------
## install.packages(c("rpart", "xgboost", "rstanarm", "lme4"))


## ----embed-2, cache = TRUE-------------------------------------------------------------------------------------------
# Not loaded with tidymodels
library(embed)

# Split iris dataset
iris_split <- initial_split(iris, prop = 0.75, strata = Species)
iris_train <- training(iris_split)
iris_test <- testing(iris_split)

# Unsupervised
set.seed(42)
uns_rec_prep <- recipe(Species ~ ., data = iris_train) |>
  step_normalize(all_predictors()) |>
  step_umap(all_predictors(), num_comp = 2) |>
  prep()
uns_rec_prep

bake(uns_rec_prep, new_data = iris_test, Species, starts_with("umap")) |>
  ggplot(aes(x = UMAP1, y = UMAP2, col = Species)) +
  geom_point(alpha = .5) +
  theme_bw() +
  theme(legend.position = "top") +
  coord_equal()

# Supervised
set.seed(42)
sup_rec_prep <- recipe(Species ~ ., data = iris_train) |>
  step_normalize(all_predictors()) |>
  step_umap(all_predictors(), outcome = vars(Species), num_comp = 2) |>
  prep()
sup_rec_prep

bake(sup_rec_prep, new_data = iris_test, Species, starts_with("umap")) |>
  ggplot(aes(x = UMAP1, y = UMAP2, col = Species)) +
  geom_point(alpha = .5) +
  theme_bw() +
  theme(legend.position = "top")


## ----embed-ex-1a, cache = TRUE---------------------------------------------------------------------------------------
# Load data
load(file = "qmnist_nist.RData")

# Caution! $px is a list, it will create problems afterwards. We have to
# fix this. digit and writer are already factors, so we do not have to
# transform them.
train_nist_tib <- train_nist |>
  select(digit, writer) |>
  cbind("px" = as.matrix(train_nist$px)) |>
  as_tibble()
test_nist_tib <- test_nist |>
  select(digit, writer) |>
  cbind("px" = as.matrix(test_nist$px)) |>
  as_tibble()




## ----embed-ex-2a, eval = FALSE, cache = TRUE-------------------------------------------------------------------------
## # This can take a while
## install.packages("keras")
## library(keras)
## install_keras()
## fashion_mnist <- keras::dataset_fashion_mnist()




## ----agua-1, message = TRUE, cache = FALSE---------------------------------------------------------------------------
# Load dataset -- contains measurements from different types of penguins
data(concrete, package = "modeldata")

# Not loaded with tidymodels
library(agua)

# Start h2o cluster
h2o_start()

# Make an initial splitting
set.seed(987)
concrete_split <- initial_split(concrete, strata = compressive_strength)
concrete_train <- training(concrete_split)
concrete_test <- testing(concrete_split)


## ----agua-2, message = TRUE, cache = TRUE----------------------------------------------------------------------------
# Parsnip specification
auto_spec <-
  auto_ml() |>
  set_engine("h2o", max_runtime_secs = 120, seed = 1) |>
  set_mode("regression")

# Preprocessing recipe
normalized_rec <-
  recipe(compressive_strength ~ ., data = concrete_train) |>
  step_normalize(all_predictors())

# Create the workflow
auto_wflow <-
  workflow() |>
  add_model(auto_spec) |>
  add_recipe(normalized_rec)

# The fit takes place now
auto_fit <- fit(auto_wflow, data = concrete_train)
extract_fit_parsnip(auto_fit)

# Fitted models
nrow(as.data.frame(auto_fit$fit$fit$fit@leaderboard))


## ----agua-3, cache = TRUE--------------------------------------------------------------------------------------------
# Predict with the best model
predict(auto_fit, new_data = concrete_test)


## ----agua-4, cache = TRUE--------------------------------------------------------------------------------------------
# Rank the models according to one metric
rank_results(auto_fit) |>
  filter(.metric == "rmse") |>
  arrange(rank)

# Extract metrics
collect_metrics(auto_fit, summarize = FALSE)

# Create a tibble with entries being the predicted tibbles (so a dataset)
tidy(auto_fit) |>
  mutate(
    .predictions = map(.model, predict, new_data = head(concrete_test))
  )

# Show the contributions to the stacked ensembles
auto_fit |>
  extract_fit_parsnip() |>
  member_weights()
auto_fit |>
  extract_fit_parsnip() |>
  member_weights() |>
  unnest(importance) |>
  filter(type == "scaled_importance") |>
  ggplot() +
  geom_boxplot(aes(value, algorithm)) +
  scale_x_sqrt() +
  labs(y = NULL, x = "scaled importance",
       title = "Member importance in stacked ensembles") +
  theme_bw()


## ----agua-8, cache = TRUE--------------------------------------------------------------------------------------------
autoplot(auto_fit, type = "rank", metric = c("rmse")) +
  theme(legend.position = "none") +
  theme_bw()


## ----agua-9, message = TRUE, cache = TRUE----------------------------------------------------------------------------
# AutoML workflow allowing for refitting
auto_spec_refit <- auto_ml() |>
  set_engine("h2o", max_runtime_secs = 60, save_data = TRUE,
             keep_cross_validation_predictions = TRUE) |>
  set_mode("regression")
auto_wflow_refit <- workflow() |>
  add_model(auto_spec_refit) |>
  add_recipe(normalized_rec)

# First fit
first_auto <- fit(auto_wflow_refit, data = concrete_train)

# Fit for another 60 seconds
second_auto <- refit(first_auto, max_runtime_secs = 60)






## ----corrr-1, cache = TRUE-------------------------------------------------------------------------------------------
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


## ----corrr-2, cache = TRUE-------------------------------------------------------------------------------------------
# Plot a correlation matrix using ggplot2
rplot(cor_air, print_cor = TRUE)

# Filter for the lower triangular part
cor_air |>
  shave() |>
  rplot(print_cor = TRUE)

# A nice network plot. The position of the nodes is determined by
# multidimensional scaling using cmdscale().
network_plot(cor_air, min_cor = 0.3)
# Notice the absence of paths between nodes with correlation below 0.3

# With another dataset, using the dice() function to filter for the
# desired variables once the correlation matrix has been computed
mtcars |>
  correlate() |>
  dice(mpg, wt, am) |>
  network_plot()


## ----corrr-3, cache = TRUE, warning = FALSE--------------------------------------------------------------------------
# Suppose we wanted to build a matrix whose entries are some kind of
# non-standard correlations. The first step would be to define a function
# that provides it. For example, we can compute a distance correlation using
# energy::dcor() (this is a "correlation" in [0, 1]).
compute_dcor <- function(x, y) energy::dcor(x, y)

# colpair_map() allows applying a two-sample-to-scalar function and return a
# tidied cor_df object
(dcors <- airquality |>
    na.omit() |>
    colpair_map(.f = compute_dcor) |>
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


## ----tidypredict-1, cache = TRUE-------------------------------------------------------------------------------------
# Not loaded with tidymodels
library(tidypredict)

# Load dataset
data(trees)

# Fit a linear model using parsnip
lm_model <- linear_reg() |>
  set_engine("lm") |>
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
trees |>
  mutate("fit" = !!lm_formula) |>
  head()

# Automatic addition of predictions
trees |>
  tidypredict_to_column(lm_model) |>
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


## ----tidypredict-2, cache = TRUE-------------------------------------------------------------------------------------
# Load dataset
backorders <- read.csv(file = "product_backorders.csv")
backorders$went_on_backorder <- factor(backorders$went_on_backorder,
                                       levels = c("Yes", "No"))

# Fit a logistic model using parsnip
log_model <- logistic_reg() |>
  set_engine("glm") |>
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
backorders |>
  mutate("fit" = !!log_formula) |>
  select(went_on_backorder, fit) |>
  head()

# Automatic addition of predictions
backorders |>
  tidypredict_to_column(log_model) |>
  select(went_on_backorder, fit) |>
  head()

# Skip using parsnip and directly call glm()
log_model <- glm(went_on_backorder ~ national_inv + lead_time + in_transit_qty +
                   forecast_3_month + forecast_6_month, family = "binomial",
                 data = backorders)
tidypredict_fit(log_model)
tidypredict_sql(log_model, con = sql)


## ----tidypredict-3, cache = TRUE-------------------------------------------------------------------------------------
# Fit an XGBoost model using parsnip
xgb_model <- boost_tree(mode = "classification") |>
   set_engine("xgboost") |>
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
backorders |>
  tidypredict_to_column(model = xgb_model) |>
  select(went_on_backorder, fit) |>
  head()

