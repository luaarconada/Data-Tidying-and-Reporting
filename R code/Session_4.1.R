## ----------------------------------------------------------------------------------------------
library(tidyverse)
df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

# map_dbl(df, mean)
map_int(df, mean) # Error, as expected
df %>% map_dbl(mean)

df %>% map_dbl(median)

df %>% map_dbl(sd)
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
map_dbl(df, mean, trim = 0.5)
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
z <- list(x = 1:3, y = 4:5)
map_int(z, length)
iris %>% map_int(length)
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
#cyl takes values 4, 6 and 8. Split is a base R function that splits something into group
mtcars_cyl <- mtcars %>%
  split(.$cyl) # It creates a list and each contains the observations for each value of cyl
str(mtcars_cyl)

models <- mtcars %>%
  split(.$cyl) %>%
  map(~ lm(mpg ~ wt, data = .))

# Equivalent to
f <- function(x) {
  lm(mpg ~ wt, data = x)
}
models <- mtcars %>%
  split(.$cyl) %>%
  map(f)
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
models %>%
  map(summary) %>%
  map_dbl("r.squared")
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
square <- function(x) {
  return(x * x)
}
vector1 <- c(2, 4, 5, 6)

# Using map() function to generate squares
map(vector1, square)
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
x <- c(2, 4, 5, 6)
y <- c(2, 3, 4, 5)
to_Power <- function(x, y) {
  return(x^y)
}
map2(x, y, to_Power)
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
map2(.x = x, .y = y, .f = ~ .x + .y)
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
mtcars_sub <- mtcars[1:5, c("mpg", "hp", "disp")]
pmap(mtcars_sub, sum)
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
mu <- list(5, 10, -3)
sigma <- list(1, 5, 10)
map2(mu, sigma, rnorm, n = 5) %>%
  str()

# be careful with the order of arguments!
map2(sigma, mu, rnorm, n = 5) %>%
  str()
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
n <- list(1, 3, 5)
args1 <- list(n, mu, sigma)
args1 %>%
  pmap(rnorm)
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
n <- list(1, 3, 5)
args2 <- list(sd = sigma, mean = mu, n = n) # With a change of order!
args2 %>%
  pmap(rnorm)
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
params <- tribble(
  ~mean, ~sd, ~n,
  5, 1, 1,
  10, 5, 3,
  -3, 10, 5
)

# The same as above, but less readable
tibble(mean = c(5, 10, -3), sd = c(1, 5, 1), n = c(1, 3, 5))

params %>%
  pmap(rnorm)
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
f <- c("runif", "rnorm", "rpois")
param <- list(
  list(min = -1, max = 1),
  list(sd = 5),
  list(lambda = 10)
)
invoke_map(f, param, n = 5) %>%
  str()
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
sim <- tribble(
  ~f, ~params,
  "runif", list(min = -1, max = 1),
  "rnorm", list(sd = 5),
  "rpois", list(lambda = 10)
)

sim <- sim %>%
  mutate(sim = invoke_map(f, params, n = 10))

# Extract first sample
sim$sim[1][[1]]
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
library(stringr)
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
double_quote <- "\""
single_quote <- "'"
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
x <- c("\"", "\\")
x
writeLines(x)
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
x <- "\u00b5"
x
"\u00A7"
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
str_length(c("a", "R is cool for data science", NA))
str_length(c(1, 10))
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
str_c("x", "y")
str_c("x", "y", "z")
str_c("x", "y", sep = ", ")

# Replacements for base R's paste
paste("x", "y", sep = "")
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
x <- c("abc", NA)
str_c("|-", x, "-|")
str_c("|-", str_replace_na(x), "-|")
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
str_c("prefix-", c("a", "b", "c"), "-suffix")
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
# Different
str_length(str_c("x", "y", "z"))
str_length(str_c(c("x", "y", "z")))

str_c(c("x", "y", "z"), collapse = ", ")
# sep and collapse arguments work are those from paste() in base R
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
x <- c("Apple", "Banana", "Pear")
str_sub(x, start = 1, end = 3) # start/end included
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
# In reverse mode: from the end
str_sub(x, start = -3, end = -1)
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
str_to_lower(x)
x %>% str_to_upper() %>%
  str_to_sentence()

# str_sub allows assignments!
str_sub(x, 1, 1) <- str_to_lower(str_sub(x, 1, 1))
x
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
x <- c("apple", "banana", "pear")
str_view(string = x, pattern = "an")
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
str_view(x, ".a.")
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
str_view(c("abc", "a.c", "bef."), "\\.") # Two \\!
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
x <- c("apple", "banana", "pear")
str_view(x, "^a")
str_view(x, "a$")
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
x <- c("apple pie", "apple", "apple cake")
str_view(x, "^apple$")

str_view(x, "[aeiou]") # Vowels
str_view(x, "[^aeiou]") # Constants
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
str_view(c("grey", "gray"), "gr(e|a)y")
str_view(c("grey", "gray"), "gr[e|a]y")
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
x <- c("apple", "banana", "pear")
str_detect(x, "e")
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
# Common words
length(words)
# How many common words start with t?
words %>%
  str_detect("^t") %>%
  mean()
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
# What proportion of common words end with a vowel?
words %>%
  str_detect("[aeiou]$") %>%
  mean()
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
no_vowels_1 <- !str_detect(words, "[aeiou]") # Negation of vowels
sum(no_vowels_1)
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
words[str_detect(words, "u$")]

# Or, much better:
str_subset(words, "u$")
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
df <- tibble(
  word = words,
  i = 1:length(words)
)
df %>%
  filter(str_detect(words, "x$"))
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
x <- c("apple", "banana", "pear")
str_count(x, "a")

# On average, how many vowels per word?
mean(str_count(words, "[aeiou]"))

# On average, how many consonants per word?
mean(str_count(words, "[^aeiou]"))
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
df %>%
  mutate(
    vowels = str_count(word, "[aeiou]"),
    consonants = str_count(word, "[^aeiou]")
  )
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
length(sentences)
head(sentences)
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
colors <- c(
  "red", "orange", "yellow", "green", "blue", "purple"
)
color_match <- str_c(colors, collapse = "|")
color_match
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
has_color <- str_subset(sentences, color_match)
matches <- str_extract(has_color, color_match)
head(matches)
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
more <- sentences[str_count(sentences, color_match) > 1]
str_view(more, color_match)
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
str_extract(more, color_match)
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
str_extract_all(more, color_match)
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
str_extract_all(more, color_match, simplify = TRUE)
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
x <- c("a", "a b", "a b c")
str_extract_all(x, "[a-z]", simplify = TRUE)
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
noun <- "(a|the) ([^ ]+)"
# Words: they begin with "a" or "the" and not a "space"
# https://cran.r-project.org/web/packages/stringr/vignettes/regular-expressions.html

has_noun <- sentences %>%
  str_subset(noun) %>%
  head(10)

has_noun %>%
  str_extract(noun)
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
has_noun %>%
  str_match(noun)
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
tibble(sentence = sentences) %>%
  tidyr::extract(
    sentence, c("article", "noun"), "(a|the) ([^ ]+)",
    remove = FALSE
  )
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
x <- c("apple", "pear", "banana")
str_replace(x, pattern = "[aeiou]", replacement = "-")
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
str_replace_all(x, pattern = "[aeiou]", replacement = "-")
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
x <- c("1 house", "2 cars", "3 people")
str_replace_all(x, pattern = c("1" = "one", "2" = "two", "3" = "three"))
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
sentences %>%
  head(5) %>%
  str_split(" ")
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
sentences %>%
  head(5) %>%
  str_split(" ", simplify = TRUE)
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
fields <- c("Name: Hadley", "Country: NZ", "Age: 35")
fields %>% str_split(": ", n = 2, simplify = TRUE)
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
x <- "This is a sentence. This is another sentence."
str_view(x, pattern = boundary("word"))
## ----------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------
str_split(x, boundary("word"))[[1]]
## ----------------------------------------------------------------------------------------------
