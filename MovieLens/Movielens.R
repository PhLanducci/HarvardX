##########################################################
##########################################################
#
# Title:  HarvardX Data Science Capstone Project "MovieLens"
# Author: Philippe Landucci
# Date:   January 8th, 2021
#
##########################################################
##########################################################

##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################
# Note: this process could take a couple of minutes
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))
movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))
# if using R 4.0 or later:
#movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
#                                           title = as.character(title),
#                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)
rm(dl, ratings, movies, test_index, temp, movielens, removed)

################################################################################
# End of code provided
################################################################################

# install additional packages and load libraries
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(recosystem)) install.packages("recosystem", repos = "http://cran.us.r-project.org")
library(stringr)
library(tidyr)
library(kableExtra)
library(knitr)
library(recosystem)

################################################################################
# Utilities
################################################################################

# I have a (Swiss) German version of Windows and need to change to English
# to ensure some values (e.g. month name) are displayed accordingly
my_LC_TIME <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "English")

# plot and table counters
table_inx <- 0
plot_inx <- 0

# table formatting function
myTableFormat <- function(table, colnames, table_inx, title = ""){
  var_caption <- paste("Table #", toString(table_inx))
  if (title != "")
    var_caption <- paste(var_caption, " - ", title)
  table %>% kable(col.names = colnames, caption = var_caption) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                position = "center", font_size = 12, full_width = FALSE)
}

myPoly <- function(lm_poly, x){
  y <- 0
  for (order in 1:length(lm_poly$coefficients))
    y <- y + (as.numeric(lm_poly$coefficients[order]) * x^(order - 1))
  return(y)
}

# "42 is the answer" ("A Hitchhiker's Guide to the Galaxy")
my_seed <- 42

rmse <- function(actual, predicted){
  sqrt(mean((actual - predicted)^2, na.rm = TRUE))
}

# RMSE goal values according to exercise
rmse_goal_1 <- 0.89999
rmse_goal_2 <- 0.86549
rmse_goal_3 <- 0.86499
rmse_goal_4 <- 0.86490

# clean-up / optimize memory after initial data load
my_gc_verbose <- TRUE
gc(verbose = my_gc_verbose, full = TRUE)

################################################################################
################################################################################
# 1. DATA EXPLORATION
#
# Background information about the Movielens 10M data set can be found under
# http://files.grouplens.org/datasets/movielens/ml-10m-README.html
#
# 1.1.  High-level exploration of the training and test data sets
# 1.2.  Extraction of movie release year into a separate column
# 1.3.  Oldest and latest movies
# 1.4.  Extraction of year of rating and movie age - when rated - into separate columns
# 1.5.  Sanity check - find/remove observations where movie rated before launch
# 1.6.  Exploration of movie age on mean of ratings
# 1.7.  Exploration of ratings
# 1.8.  Further exploration of movies
# 1.9.  Exploration of users
# 1.10. Exploration of genres
# 1.11. Structure of data sets after Data Exploration
# 1.12. Additional data preparation due to change in ratings scale in February 2003
#
################################################################################
################################################################################

################################################################################
# 1.1. High-level exploration of the training and test data sets
################################################################################

################################################################################
# 1.1.1. Structure, first rows and summary of the training data set (edx)

str(edx)
head(edx)
summary(edx)

cat("The training data set \"edx\" has", toString(dim(edx)[1]), "observations of",
  toString(dim(edx)[2]),"variables.\n")

cat("The variables are:\n",
"- ", colnames(edx)[1], "\t: User identification key (integer),\n",
"- ", colnames(edx)[2], "\t: Movie identification key (numeric),\n",
"- ", colnames(edx)[3], "\t: \"Five Stars\" type rating, ranging from 0.5 to 5 (numeric),\n",
"- ", colnames(edx)[4], "\t: Time stamp of rating, in Posix format (integer),\n",
"- ", colnames(edx)[5], "\t\t: Title and release year of movie (character string, release year in \"()\"),\n",
"- ", colnames(edx)[6], "\t: Genre(s) of movie (character string, multiple genres delimited by \"|\").\n",
  sep = "")

cat("In the training data set,", toString(n_distinct(edx$userId)), "users have rated",
  toString(n_distinct(edx$movieId)), "movies\n")

################################################################################
# 1.1.2. Structure, first rows and summary of the test data set (validation)

str(validation)
head(validation)
summary(validation)

cat("The test data set \"validation\" has", toString(dim(validation)[1]),
  "observations and - obviously - the same structure as the training set.\n")

cat("In the test data set,", toString(n_distinct(validation$userId)), "users have rated",
    toString(n_distinct(validation$movieId)), "movies\n")


################################################################################
# 1.2. Extraction of movie release year into a separate column
################################################################################

edx <- edx %>% mutate(year_released = as.integer(str_sub(title, start = str_length(title)-4, end = str_length(title)-1)),
                      title = str_sub(title, end = str_length(title)-7))

cat("The training data set now has the following structure:\n",
"- ", colnames(edx)[1], "\t: User identification key (integer),\n",
"- ", colnames(edx)[2], "\t: Movie identification key (numeric),\n",
"- ", colnames(edx)[3], "\t: \"Five Stars\" type rating, ranging from 0.5 to 5 (numeric),\n",
"- ", colnames(edx)[4], "\t: Time stamp of rating, in Posix format (integer),\n",
"- ", colnames(edx)[5], "\t\t: Title and release year of movie (character string, release year in \"()\"),\n",
"- ", colnames(edx)[6], "\t: Genre(s) of movie (character string, multiple genres delimited by \"|\"),\n",
"- ", colnames(edx)[7], "\t: Release year of movie (integer).\n",
sep = "")


################################################################################
# 1.3. Oldest and newest movies
################################################################################

cat("The next two tables show the oldest and newest movies in the training data set.\n")

################################################################################
# 1.3.1. Oldest movies according to release year

tmp <- edx %>% group_by(movieId, year_released, title) %>% arrange(year_released) %>% distinct(movieId) %>% head()
tmp$movieId <- NULL

table_inx <- table_inx + 1

tmp %>% kable(caption = paste("Table #", toString(table_inx), " - Oldest movies", sep = ""),
               col.names = c("Year released", "Movie title")) %>%
  row_spec(0, bold = TRUE) %>%
  kable_styling(position = "center", full_width = FALSE)

rm(tmp)

################################################################################
# 1.3.2. Latest movies according to release year

tmp <- edx %>% group_by(movieId, year_released, title) %>% arrange(desc(year_released)) %>% distinct(movieId) %>% head()
tmp$movieId <- NULL

table_inx <- table_inx + 1

tmp %>% kable(caption = paste("Table #", toString(table_inx), " - Latest movies", sep = ""),
              col.names = c("Year released", "Movie title")) %>%
  row_spec(0, bold = TRUE) %>%
  kable_styling(position = "center", full_width = FALSE)

rm(tmp)


################################################################################
# 1.4. Extraction of year of rating and movie age - when rated - into separate columns
################################################################################

# Extract year of rating from time stamp
edx <- edx %>% mutate(year_rated = year(as.POSIXct(timestamp, origin = "1970-01-01", tz = "UTC")))

# Add movie age (year rated minus year of release) column
edx <- edx %>% mutate(movie_age = as.integer(year_rated - year_released))

cat("The training data set now has the following structure:\n",
"- ", colnames(edx)[1], ": User identification key (integer).\n",
"- ", colnames(edx)[2], ": Movie identification key (numeric).\n",
"- ", colnames(edx)[3], ": \"Five Stars\" type rating, ranging from 0.5 to 5 (numeric).\n",
"- ", colnames(edx)[4], ": Time stamp of rating, in Posix format (integer).\n",
"- ", colnames(edx)[5], ": Title of movie (character string).\n",
"- ", colnames(edx)[6], ": Genre(s) of movie (character string, multiple genres delimited by \"|\").\n",
"- ", colnames(edx)[7], ": Release year of movie (integer).\n",
"- ", colnames(edx)[8], ": Year of rating (integer).\n",
"- ", colnames(edx)[9], ": Movie age, in years, when rated (integer).\n")


################################################################################
# 1.5. Sanity check - find/remove observations where movie rated before launch
################################################################################

rated_before_launch <- edx %>% filter(movie_age < 0) %>% count()

cat("Warning:", toString(rated_before_launch) , "movies have been rated before launch...\n")

rows_rated_before_launch <- which(edx$movie_age < 0)
edx <- edx[-c(rows_rated_before_launch), ]

cat("The number of observations in the training data set has been reduced by",
  toString(rated_before_launch),
  "to", toString(toString(dim(edx)[1])), "\n")

rm(rated_before_launch, rows_rated_before_launch)


################################################################################
# 1.6. Exploration of movie age on mean of ratings
################################################################################

################################################################################
# 1.6.1. Mean ratings per movie age

tmp <- edx %>%
  group_by(movie_age) %>%
  summarise(mean = mean(rating)) %>% arrange(desc(mean))

tmp1 <- tmp %>% head(n = 6)
tmp2 <- tmp %>% tail(n = 6) %>% arrange(mean)
tmp3 <- bind_cols(tmp1, tmp2)

table_inx <- table_inx + 1

tmp3 %>% kable(caption = paste("Table #", toString(table_inx), " - Ratings means vs. movie age", sep = ""),
          col.names = c("Movie age", "Ratings means (highests)", "Movie age", "Ratings means (lowests)")) %>%
          row_spec(0, bold = TRUE) %>%
          kable_styling(position = "center", full_width = FALSE)

rm(tmp1, tmp2, tmp3)

plot_inx <- plot_inx + 1

tmp_plot <- tmp %>% ggplot(aes(x = movie_age, y = mean)) +
  geom_point() +
  geom_smooth(formula = "y ~ x", method = "loess") +
  ggtitle(paste("Plot #", toString(plot_inx), "- Mean ratings vs. Movie age")) +
  xlab("Movie age") + ylab("Rating means")

tmp_plot
rm (tmp_plot)

################################################################################
# 1.6.2. Modeling mean ratings as a function of movie age
# The plot shows kind of a parabolic (2nd order) relationship between movie age and mean rating
# This assumption will be verified by fitting a linear model with a polynomial function
# Even though curve fitting can be - very slightly - improved with high orders,
# we stop at 4 to avoid over-fitting and rank deficiency

orders <- seq(2, 4, 1)

rmses <- sapply(orders, function(order){
  set.seed(my_seed, sample.kind="Rounding")
  lm_poly <- lm(mean ~ poly(movie_age, order, raw=TRUE), data = tmp)
  return(rmse(tmp$mean, predict.lm(lm_poly, new_data = tmp$movie_age)))
  }
)
cat("Best fitting result with order: ", toString(orders[which.min(rmses)]),
    ", rmse: ", sprintf(min(rmses), fmt = "%#.7f"), "\n", sep = "")

set.seed(my_seed, sample.kind="Rounding")
lm_ma_poly <- lm(mean ~ poly(movie_age, orders[which.min(rmses)], raw=TRUE), data = tmp)

# lm_ma_rmse <- rmse(tmp$mean, predict.lm(lm_ma_poly, new_data = tmp))
# 0.08977103

rm(tmp, orders, rmses)


################################################################################
# 1.7 Exploration of ratings
################################################################################

################################################################################
# 1.7.1. Distribution of number of ratings per number of stars

tmp <- edx %>% group_by(rating) %>%
  summarize(count = n()) %>% arrange(desc(count))

table_inx <- table_inx + 1

tmp %>% kable(caption = paste("Table #", toString(table_inx), " - Number of ratings vs. stars", sep = ""),
              col.names = c("Rating", "Count")) %>%
          row_spec(0, bold = TRUE) %>%
          kable_styling(position = "center", full_width = FALSE)

plot_inx <- plot_inx + 1

tmp_plot <- tmp %>%
  ggplot(aes(x = factor(rating), y = count)) +
  geom_col(fill = "gray") +
  ggtitle(paste("Plot #", toString(plot_inx), " - Number of ratings vs. stars", sep = "")) +
  xlab("Stars") + ylab("Count")

tmp_plot

rm(tmp, tmp_plot)

cat("The plot shows that there are systematically less half star that full star ratings, needs further exploration...\n")

################################################################################
# 1.7.2. Distribution of number of ratings per year

tmp <- edx %>% group_by(year_rated) %>%
  summarize(count = n())

table_inx <- table_inx + 1

tmp %>% kable(caption = paste("Table #", toString(table_inx), " - Ratings per year", sep = ""),
              col.names = c("Year rated", "Ratings")) %>%
          row_spec(0, bold = TRUE) %>%
          kable_styling(position = "center", full_width = FALSE)

plot_inx <- plot_inx + 1

tmp_plot <- tmp %>% ggplot(aes(x = year_rated, y = count)) +
  geom_col(col="white", fill="gray") +
  ggtitle(paste("Plot #", toString(plot_inx), " - Ratings per year", sep = "")) +
  xlab("Rating year") + ylab("Number of ratings")

tmp_plot

rm(tmp, tmp_plot)

################################################################################
# 1.7.3. Distribution of ratings per year

tmp <- edx %>% group_by(year_rated, rating) %>% summarize(count = n())

plot_inx <- plot_inx + 1

tmp_plot <- tmp %>% ggplot(aes(x = year_rated, y = as.factor(rating))) +
  geom_tile(aes(fill = count), show.legend = FALSE) +
  geom_text(aes(label = round(count/10000, 2)), colour = "white") +
  scale_x_continuous(breaks = seq(min(edx$year_rated), max(edx$year_rated), by = 2)) +
  ggtitle(paste("Plot #", toString(plot_inx), " - Distribution of ratings per year", sep = ""),
    subtitle = "(in 10000)") +
  xlab("Year rated") + ylab("Ratings (Stars)")

tmp_plot

rm(tmp, tmp_plot)

cat("There is a clear change in the ratings scale in 2003, next step is to find the exact date.\n")

tmp1 <- edx %>% group_by(year_rated, rating) %>% filter((rating * 2) %% 2 == 1) %>%
  arrange(year_rated) %>% head(n = 1)
tmp2 <- edx %>% filter(year_rated == tmp1$year_rated) %>% group_by(rating, timestamp) %>%
  filter((rating * 2) %% 2 == 1) %>% arrange(timestamp) %>% head(n = 1)

edx_ts_start_hp <- tmp2$timestamp

rm(tmp1, tmp2)

edx_date_start_hp <- as.Date(as.POSIXct(edx_ts_start_hp, origin = "1970-01-01", tz = "UTC"))

cat("Half stars ratings are in the training set since", format(edx_date_start_hp, "%B %d, %Y"))

# Check for half stars rating start in the test set

# Extraction of release year is done already here to have the order of variables
# in the training and test sets
validation <- validation %>% mutate(year_released = as.integer(str_sub(title,
                              start = str_length(title)-4, end = str_length(title)-1)))

# Extract year of rating from time stamp
validation <- validation %>% mutate(year_rated = year(as.POSIXct(timestamp, origin = "1970-01-01", tz = "UTC")))

tmp1 <- validation %>% group_by(year_rated, rating) %>% filter((rating * 2) %% 2 == 1) %>%
  arrange(year_rated) %>% head(n = 1)
tmp2 <- validation %>% filter(year_rated == tmp1$year_rated) %>% group_by(rating, timestamp) %>%
  filter((rating * 2) %% 2 == 1) %>% arrange(timestamp) %>% head(n = 1)

validation_ts_start_hp <- tmp2$timestamp

rm(tmp1, tmp2)

validation_date_start_hp <- as.Date(as.POSIXct(validation_ts_start_hp, origin = "1970-01-01", tz = "UTC"))

cat("Half stars ratings are in the validation set since", format(validation_date_start_hp, "%B %d, %Y"))


################################################################################
# 1.8. Further exploration of movies
################################################################################

################################################################################
# 1.8.1 Distribution of movies vs. number of ratings

plot_inx <- plot_inx + 1

tmp_plot <- edx %>% group_by(movieId) %>% summarize(count = n()) %>%
  ggplot(aes(x = count)) +
  geom_histogram(binwidth = 0.1, col="white", fill="gray") +
  scale_x_log10() +
  ggtitle(paste("Plot #", toString(plot_inx), " - Movies vs. number of ratings", sep = "")) +
  xlab("Ratings") + ylab("Movies")

tmp_plot

rm(tmp_plot)

################################################################################
# 1.8.2. Most and least rated movies

cat("The next two tables show the most and least rated movies in the training data set.\n")

# Most rated movies
tmp <- edx %>% group_by(movieId, title) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  head(n=10)

tmp$movieId <- NULL

table_inx <- table_inx + 1

tmp %>% kable(caption = paste("Table #", toString(table_inx), " - Most rated movies", sep = ""),
              col.names = c("Movie title", "Ratings")) %>%
          row_spec(0, bold = TRUE) %>%
          kable_styling(position = "center", full_width = FALSE)

rm(tmp)

# Least rated movies
tmp <- edx %>% group_by(movieId, title) %>%
  summarize(count = n()) %>%
  arrange(count) %>%
  head(n = 10)

tmp$movieId <- NULL

table_inx <- table_inx + 1

tmp %>% kable(caption = paste("Table #", toString(table_inx), " - Least rated movies", sep = ""),
              col.names = c("Movie title", "Ratings")) %>%
          row_spec(0, bold = TRUE) %>%
          kable_styling(position = "center", full_width = FALSE)

rm(tmp)

# titles are no more used, free up some memory...
edx$title <- NULL
gc(verbose = my_gc_verbose, full = TRUE)


################################################################################
# 1.9. Exploration of Users
################################################################################

################################################################################
# 1.9.1. Distribution of users vs. number of ratings

plot_inx <- plot_inx + 1

tmp_plot <- edx %>% group_by(userId) %>% summarize(count = n()) %>%
  ggplot(aes(x = count)) +
  geom_histogram(binwidth = 0.05, col="white", fill="gray") +
  scale_x_log10() +
  ggtitle(paste("Plot #", toString(plot_inx), " - Users vs. number of ratings", sep = "")) +
  xlab("Ratings") + ylab("Users")

tmp_plot

rm(tmp_plot)

################################################################################
# 1.9.2. Users having given the lowest and highest numbers of rankings

tmp <- edx %>% group_by(userId) %>% summarize(count = n()) %>%
  summarize(min = min(count), max = max(count))

user_min <- edx %>% group_by(userId) %>% summarize(count = n()) %>%
  filter(count == tmp$min)

user_max <- edx %>% group_by(userId) %>% summarize(count = n()) %>%
  filter(count == tmp$max)

tmp1 <- edx %>% filter(userId == user_min$userId) %>% distinct(year_rated) %>%
  arrange(year_rated)

tmp2 <- edx %>% filter(userId == user_max$userId) %>% distinct(year_rated) %>%
  arrange(year_rated)

user_stats_info <- function(years, userId, ratings){
  if (nrow(years) == 1) {
    cat("User with Id", toString(userId), "has given", toString(ratings),
      "ratings in", toString(years), "\n")
  } else {
    cat("User with Id", toString(userId), "has given", toString(ratings),
      "ratings from", toString(min(years)), "to",  toString(max(years)), "\n")
  }
}

cat("The next lines show which users have given the lowest and highest numbers of rankings:\n")
user_stats_info(tmp1, user_min$userId, tmp$min)
user_stats_info(tmp2, user_max$userId, tmp$max)

rm(tmp, tmp1, tmp2, user_min, user_max)
gc(verbose = my_gc_verbose, full = TRUE)


################################################################################
# 1.10. Exploration of Genres
################################################################################

################################################################################
# 1.10.1. Distribution of ratings vs. number of genres

# Add column with the number of genres of each movie
edx <- edx %>% mutate(genres_cnt = as.integer(1 + str_count(genres, "\\|")))

# Number of ratings of movies having one or more genres
tmp <- edx %>% group_by(genres_cnt) %>% summarize(count = n()) %>% arrange(desc(count))

table_inx <- table_inx + 1

tmp %>% kable(caption = paste("Table #", toString(table_inx), " - Ratings per number of genres", sep = ""),
              col.names = c("# Genres", "# Ratings")) %>%
          row_spec(0, bold = TRUE) %>%
          kable_styling(position = "center", full_width = FALSE)

rm(tmp)

# This additional variable will not be used later, save memory...
edx$genres_cnt <- NULL
gc(verbose = my_gc_verbose, full = TRUE)

################################################################################
# 1.10.2. Generate a temporary data set for more detailed exploration (takes a lot of time...)

tmp_edx <- edx %>% select(genres, rating) %>%
  mutate(genre = factor(genres)) %>%
  separate_rows(genre, sep = "\\|")

# The "genres" variable is no more needed, free up memory...
tmp_edx$genres <- NULL
gc(verbose = my_gc_verbose, full = TRUE)

################################################################################
# 1.10.3. Distribution of ratings per genre

tmp <- tmp_edx %>%
  group_by(genre) %>%
  summarize(count = n()) %>% arrange(desc(count))

table_inx <- table_inx + 1

tmp %>% kable(caption = paste("Table #", toString(table_inx), " - Ratings per genre", sep = ""),
              col.names = c("Genres", "Ratings")) %>%
          row_spec(0, bold = TRUE) %>%
          kable_styling(position = "center", full_width = FALSE)

plot_inx <- plot_inx + 1

tmp_plot <- tmp %>%
  mutate(genre = fct_reorder(genre, count)) %>%   
  ggplot(aes(x = count, y = genre)) +
  geom_col(fill = "gray") +
  ggtitle(paste("Plot #", toString(plot_inx), " - Ratings per genre", sep = "")) +
  xlab("Ratings") + ylab("Genre")

tmp_plot

rm(tmp, tmp_plot)

################################################################################
# 1.10.4. Handling of observations with "(no genres listed)"
# genre "(no genres listed)" is a candidate outlier
# there is only 1 movie without genre - and this only in edx, not in validation data set

edx %>% filter(genres == "(no genres listed)") %>% summarize(edx_ngl_cnt = n())

remove_edx_rows <- which(edx$genres == "(no genres listed)")

edx <- edx[-c(remove_edx_rows), ]

cat("The number of observations in the training data set has been reduced by",
  toString(length(remove_edx_rows)), "to", toString(toString(dim(edx)[1])), "\n")

rm(remove_edx_rows)

################################################################################
# 1.10.5. Rating means by genre

tmp <- tmp_edx %>%
  group_by(genre) %>%
  summarize(mean = mean(rating)) %>% arrange(desc(mean))

table_inx <- table_inx + 1

tmp %>% kable(caption = paste("Table #", toString(table_inx), " - Ratings mean per genre", sep = ""),
              col.names = c("Genres", "Ratings mean")) %>%
          row_spec(0, bold = TRUE) %>%
          kable_styling(position = "center", full_width = FALSE)

plot_inx <- plot_inx + 1

tmp_plot <- tmp %>%
  mutate(genre = fct_reorder(genre, mean)) %>%   
  ggplot(aes(x = mean, y = genre)) +
  geom_col(fill = "gray") +
  ggtitle(paste("Plot #", toString(plot_inx), " - Rating means per genre", sep = "")) +
  xlab("Rating mean") + ylab("Genre")

tmp_plot

################################################################################
# 1.10.6. Conclusion on genre(s)

cor_rating_genre <- cor(tmp_edx$rating, as.numeric(factor(tmp_edx$genre)))

cat("Exploration of genres does not indicate a strong correlation to ratings.\n")
cat("This is confirmed by a very low correlation between genres and ratings:",
    sprintf(cor_rating_genre, fmt = "%#.7f"), "\n")
cat("Variables related to genres can be dropped to reduce memory usage.\n")

rm(tmp, tmp_plot, cor_rating_genre)
rm(tmp_edx)
edx$genres <- NULL
gc(verbose = my_gc_verbose, full = TRUE)


################################################################################
# 1.11. Structure of data sets after Data Exploration
################################################################################

################################################################################
# 1.11.1. Training data set

cat("The training data set now has the following structure:\n",
"- ", colnames(edx)[1], ": User identification key (integer).\n",
"- ", colnames(edx)[2], ": Movie identification key (numeric).\n",
"- ", colnames(edx)[3], ": \"Five Stars\" type rating, ranging from 0.5 to 5 (numeric).\n",
"- ", colnames(edx)[4], ": Time stamp of rating, in Posix format (integer).\n",
"- ", colnames(edx)[5], ": Release year of movie (integer).\n",
"- ", colnames(edx)[6], ": Year of rating (integer).\n",
"- ", colnames(edx)[7], ": Movie age, when rated (integer).\n")

################################################################################
# 1.11.2. Test data set

# Extract the movie_age variable (release year and year_rated were extracted in section 1.7.c)
validation <- validation %>% mutate(movie_age = as.integer(year_rated - year_released))

# Movie genre(s) and title won't be used to fit models and therefore
# can be removed from the test set, again to reduce memory usage...

validation$genres <- NULL
validation$title <- NULL
gc(verbose = my_gc_verbose, full = TRUE)
#str(validation)

cat("The test data set now has the following structure:\n",
"- ", colnames(validation)[1], ": User identification key (integer).\n",
"- ", colnames(validation)[2], ": Movie identification key (numeric).\n",
"- ", colnames(validation)[3], ": \"Five Stars\" type rating, ranging from 0.5 to 5 (numeric).\n",
"- ", colnames(validation)[4], ": Time stamp of rating, in Posix format (integer).\n",
"- ", colnames(validation)[5], ": Release year of movie (integer).\n",
"- ", colnames(validation)[6], ": Year of rating (integer).\n",
"- ", colnames(validation)[7], ": Movie age, when rated (integer).\n")


################################################################################
################################################################################
# 2. MODELS AND THEIR PERFORMANCE (RMSE)
#
# 2.1. Performance evaluation metric: RMSE
# 2.2. Additional data preparation incl. splitting "edx" data into training and test sets
# 2.3. Baseline "Naive" (just using the mean) models
# 2.4. "Movie effect" models
# 2.5. "Movie Age effect" models
# 2.6. "Movie and User effects" models
# 2.7. "Regularized Movie and User effects" models
# 2.8. "Matrix Factorization" models
#
################################################################################
################################################################################

################################################################################
# 2.1. Performance evaluation metric: RMSE
#
# The root-mean-square error error (RMSE) or root-mean-square deviation (RMSD)
# is a frequently used measure of the differences between values predicted by a
# model and the values observed.
# RMSE is the square root of the average of squared errors and is therefore always
# non-negative. A value of 0 (almost never achieved in practice) would indicate a
# perfect fit to the data. In general, a lower RMSE is better than a higher one.
# The effect of each error on RMSE is proportional to the size of the squared error;
# thus larger errors have a disproportionately large effect on RMSE.
# Consequently, RMSE is sensitive to outliers.
# Source: https://en.wikipedia.org/wiki/Root-mean-square_deviation
################################################################################

################################################################################
# 2.2. Split of "edx" data into training and test sets and further splits
# due to change in ratings scale in February 2003
################################################################################

################################################################################
# 2.2.1. Split of "edx" data into training and test sets
# (same procedure as in code provided...)

# Test set will be 20% of edx data
set.seed(my_seed)
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE)
training_set <- edx[-test_index,]
test_set <- edx[test_index,]
rm(test_index)

# Make sure userId and movieId in the test set are also in the training set
test_set <- test_set %>% 
  semi_join(training_set, by = "movieId") %>%
  semi_join(training_set, by = "userId")

# timestamps are no more needed
training_set$timestamp <- NULL
test_set$time$stamp <- NULL

str(training_set)
str(test_set)

cat("The \"original\" training set \"training_set\" has", toString(dim(training_set)[1]),
    "observations of", toString(dim(training_set)[2]),"variables.\n")
cat("This is ", sprintf(100*dim(training_set)[1]/dim(edx)[1], fmt = "%#.2f"),
    "% of the original training set before splitting (\"edx\")\n", sep="")

cat("The \"original\" test set \"test_set\" has", toString(dim(test_set)[1]),
    "observations of", toString(dim(test_set)[2]),"variables.\n")
cat("This is ", sprintf(100*dim(test_set)[1]/dim(edx)[1], fmt = "%#.2f"),
    "% of the original training set before splitting (\"edx\")\n", sep="")

################################################################################
# 2.2.2. Additional data preparation due to change in ratings scale in February 2003
#
# In addition to the original training and test data sets,
# those will be split into data sets reflecting the change in ratings scale in 2003
# - "full stars (fs)" data sets from 1st rating date to date of change (excluded)
# - "half stars (hs)" data sets from date of change (included) to last rating date
# And again, those data sets will be split into training and test sets
################################################################################

# "full stars" sets (before and after split into training and test sets)
edx_fs <- edx %>% filter(timestamp < edx_ts_start_hp) %>% select(-timestamp)
validation_fs <- validation %>% filter(timestamp < validation_ts_start_hp) %>% select(-timestamp)

# Test set will be 20% of edx_fs data
set.seed(my_seed)
test_index <- createDataPartition(y = edx_fs$rating, times = 1, p = 0.2, list = FALSE)
training_set_fs <- edx_fs[-test_index,]
test_set_fs <- edx_fs[test_index,]

# Make sure userId and movieId in the test set are also in the training set
test_set_fs <- test_set_fs %>% 
  semi_join(training_set_fs, by = "movieId") %>%
  semi_join(training_set_fs, by = "userId")

str(training_set_fs)
str(test_set_fs)

cat("The \"full stars\" training set \"training_set_fs\" has", toString(dim(training_set_fs)[1]),
    "observations of", toString(dim(training_set_fs)[2]),"variables.\n")
cat("This is ", sprintf(100*dim(training_set_fs)[1]/dim(edx_fs)[1], fmt = "%#.2f"),
    "% of the original training set before splitting (\"edx_fs\") \n", sep="")

cat("The \"full stars\" test set \"test_set_fs\" has", toString(dim(test_set_fs)[1]),
    "observations of", toString(dim(test_set_fs)[2]),"variables.\n")
cat("This is ", sprintf(100*dim(test_set_fs)[1]/dim(edx_fs)[1], fmt = "%#.2f"),
    "% of the original training set before splitting (\"edx_fs\") \n", sep = "")

cat("The \"full stars\" validation data set \"validation_fs\" has", toString(dim(validation_fs)[1]),
    "observations of", toString(dim(validation_fs)[2]),"variables.\n")
cat("This is ", sprintf(100*dim(validation_fs)[1]/dim(validation)[1], fmt = "%#.2f"),
    "% of the original validation set\n", sep="")

# "half stars" sets (before and after split into training and test sets)
edx_hs <- edx %>% filter(timestamp >= edx_ts_start_hp) %>% select(-timestamp)
validation_hs <- validation %>% filter(timestamp >= validation_ts_start_hp) %>% select(-timestamp)

# Test set will be 20% of edx_fs data
set.seed(my_seed)
test_index <- createDataPartition(y = edx_hs$rating, times = 1, p = 0.2, list = FALSE)
training_set_hs <- edx_hs[-test_index,]
test_set_hs <- edx_hs[test_index,]

# Make sure userId and movieId in the test set are also in the training set
test_set_hs <- test_set_hs %>% 
  semi_join(training_set_hs, by = "movieId") %>%
  semi_join(training_set_hs, by = "userId")

str(training_set_hs)
str(test_set_hs)

cat("The \"half stars\" training set \"training_set_hs\" has", toString(dim(training_set_hs)[1]),
    "observations of", toString(dim(training_set_hs)[2]),"variables.\n")
cat("This is ", sprintf(100*dim(training_set_hs)[1]/dim(edx_hs)[1], fmt = "%#.2f"),
    "% of the original training set before splitting (\"edx_hs\") \n", sep="")

cat("The \"half stars\" test set \"test_set_hs\" has", toString(dim(test_set_hs)[1]),
    "observations of", toString(dim(test_set_hs)[2]),"variables.\n")
cat("This is ", sprintf(100*dim(test_set_hs)[1]/dim(edx_hs)[1], fmt = "%#.2f"),
    "% of the original training set before splitting (\"edx_hs\") \n", sep = "")

cat("The \"half stars\" validation data set \"validation_hs\" has", toString(dim(validation_hs)[1]),
    "observations of", toString(dim(validation_hs)[2]),"variables.\n")
cat("This is ", sprintf(100*dim(validation_hs)[1]/dim(validation)[1], fmt = "%#.2f"),
    "% of the original validation set\n", sep="")

# the "timestamp" variable is no longer needed and can therefore be dropped
# from the original data sets to free up memory...

edx$timestamp <- NULL
validation$timestamp <- NULL
gc(verbose = my_gc_verbose, full = TRUE)


################################################################################
# 2.3. Baseline "Naive" (just using the mean) models
################################################################################

################################################################################
# 2.3.1. Get the mean ratings of the original and split training sets

mu <- mean(edx$rating)
# 3.512472
mu_fs <- mean(edx_fs$rating)
# 3.555887
mu_hs <- mean(edx_hs$rating)
# 3.465126
training_mu <- mean(training_set$rating)
# 3.5123848
training_mu_fs <- mean(training_set_fs$rating)
# 3.5559893
training_mu_hs <- mean(training_set_hs$rating)
# 3.4652699

cat("Mean ratings of the original training sets (\"edx\", \"edx_fs\" and \"edx_hs\") are:\n",
    sprintf(mu, fmt = "%#.7f"), ", ", sprintf(mu_fs, fmt = "%#.7f"),
    " and ", sprintf(mu_hs, fmt = "%#.7f"), "\n", sep = "")

cat("Mean ratings of the split training sets (\"training_set\", \"training_set_fs\" and \"training_set_hs\") are:\n",
    sprintf(training_mu, fmt = "%#.7f"), ", ", sprintf(training_mu_fs, fmt = "%#.7f"),
    " and ", sprintf(training_mu_hs, fmt = "%#.7f"), "\n", sep = "")

################################################################################
# 2.3.2. "Naive" model's RMSE on the original data sets

naive_predicted_test <- rep(training_mu, nrow(test_set))
naive_rmse_test <- rmse(test_set$rating, naive_predicted_test)
# 1.00597

naive_predicted_val <- rep(training_mu, nrow(validation))
naive_rmse_val <- rmse(validation$rating, naive_predicted_val)
# 1.0612018

cat("The \"Naive\" model achieves an RMSE of",
    sprintf(naive_rmse_val, fmt = "%#.7f"), "on the original data sets")

all_rmse_results <- tibble(Model = "Naive (just the average)",
                      `Data Sets` = "original",
                      RMSE = sprintf(naive_rmse_val, fmt = "%#.7f"))

rm(naive_predicted_test, naive_rmse_test, naive_predicted_val, naive_rmse_val)

################################################################################
# 2.3.3. "Naive" model's RMSE on the "full stars" data sets

naive_fs_predicted_test <- rep(training_mu_fs, nrow(test_set_fs))
naive_fs_rmse_test <- rmse(test_set_fs$rating, naive_fs_predicted_test)
# 1.087191

naive_fs_predicted_val <- rep(training_mu_fs, nrow(validation_fs))
naive_fs_rmse_val <- rmse(validation_fs$rating, naive_fs_predicted_val)
# 1.0864602

cat("The \"Naive\" model achieves an RMSE of",
    sprintf(naive_fs_rmse_val, fmt = "%#.7f"), "on the \"full stars\" data sets")

all_rmse_results <- bind_rows(all_rmse_results, tibble(Model = "Naive (just the average)",
                                                  `Data Sets` = "full stars",
                                                  RMSE =  sprintf(naive_fs_rmse_val, fmt = "%#.7f")))

rm(naive_fs_predicted_test, naive_fs_rmse_test, naive_fs_predicted_val, naive_fs_rmse_val)

################################################################################
# 2.3.4. "Naive" model's RMSE on the "half stars" data sets

naive_hs_predicted_test <- rep(training_mu_hs, nrow(test_set_hs))
naive_hs_rmse_test <- rmse(test_set_hs$rating, naive_hs_predicted_test)
# 1.029385

naive_hs_predicted_val <- rep(training_mu_hs, nrow(validation_hs))
naive_hs_rmse_val <- rmse(validation_hs$rating, naive_hs_predicted_val)
# 1.0306833

cat("The \"Naive\" model achieves an RMSE of",
    sprintf(naive_hs_rmse_val, fmt = "%#.7f"), "on the \"half stars\" data sets")

all_rmse_results <- bind_rows(all_rmse_results, tibble(Model = "Naive (just the average)",
                                                  `Data Sets` = "half stars",
                                                  RMSE = sprintf(naive_hs_rmse_val, fmt = "%#.7f")))

rm(naive_hs_predicted_test, naive_hs_rmse_test, naive_hs_predicted_val, naive_hs_rmse_val)

################################################################################
# 2.3.5. "Naive" model's RMSE on all three data sets and first observations

tmp <- all_rmse_results %>% filter(str_detect(Model, "Naive"))

table_inx <- table_inx + 1

tmp %>% kable(caption = paste("Table #", toString(table_inx), " - Results of \"Naive\" models", sep = ""),
              col.names = c("Model", "Data Sets", "RMSE")) %>%
          row_spec(0, bold = TRUE) %>%
          kable_styling(position = "center", full_width = FALSE)

rm(tmp)

cat("The \"Naive\" model based on data after switch to \"half stars\" ratings has a lower RMSE
than those based on data before this switch or based on original data.\n
We will see if the same applies to the next models' results.\n")


################################################################################
# 2.4. "Movie effect" models
################################################################################

################################################################################
# 2.4.1. "Movie effect" model and its RMSE on the original data sets

training_movie_avgs <- training_set %>%
  group_by(movieId) %>%
  summarize(b_movie = mean(rating - training_mu))

plot_inx <- plot_inx + 1

tmp_plot <- training_movie_avgs %>% ggplot(aes(x = b_movie)) +
  geom_histogram(binwidth = 0.1, col="white", fill="gray") +
  ggtitle(paste("Plot #", toString(plot_inx), " - Distribution of Movie effect", sep = ""),
    subtitle="(original training set)") +
  xlab("Movie effect") + ylab("Count")

tmp_plot

rm(tmp_plot)

lm_bm_predicted_test <- test_set %>%
  left_join(training_movie_avgs, by = "movieId") %>%
  mutate(pred = training_mu + b_movie) %>%
  .$pred

lm_bm_rmse_test <- rmse(test_set$rating, lm_bm_predicted_test)
# 0.9433617

# Validation
edx_movie_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_movie = mean(rating - mu))

lm_bm_predicted_val <- validation %>%
  left_join(edx_movie_avgs, by = "movieId") %>%
  mutate(pred = mu + b_movie) %>%
  .$pred

lm_bm_rmse_val <- rmse(validation$rating, lm_bm_predicted_val)
# 0.9439100

cat("The \"Movie effect\" model achieves an RMSE of",
    sprintf(lm_bm_rmse_val, fmt = "%#.7f"), "on the original data sets")

all_rmse_results <- bind_rows(all_rmse_results, tibble(Model = "Movie effect",
                                                  `Data Sets` = "original",
                                                  RMSE = sprintf(lm_bm_rmse_val, fmt = "%#.7f")))

rm(lm_bm_predicted_test, lm_bm_rmse_test, lm_bm_predicted_val, lm_bm_rmse_val)

################################################################################
# 2.4.2. "Movie effect" model and its RMSE on the "full stars" data sets

training_fs_movie_avgs <- training_set_fs %>%
  group_by(movieId) %>%
  summarize(b_fs_movie = mean(rating - training_mu_fs))

plot_inx <- plot_inx + 1

tmp_plot <- training_fs_movie_avgs %>% ggplot(aes(x = b_fs_movie)) +
  geom_histogram(binwidth = 0.1, col="white", fill="gray") +
  ggtitle(paste("Plot #", toString(plot_inx), " - Distribution of Movie effect", sep = ""),
    subtitle="(\"full stars\" training set)") +
  xlab("Movie effect") + ylab("Count")

tmp_plot

rm(tmp_plot)

lm_fs_bm_predicted_test <- test_set_fs %>%
  left_join(training_fs_movie_avgs, by = "movieId") %>%
  mutate(pred = training_mu_fs + b_fs_movie) %>%
  .$pred

lm_fs_bm_rmse_test <- rmse(test_set_fs$rating, lm_fs_bm_predicted_test)
# 0.9690648

# validation
edx_fs_movie_avgs <- edx_fs %>%
  group_by(movieId) %>%
  summarize(b_fs_movie = mean(rating - mu_fs))

lm_fs_bm_predicted_val <- validation_fs %>%
  left_join(edx_fs_movie_avgs, by = "movieId") %>%
  mutate(pred = mu_fs + b_fs_movie) %>%
  .$pred

lm_fs_bm_rmse_val <- rmse(validation_fs$rating, lm_fs_bm_predicted_val)
# 0.9680030

cat("The \"Movie effect\" model achieves an RMSE of",
    sprintf(lm_fs_bm_rmse_val, fmt = "%#.7f"), "on the \"full stars\" data sets")

all_rmse_results <- bind_rows(all_rmse_results, tibble(Model = "Movie effect",
                                                  `Data Sets` = "full stars",
                                                  RMSE = sprintf(lm_fs_bm_rmse_val, fmt = "%#.7f")))

rm(lm_fs_bm_predicted_test, lm_fs_bm_rmse_test, lm_fs_bm_predicted_val, lm_fs_bm_rmse_val)

################################################################################
# 2.4.3. "Movie effect" model and its RMSE on the "half stars" data sets

training_hs_movie_avgs <- training_set_hs %>%
  group_by(movieId) %>%
  summarize(b_hs_movie = mean(rating - training_mu_hs))

plot_inx <- plot_inx + 1

tmp_plot <- training_hs_movie_avgs %>% ggplot(aes(x = b_hs_movie)) +
  geom_histogram(binwidth = 0.1, col="white", fill="gray") +
  ggtitle(paste("Plot #", toString(plot_inx), " - Distribution of Movie effect", sep = ""),
    subtitle="(\"half stars\" training set)") +
  xlab("Movie effect") + ylab("Count")

tmp_plot

rm(tmp_plot)

lm_hs_bm_predicted_test <- test_set_hs %>%
  left_join(training_hs_movie_avgs, by = "movieId") %>%
  mutate(pred = training_mu_hs + b_hs_movie) %>%
  .$pred

lm_hs_bm_rmse_test <- rmse(test_set_hs$rating, lm_hs_bm_predicted_test)
# 0.9055151

# Validation
edx_hs_movie_avgs <- edx_hs %>%
  group_by(movieId) %>%
  summarize(b_hs_movie = mean(rating - mu_hs))

lm_hs_bm_predicted_val <- validation_hs %>%
  left_join(edx_hs_movie_avgs, by = "movieId") %>%
  mutate(pred = mu_hs + b_hs_movie) %>%
  .$pred

lm_hs_bm_rmse_val <- rmse(validation_hs$rating, lm_hs_bm_predicted_val)
# 0.9060957

cat("The \"Movie effect\" model achieves an RMSE of",
    sprintf(lm_hs_bm_rmse_val, fmt = "%#.7f"), "on the \"half stars\" data sets")

all_rmse_results <- bind_rows(all_rmse_results, tibble(Model = "Movie effect",
                                                  `Data Sets` = "half stars",
                                                  RMSE = sprintf(lm_hs_bm_rmse_val, fmt = "%#.7f")))

rm(lm_hs_bm_predicted_test, lm_hs_bm_rmse_test, lm_hs_bm_predicted_val, lm_hs_bm_rmse_val)

################################################################################
# 2.4.4. "Movie effect" models' RMSE on all three data sets

tmp <- all_rmse_results %>% filter(str_detect(Model, "Movie effect"))

table_inx <- table_inx + 1

tmp %>% kable(caption = paste("Table #", toString(table_inx),
                          " - Results of \"Movie effect\" models", sep = ""),
          col.names = c("Model", "Data Sets", "RMSE")) %>%
          row_spec(0, bold = TRUE) %>%
          kable_styling(position = "center", full_width = FALSE)

rm(tmp)
gc(verbose = my_gc_verbose, full = TRUE)


################################################################################
# 2.5. "Movie Age effect" models
################################################################################

################################################################################
# 2.5.1. "Movie Age effect" model and its RMSE on the original data sets

# Generate a temporary data set with the mean age of each movie
# Reminder: movie-age = year_rated - year_released and they are many ratings per movie
tmp <- training_set %>% group_by(movieId) %>% summarize(mean_age = mean(movie_age))

# this does not work, returns only 94 values (model's training set size)
# maybe because new_data has much more rows...
# tmp$predicted_mean_rating <- predict.lm(lm_ma_poly, new_data = tmp)

# Compute the predictions with a polynomial function using the model's coefficients
tmp$predicted_mean_rating <- myPoly(lm_ma_poly, tmp$mean_age)

training_age_avgs <- training_set %>%
  group_by(movieId) %>%
  summarize(b_age = mean(rating - tmp$predicted_mean_rating))

plot_inx <- plot_inx + 1

tmp_plot <- training_age_avgs %>% ggplot(aes(x = b_age)) +
  geom_histogram(binwidth = 0.1, col="white", fill="gray") +
  ggtitle(paste("Plot #", toString(plot_inx), " - Distribution of Movie Age effect", sep = ""),
    subtitle="(original training set)") +
  xlab("Movie Age effect") + ylab("Count")

tmp_plot

rm(tmp_plot)

lm_ba_predicted_test <- test_set %>%
  left_join(training_age_avgs, by = "movieId") %>%
  mutate(pred = training_mu + b_age) %>%
  .$pred

lm_ba_rmse_test <- rmse(test_set$rating, lm_ba_predicted_test)
# 0.9441262

# validation
tmp <- edx %>% group_by(movieId) %>% summarize(mean_age = mean(movie_age))

tmp$predicted_mean_rating <- myPoly(lm_ma_poly, tmp$mean_age)

edx_age_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_age = mean(rating - tmp$predicted_mean_rating))

lm_ba_predicted_val <- validation %>%
  left_join(edx_age_avgs, by = "movieId") %>%
  mutate(pred = mu + b_age) %>%
  .$pred

lm_ba_rmse_val <- rmse(validation$rating, lm_ba_predicted_val)
# 0.9446426

cat("The \"Movie Age effect\" model achieves an RMSE of",
    sprintf(lm_ba_rmse_val, fmt = "%#.7f"), "on the original data sets")

all_rmse_results <- bind_rows(all_rmse_results, tibble(Model = "Movie Age effect",
                                                  `Data Sets` = "original",
                                                  RMSE = sprintf(lm_ba_rmse_val, fmt = "%#.7f")))

rm(tmp, lm_ba_predicted_test, lm_ba_rmse_test, lm_ba_predicted_val, lm_ba_rmse_val)

################################################################################
# 2.5.2. "Movie Age effect" model and its RMSE on the "full stars" data sets

# Generate a temporary data set with the mean age of each movie
tmp <- training_set_fs %>% group_by(movieId) %>% summarize(mean_age = mean(movie_age))

# Compute the predictions with a polynomial function using the model's coefficients
tmp$predicted_mean_rating <- myPoly(lm_ma_poly, tmp$mean_age)

training_fs_age_avgs <- training_set_fs %>%
  group_by(movieId) %>%
  summarize(b_fs_age = mean(rating - tmp$predicted_mean_rating))

plot_inx <- plot_inx + 1

tmp_plot <- training_fs_age_avgs %>% ggplot(aes(x = b_fs_age)) +
  geom_histogram(binwidth = 0.1, col="white", fill="gray") +
  ggtitle(paste("Plot #", toString(plot_inx), " - Distribution of Movie Age effect", sep = ""),
    subtitle="( \"full stars\" training set)") +
  xlab("Movie Age effect") + ylab("Count")

tmp_plot

rm(tmp_plot)

lm_fs_ba_predicted_test <- test_set_fs %>%
  left_join(training_fs_age_avgs, by = "movieId") %>%
  mutate(pred = training_mu_fs + b_fs_age) %>%
  .$pred

lm_fs_ba_rmse_test <- rmse(test_set_fs$rating, lm_fs_ba_predicted_test)
# 0.9696211

# validation
tmp <- edx_fs %>% group_by(movieId) %>% summarize(mean_age = mean(movie_age))

tmp$predicted_mean_rating <- myPoly(lm_ma_poly, tmp$mean_age)

edx_fs_age_avgs <- edx_fs %>%
  group_by(movieId) %>%
  summarize(b_fs_age = mean(rating - tmp$predicted_mean_rating))

lm_fs_ba_predicted_val <- validation_fs %>%
  left_join(edx_fs_age_avgs, by = "movieId") %>%
  mutate(pred = mu_fs + b_fs_age) %>%
  .$pred

lm_fs_ba_rmse_val <- rmse(validation_fs$rating, lm_fs_ba_predicted_val)
# 0.9685611

cat("The \"Movie Age effect\" model achieves an RMSE of",
    sprintf(lm_fs_ba_rmse_val, fmt = "%#.7f"), "on the \"full stars\" data sets")

all_rmse_results <- bind_rows(all_rmse_results, tibble(Model = "Movie Age effect",
                                                  `Data Sets` = "full stars",
                                                  RMSE = sprintf(lm_fs_ba_rmse_val, fmt = "%#.7f")))

rm(tmp, lm_fs_ba_predicted_test, lm_fs_ba_rmse_test, lm_fs_ba_predicted_val, lm_fs_ba_rmse_val)

################################################################################
# 2.5.3. "Movie Age effect" model and its RMSE on the "half stars" data sets

# Generate a temporary data set with the mean age of each movie
tmp <- training_set_hs %>% group_by(movieId) %>% summarize(mean_age = mean(movie_age))

# Compute the predictions with a polynomial function using the model's coefficients
tmp$predicted_mean_rating <- myPoly(lm_ma_poly, tmp$mean_age)

training_hs_age_avgs <- training_set_hs %>%
  group_by(movieId) %>%
  summarize(b_hs_age = mean(rating - tmp$predicted_mean_rating))

plot_inx <- plot_inx + 1

tmp_plot <- training_hs_age_avgs %>% ggplot(aes(x = b_hs_age)) +
  geom_histogram(binwidth = 0.1, col="white", fill="gray") +
  ggtitle(paste("Plot #", toString(plot_inx), " - Distribution of Movie Age effect", sep = ""),
    subtitle="( \"half stars\" training set)") +
  xlab("Movie Age effect") + ylab("Count")

tmp_plot

rm(tmp_plot)

lm_hs_ba_predicted_test <- test_set_hs %>%
  left_join(training_hs_age_avgs, by = "movieId") %>%
  mutate(pred = training_mu_hs + b_hs_age) %>%
  .$pred

lm_hs_ba_rmse_test <- rmse(test_set_hs$rating, lm_hs_ba_predicted_test)
# 0.9110703

# validation
tmp <- edx_hs %>% group_by(movieId) %>% summarize(mean_age = mean(movie_age))

tmp$predicted_mean_rating <- myPoly(lm_ma_poly, tmp$mean_age)

edx_hs_age_avgs <- edx_hs %>%
  group_by(movieId) %>%
  summarize(b_hs_age = mean(rating - tmp$predicted_mean_rating))

lm_hs_ba_predicted_val <- validation_hs %>%
  left_join(edx_hs_age_avgs, by = "movieId") %>%
  mutate(pred = mu_hs + b_hs_age) %>%
  .$pred

lm_hs_ba_rmse_val <- rmse(validation_hs$rating, lm_hs_ba_predicted_val)
# 0.9115944

cat("The \"Movie Age effect\" model achieves an RMSE of",
    sprintf(lm_hs_ba_rmse_val, fmt = "%#.7f"), "on the \"half stars\" data sets")

all_rmse_results <- bind_rows(all_rmse_results, tibble(Model = "Movie Age effect",
                                                       `Data Sets` = "half stars",
                                                       RMSE = sprintf(lm_hs_ba_rmse_val, fmt = "%#.7f")))

rm(tmp, lm_hs_ba_predicted_test, lm_hs_ba_rmse_test, lm_hs_ba_predicted_val, lm_hs_ba_rmse_val)

################################################################################
# 2.5.4. "Movie Age effect" models' RMSE on all three data sets

tmp <- all_rmse_results %>% filter(str_detect(Model, "Movie Age effect"))

table_inx <- table_inx + 1

tmp %>% kable(caption = paste("Table #", toString(table_inx),
                          " - Results of \"Movie Age effect\" models", sep = ""),
          col.names = c("Model", "Data Sets", "RMSE")) %>%
          row_spec(0, bold = TRUE) %>%
          kable_styling(position = "center", full_width = FALSE)

rm(tmp)
gc(verbose = my_gc_verbose, full = TRUE)

################################################################################
# 2.5.5. Next observations on Movie models' performance and conclusions

# Generate temporary results table without Naive models' results
tmp <- all_rmse_results %>% filter(!str_detect(Model, "Naive"))

table_inx <- table_inx + 1

tmp %>% kable(caption = paste("Table #", toString(table_inx),
                          " - Results of \"Movie\" and \"Movie Age effect\" models", sep = ""),
          col.names = c("Model", "Data Sets", "RMSE")) %>%
          row_spec(0, bold = TRUE) %>%
          kable_styling(position = "center", full_width = FALSE)

rm(tmp)

cat("Models, based on data after switch to \"half stars\" ratings, have again lower RMSEs
than those based on data before this switch or based on original data.\n")
cat("Modeling the \"Movie Age\" effect does not provide better results than modeling the \"Movie\" effect.\n")
cat("Models including the \"movie_age\" variable won't therefore be further investigated.\n")

# drop "movie_age" related elements to free some memory...
rm(lm_ma_poly)
rm(edx_age_avgs, edx_fs_age_avgs, edx_hs_age_avgs)
rm(training_age_avgs, training_fs_age_avgs, training_hs_age_avgs)

edx$movie_age <- NULL
training_set$movie_age <- NULL
test_set$movie_age <- NULL

edx_fs$movie_age <- NULL
training_set_fs$movie_age <- NULL
test_set_fs$movie_age <- NULL

edx_hs$movie_age <- NULL
training_set_hs$movie_age <- NULL
test_set_hs$movie_age <- NULL

validation$movie_age <- NULL
validation_fs$movie_age <- NULL
validation_hs$movie_age <- NULL

gc(verbose = my_gc_verbose, full = TRUE)

################################################################################
# 2.6. "Movie and User effects" models
################################################################################

################################################################################
# 2.6.1. "Movie and User effects" model and its RMSE on the original data sets

training_user_avgs <- training_set %>%
  left_join(training_movie_avgs, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_user = mean(rating - training_mu - b_movie))

plot_inx <- plot_inx + 1

tmp_plot <- training_user_avgs %>% ggplot(aes(x = b_user)) +
  geom_histogram(binwidth = 0.1, col="white", fill="gray") +
  ggtitle(paste("Plot #", toString(plot_inx), " - Distribution of User effect", sep = ""),
    subtitle="(original training set)") +
  xlab("User effect") + ylab("Count")

tmp_plot

rm(tmp_plot)

lm_bmu_predicted_test <- test_set %>%
  left_join(training_movie_avgs, by = "movieId") %>%
  left_join(training_user_avgs, by = "userId") %>%
  mutate(pred = training_mu + b_movie + b_user) %>%
  .$pred

lm_bmu_rmse_test <- rmse(test_set$rating, lm_bmu_predicted_test)
# 0.8654557

# Validation
edx_user_avgs <- edx %>%
  left_join(edx_movie_avgs, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_user = mean(rating - mu - b_movie))

lm_bmu_predicted_val <- validation %>%
  left_join(edx_movie_avgs, by = "movieId") %>%
  left_join(edx_user_avgs, by = "userId") %>%
  mutate(pred = mu + b_movie + b_user) %>%
  .$pred

lm_bmu_rmse_val <- rmse(validation$rating, lm_bmu_predicted_val)
# 0.8653498

cat("The \"Movie and User effects\" model achieves an RMSE of",
    sprintf(lm_bmu_rmse_val, fmt = "%#.7f"), "on the original data sets")

all_rmse_results <- bind_rows(all_rmse_results, tibble(Model = "Movie and User effects",
                                                  `Data Sets` = "original",
                                                  RMSE = sprintf(lm_bmu_rmse_val, fmt = "%#.7f")))

rm(training_movie_avgs, training_user_avgs,
   edx_movie_avgs, edx_user_avgs,
   lm_bmu_predicted_test, lm_bmu_rmse_test,
   lm_bmu_predicted_val, lm_bmu_rmse_val)

################################################################################
# 2.6.2. "Movie and User effects" model its RMSE on the "full stars" data sets

training_fs_user_avgs <- training_set_fs %>%
  left_join(training_fs_movie_avgs, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_fs_user = mean(rating - training_mu_fs - b_fs_movie))

plot_inx <- plot_inx + 1

tmp_plot <- training_fs_user_avgs %>% ggplot(aes(x = b_fs_user)) +
  geom_histogram(binwidth = 0.1, col="white", fill="gray") +
  ggtitle(paste("Plot #", toString(plot_inx), " - Distribution of User effect", sep = ""),
    subtitle="(\"full stars\" training set)") +
  xlab("User effect") + ylab("Count")

tmp_plot

rm(tmp_plot)

lm_fs_bmu_predicted_test <- test_set_fs %>%
  left_join(training_fs_movie_avgs, by = "movieId") %>%
  left_join(training_fs_user_avgs, by = "userId") %>%
  mutate(pred = training_mu_fs + b_fs_movie + b_fs_user) %>%
  .$pred

lm_fs_bmu_rmse_test <- rmse(test_set_fs$rating, lm_fs_bmu_predicted_test)
# 0.8992569

# Validation
edx_fs_user_avgs <- edx_fs %>%
  left_join(edx_fs_movie_avgs, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_fs_user = mean(rating - mu_fs - b_fs_movie))

lm_fs_bmu_predicted_val <- validation_fs %>%
  left_join(edx_fs_movie_avgs, by = "movieId") %>%
  left_join(edx_fs_user_avgs, by = "userId") %>%
  mutate(pred = mu_fs + b_fs_movie + b_fs_user) %>%
  .$pred

lm_fs_bmu_rmse_val <- rmse(validation_fs$rating, lm_fs_bmu_predicted_val)
# 0.8982483

cat("The \"Movie and User effects\" model achieves an RMSE of",
    sprintf(lm_fs_bmu_rmse_val, fmt = "%#.7f"), "on the \"full stars\" data sets")

all_rmse_results <- bind_rows(all_rmse_results, tibble(Model = "Movie and User effects",
                                                  `Data Sets` = "full stars",
                                                  RMSE = sprintf(lm_fs_bmu_rmse_val, fmt = "%#.7f")))

rm(training_fs_movie_avgs, training_fs_user_avgs,
   edx_fs_movie_avgs, edx_fs_user_avgs,
   lm_fs_bmu_predicted_test, lm_fs_bmu_rmse_test,
   lm_fs_bmu_predicted_val, lm_fs_bmu_rmse_val)

################################################################################
# 2.6.3. "Movie and User effects" model its RMSE on the "half stars" data sets

training_hs_user_avgs <- training_set_hs %>%
  left_join(training_hs_movie_avgs, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_hs_user = mean(rating - training_mu_hs - b_hs_movie))

plot_inx <- plot_inx + 1

tmp_plot <- training_hs_user_avgs %>% ggplot(aes(x = b_hs_user)) +
  geom_histogram(binwidth = 0.1, col="white", fill="gray") +
  ggtitle(paste("Plot #", toString(plot_inx), " - Distribution of User effect", sep = ""),
    subtitle="(\"half stars\" training set)") +
  xlab("User effect") + ylab("Count")

tmp_plot

rm(tmp_plot)

lm_hs_bmu_predicted_test <- test_set_hs %>%
  left_join(training_hs_movie_avgs, by = "movieId") %>%
  left_join(training_hs_user_avgs, by = "userId") %>%
  mutate(pred = training_mu_hs + b_hs_movie + b_hs_user) %>%
  .$pred

lm_hs_bmu_rmse_test <- rmse(test_set_hs$rating, lm_hs_bmu_predicted_test)
# 0.8204859

# Validation
edx_hs_user_avgs <- edx_hs %>%
  left_join(edx_hs_movie_avgs, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_hs_user = mean(rating - mu_hs - b_hs_movie))

lm_hs_bmu_predicted_val <- validation_hs %>%
  left_join(training_hs_movie_avgs, by = "movieId") %>%
  left_join(training_hs_user_avgs, by = "userId") %>%
  mutate(pred = mu_hs + b_hs_movie + b_hs_user) %>%
  .$pred

lm_hs_bmu_rmse_val <- rmse(validation_hs$rating, lm_hs_bmu_predicted_val)
# 0.8214449

cat("The \"Movie and User effects\" model achieves an RMSE of",
    sprintf(lm_hs_bmu_rmse_val, fmt = "%#.7f"), "on the \"half stars\" data sets")

all_rmse_results <- bind_rows(all_rmse_results, tibble(Model = "Movie and User effects",
                                                  `Data Sets` = "half stars",
                                                  RMSE = sprintf(lm_hs_bmu_rmse_val, fmt = "%#.7f")))

rm(training_hs_movie_avgs, training_hs_user_avgs,
   edx_hs_movie_avgs, edx_hs_user_avgs,
   lm_hs_bmu_predicted_test, lm_hs_bmu_rmse_test,
   lm_hs_bmu_predicted_val, lm_hs_bmu_rmse_val)

################################################################################
# 2.5.4. "Movie and User effects" models' RMSE on all three data sets
# and next observations on models' performance

tmp <- all_rmse_results %>% filter(str_detect(Model, "Movie and User effects"))

table_inx <- table_inx + 1

tmp %>% kable(caption = paste("Table #", toString(table_inx),
                          " - Results of \"Movie and User effects\" models", sep = ""),
          col.names = c("Model", "Data Sets", "RMSE")) %>%
          row_spec(0, bold = TRUE) %>%
          kable_styling(position = "center", full_width = FALSE)

rm(tmp)

cat("Once again, the model based on data after switch to \"half stars\" ratings, has a lower RMSE
than those based on data before this switch or based on original data.\n")
cat("The \"Movie and User\" effect model achieves an RMSE slightly under the 2nd goal RMSE value of ",
    sprintf(rmse_goal_2, fmt = "%#.7f"), ".\n", sep = "")
cat("Let's see if those results can be further improved by applying Regularization...\n")

# free up some memory...
gc(verbose = my_gc_verbose, full = TRUE)


################################################################################
# 2.7. "Regularized Movie and User effects" models
################################################################################

################################################################################
# 2.7.1. "Regularized Movie and User effects" model and its RMSE on the original data sets

# Search for optimal lambda
lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){
  reg_b_movie <- training_set %>% group_by(movieId) %>%
    summarize(reg_b_movie = sum(rating - training_mu)/(n()+l))
  
  reg_b_user <- training_set %>%
    left_join(reg_b_movie, by="movieId") %>%
    group_by(userId) %>%
    summarize(reg_b_user = sum(rating - reg_b_movie - training_mu)/(n()+l))
  
  predicted_ratings_test <- test_set %>%
    left_join(reg_b_movie, by = "movieId") %>%
    left_join(reg_b_user, by = "userId") %>%
    mutate(pred = training_mu + reg_b_movie + reg_b_user) %>% .$pred
  
  return(rmse(test_set$rating, predicted_ratings_test))
  }
)
lm_reg_bmu_rmse_test <- min(rmses)
lambda_test <- lambdas[which.min(rmses)]

plot_inx <- plot_inx + 1

tmp_plot <- tibble(lambda = lambdas, rmse = rmses) %>%
  ggplot(aes(x = lambda, y = rmse)) +
  geom_point() +
  geom_vline(xintercept = lambda_test) +
  geom_text(aes(x = lambda_test - 0.25, y = lm_reg_bmu_rmse_test + 0.0001,
                label = sprintf(lambda_test, fmt = "%#.2f"), angle = 90)) +
  geom_hline(yintercept = lm_reg_bmu_rmse_test) +
  geom_text(aes(x = 1.25, y = lm_reg_bmu_rmse_test + 0.00002,
                label = sprintf(lm_reg_bmu_rmse_test, fmt = "%#.7f"))) +
  ggtitle(paste("Plot #", toString(plot_inx), " - Regularized \"Movie and User\" effects model", sep = ""),
      subtitle="(Lambda optimization on original data sets - test run)") +
  xlab("Lambda") + ylab("RMSE")

tmp_plot

rm(tmp_plot)

cat("Test run: optimal regularization lambda:", sprintf(lambda_test, fmt = "%#.2f"),
  "with RMSE:" , sprintf(lm_reg_bmu_rmse_test, fmt = "%#.7f"), "\n")
# 4.75 and 0.8647818

# Validation
# Search for optimal lambda
lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){
  reg_b_movie <- edx %>% group_by(movieId) %>%
    summarize(reg_b_movie = sum(rating - mu)/(n()+l))
  
  reg_b_user <- edx %>%
    left_join(reg_b_movie, by="movieId") %>%
    group_by(userId) %>%
    summarize(reg_b_user = sum(rating - reg_b_movie - mu)/(n()+l))
  
  predicted_ratings_val <- validation %>%
    left_join(reg_b_movie, by = "movieId") %>%
    left_join(reg_b_user, by = "userId") %>%
    mutate(pred = mu + reg_b_movie + reg_b_user) %>% .$pred
  
  return(rmse(validation$rating, predicted_ratings_val))
  }
)
lm_reg_bmu_rmse_val <- min(rmses)
lambda_val <- lambdas[which.min(rmses)]

plot_inx <- plot_inx + 1
  
tmp_plot <- tibble(lambda = lambdas, rmse = rmses) %>%
  ggplot(aes(x = lambda, y = rmse)) +
  geom_point() +
  geom_vline(xintercept = lambda_val) +
  geom_text(aes(x = lambda_val - 0.25, y = lm_reg_bmu_rmse_val + 0.0001,
                label = sprintf(lambda_val, fmt = "%#.2f"), angle = 90)) +
  geom_hline(yintercept = lm_reg_bmu_rmse_val) +
  geom_text(aes(x = 1.25, y = lm_reg_bmu_rmse_val + 0.00002,
                label = sprintf(lm_reg_bmu_rmse_val, fmt = "%#.7f"))) +
  ggtitle(paste("Plot #", toString(plot_inx), " - Regularized \"Movie and User\" effects model", sep = ""),
          subtitle="(Lambda optimization on original data sets - validation run)") +
  xlab("Lambda") + ylab("RMSE")

tmp_plot
rm(tmp_plot)

cat("Validation run: optimal regularization lambda:", sprintf(lambda_val, fmt = "%#.2f"),
    "with RMSE:" , sprintf(lm_reg_bmu_rmse_val, fmt = "%#.7f"), "\n")
# 5.25 and 0.8648181

cat("The \"Regularized Movie and User effects\" model achieves an RMSE of",
    sprintf(lm_reg_bmu_rmse_val, fmt = "%#.7f"), "on the original data sets.")

all_rmse_results <- bind_rows(all_rmse_results, tibble(Model = "Regularized Movie and User effects",
                                                  `Data Sets` = "original",
                                                  RMSE = sprintf(lm_reg_bmu_rmse_val, fmt = "%#.7f")))

rm(lambdas, rmses, mu)
rm(lambda_test, lm_reg_bmu_rmse_test, lambda_val, lm_reg_bmu_rmse_val)
gc(verbose = TRUE, full = TRUE)

################################################################################
# 2.7.2. "Regularized Movie and User effects" model and its RMSE on the "full stars" data sets

# Search for optimal lambda_fs
lambdas_fs <- seq(0, 10, 0.25)

rmses_fs <- sapply(lambdas_fs, function(l){
  reg_fs_b_movie <- training_set_fs %>% group_by(movieId) %>%
    summarize(reg_fs_b_movie = sum(rating - training_mu_fs)/(n()+l))
  
  reg_fs_b_user <- training_set_fs %>%
    left_join(reg_fs_b_movie, by="movieId") %>%
    group_by(userId) %>%
    summarize(reg_fs_b_user = sum(rating - reg_fs_b_movie - training_mu_fs)/(n()+l))
  
  predicted_fs_ratings_test <- test_set_fs %>%
    left_join(reg_fs_b_movie, by = "movieId") %>%
    left_join(reg_fs_b_user, by = "userId") %>%
    mutate(pred_fs = training_mu_fs + reg_fs_b_movie + reg_fs_b_user) %>%
    .$pred_fs
  
  return(rmse(test_set_fs$rating, predicted_fs_ratings_test))
  }
)
lm_reg_fs_bmu_rmse_test <- min(rmses_fs)
lambda_fs_test <- lambdas_fs[which.min(rmses_fs)]

plot_inx <- plot_inx + 1

tmp_plot <- tibble(lambda = lambdas_fs, rmse = rmses_fs) %>%
  ggplot(aes(x = lambda, y = rmse)) +
  geom_point() +
  geom_vline(xintercept = lambda_fs_test) +
  geom_text(aes(x = lambda_fs_test - 0.25, y = lm_reg_fs_bmu_rmse_test + 0.0001,
                label = sprintf(lambda_fs_test, fmt = "%#.2f"), angle = 90)) +
  geom_hline(yintercept = lm_reg_fs_bmu_rmse_test) +
  geom_text(aes(x = 1.25, y = lm_reg_fs_bmu_rmse_test + 0.00002,
                label = sprintf(lm_reg_fs_bmu_rmse_test, fmt = "%#.7f"))) +
  ggtitle(paste("Plot #", toString(plot_inx), " - Regularized \"Movie and User\" effects model", sep = ""),
          subtitle="(Lambda optimization on \"full stars\" data sets - test run)") +
  xlab("Lambda") + ylab("RMSE")

tmp_plot

rm(tmp_plot)

cat("Test run: optimal regularization lambda:", sprintf(lambda_fs_test, fmt = "%#.2f"),
    "with RMSE:" , sprintf(lm_reg_fs_bmu_rmse_test, fmt = "%#.7f"), "\n")
# 5.00 and 0.8984171

# Validation
# Search for optimal lambda_fs
lambdas_fs <- seq(0, 10, 0.25)

rmses_fs <- sapply(lambdas_fs, function(l){
  reg_fs_b_movie <- edx_fs %>% group_by(movieId) %>%
    summarize(reg_fs_b_movie = sum(rating - mu_fs)/(n()+l))
  
  reg_fs_b_user <- edx_fs %>%
    left_join(reg_fs_b_movie, by="movieId") %>%
    group_by(userId) %>%
    summarize(reg_fs_b_user = sum(rating - reg_fs_b_movie - mu_fs)/(n()+l))
  
  predicted_fs_ratings_val <- validation_fs %>%
    left_join(reg_fs_b_movie, by = "movieId") %>%
    left_join(reg_fs_b_user, by = "userId") %>%
    mutate(pred_fs = mu_fs + reg_fs_b_movie + reg_fs_b_user) %>%
    .$pred_fs
  
  return(rmse(validation_fs$rating, predicted_fs_ratings_val))
  }
)
lm_reg_fs_bmu_rmse_val <- min(rmses_fs)
lambda_fs_val <- lambdas_fs[which.min(rmses_fs)]

plot_inx <- plot_inx + 1

tmp_plot <- tibble(lambda = lambdas_fs, rmse = rmses_fs) %>%
  ggplot(aes(x = lambda, y = rmse)) +
  geom_point() +
  geom_vline(xintercept = lambda_fs_val) +
  geom_text(aes(x = lambda_fs_val - 0.25, y = lm_reg_fs_bmu_rmse_val + 0.0001,
                label = sprintf(lambda_fs_val, fmt = "%#.2f"), angle = 90)) +
  geom_hline(yintercept = lm_reg_fs_bmu_rmse_val) +
  geom_text(aes(x = 1.25, y = lm_reg_fs_bmu_rmse_val + 0.00002,
                label = sprintf(lm_reg_fs_bmu_rmse_val, fmt = "%#.7f"))) +
  ggtitle(paste("Plot #", toString(plot_inx), " - Regularized \"Movie and User\" effects model", sep = ""),
          subtitle="(Lambda optimization on \"full stars\" data sets - validation run)") +
  xlab("Lambda") + ylab("RMSE")

tmp_plot
rm(tmp_plot)

cat("Validation run: optimal regularization lambda:", sprintf(lambda_fs_val, fmt = "%#.2f"),
    "with RMSE:" , sprintf(lm_reg_fs_bmu_rmse_val, fmt = "%#.7f"), "\n")
# 5.75 and 0.8975536

cat("The \"Regularized Movie and User effects\" model achieves an RMSE of",
    sprintf(lm_reg_fs_bmu_rmse_val, fmt = "%#.7f"), "on the \"full stars\" data sets.")

all_rmse_results <- bind_rows(all_rmse_results, tibble(Model = "Regularized Movie and User effects",
                                                       `Data Sets` = "full stars",
                                                       RMSE = sprintf(lm_reg_fs_bmu_rmse_val, fmt = "%#.7f")))

rm(lambdas_fs, rmses_fs, mu_fs)
rm(lambda_fs_test, lm_reg_fs_bmu_rmse_test, lambda_fs_val, lm_reg_fs_bmu_rmse_val)
gc(verbose = TRUE, full = TRUE)

################################################################################
# 2.7.3. "Regularized Movie and User effects" model and its RMSE on the "half stars" data sets

# Search for optimal lambda_hs
lambdas_hs <- seq(0, 10, 0.25)

rmses_hs <- sapply(lambdas_hs, function(l){
  reg_hs_b_movie <- training_set_hs %>% group_by(movieId) %>%
    summarize(reg_hs_b_movie = sum(rating - training_mu_hs)/(n()+l))
  
  reg_hs_b_user <- training_set_hs %>%
    left_join(reg_hs_b_movie, by="movieId") %>%
    group_by(userId) %>%
    summarize(reg_hs_b_user = sum(rating - reg_hs_b_movie - training_mu_hs)/(n()+l))
  
  predicted_hs_ratings_test <- test_set_hs %>%
    left_join(reg_hs_b_movie, by = "movieId") %>%
    left_join(reg_hs_b_user, by = "userId") %>%
    mutate(pred_hs = training_mu_hs + reg_hs_b_movie + reg_hs_b_user) %>%
    .$pred_hs
  
  return(rmse(test_set_hs$rating, predicted_hs_ratings_test))
  }
)
lm_reg_hs_bmu_rmse_test <- min(rmses_hs)
lambda_hs_test <- lambdas_hs[which.min(rmses_hs)]

plot_inx <- plot_inx + 1

tmp_plot <- tibble(lambda = lambdas_hs, rmse = rmses_hs) %>%
  ggplot(aes(x = lambda, y = rmse)) +
  geom_point() +
  geom_vline(xintercept = lambda_hs_test) +
  geom_text(aes(x = lambda_hs_test - 0.25, y = lm_reg_hs_bmu_rmse_test + 0.0001,
                label = sprintf(lambda_hs_test, fmt = "%#.2f"), angle = 90)) +
  geom_hline(yintercept = lm_reg_hs_bmu_rmse_test) +
  geom_text(aes(x = 1.25, y = lm_reg_hs_bmu_rmse_test + 0.00002,
                label = sprintf(lm_reg_hs_bmu_rmse_test, fmt = "%#.7f"))) +
  ggtitle(paste("Plot #", toString(plot_inx), " - Regularized \"Movie and User\" effects model", sep = ""),
          subtitle="(Lambda optimization on \"half stars\" data sets - test run)") +
  xlab("Lambda") + ylab("RMSE")

tmp_plot

rm(tmp_plot)

cat("Test run: optimal regularization lambda:", sprintf(lambda_hs_test, fmt = "%#.2f"),
    "with RMSE:" , sprintf(lm_reg_hs_bmu_rmse_test, fmt = "%#.7f"), "\n")
# 3.75 and 0.8198379

# Validation
# Search for optimal lambda_hs
lambdas_hs <- seq(0, 10, 0.25)

rmses_hs <- sapply(lambdas_hs, function(l){
  reg_hs_b_movie <- edx_hs %>% group_by(movieId) %>%
    summarize(reg_hs_b_movie = sum(rating - mu_hs)/(n()+l))
  
  reg_hs_b_user <- edx_hs %>%
    left_join(reg_hs_b_movie, by="movieId") %>%
    group_by(userId) %>%
    summarize(reg_hs_b_user = sum(rating - reg_hs_b_movie - mu_hs)/(n()+l))
  
  predicted_hs_ratings_val <- validation_hs %>%
    left_join(reg_hs_b_movie, by = "movieId") %>%
    left_join(reg_hs_b_user, by = "userId") %>%
    mutate(pred_hs = mu_hs + reg_hs_b_movie + reg_hs_b_user) %>%
    .$pred_hs
  
  return(rmse(validation_hs$rating, predicted_hs_ratings_val))
  }
)
lm_reg_hs_bmu_rmse_val <- min(rmses_hs)
lambda_hs_val <- lambdas_hs[which.min(rmses_hs)]

plot_inx <- plot_inx + 1

tmp_plot <- tibble(lambda = lambdas_hs, rmse = rmses_hs) %>%
  ggplot(aes(x = lambda, y = rmse)) +
  geom_point() +
  geom_vline(xintercept = lambda_hs_val) +
  geom_text(aes(x = lambda_hs_val - 0.25, y = lm_reg_hs_bmu_rmse_val + 0.0001,
                label = sprintf(lambda_hs_val, fmt = "%#.2f"), angle = 90)) +
  geom_hline(yintercept = lm_reg_hs_bmu_rmse_val) +
  geom_text(aes(x = 1.25, y = lm_reg_hs_bmu_rmse_val + 0.00002,
              label = sprintf(lm_reg_hs_bmu_rmse_val, fmt = "%#.7f"))) +
  ggtitle(paste("Plot #", toString(plot_inx), " - Regularized \"Movie and User\" effects model", sep = ""),
          subtitle="(Lambda optimization on \"half stars\" data sets - validation run)") +
  xlab("Lambda") + ylab("RMSE")

tmp_plot
rm(tmp_plot)

cat("Validation run: optimal regularization lambda:", sprintf(lambda_hs_val, fmt = "%#.2f"),
    "with RMSE:" , sprintf(lm_reg_hs_bmu_rmse_val, fmt = "%#.7f"), "\n")
# 4.00 and 0.8198425

cat("The \"Regularized Movie and User effects\" model achieves an RMSE of",
    sprintf(lm_reg_hs_bmu_rmse_val, fmt = "%#.7f"), "on the \"half stars\" data sets.")

all_rmse_results <- bind_rows(all_rmse_results, tibble(Model = "Regularized Movie and User effects",
                                                       `Data Sets` = "half stars",
                                                       RMSE = sprintf(lm_reg_hs_bmu_rmse_val, fmt = "%#.7f")))

rm(lambdas_hs, rmses_hs, mu_hs)
rm(lambda_hs_test, lm_reg_hs_bmu_rmse_test, lambda_hs_val, lm_reg_hs_bmu_rmse_val)
gc(verbose = TRUE, full = TRUE)

################################################################################
# 2.7.4. "Regularized Movie and User effects" models' RMSE on all three data sets
# and next observations on models' performance

tmp <- all_rmse_results %>% filter(str_detect(Model, "Regularized Movie and User effects"))

table_inx <- table_inx + 1

tmp %>% kable(caption = paste("Table #", toString(table_inx),
                          " - Results of \"Regularized Movie and User effects\" models", sep = ""),
          col.names = c("Model", "Data Sets", "RMSE")) %>%
          row_spec(0, bold = TRUE) %>%
          kable_styling(position = "center", full_width = FALSE)

rm(tmp)

cat("As expected, the model based on data after the switch to \"half stars\" ratings, has a lower RMSE
than those based on data before this switch or based on original data.\n")
cat("The \"Regularized Movie and User effects\" model achieves an RMSE slightly slightly better than 
the 4th and final goal RMSE value of", sprintf(rmse_goal_4, fmt = "%#.7f"),
"on both the original data sets (as required) and the \"half stars\" data sets.\n")
cat("That would be good enough, but let's see if those results can be further improved by Matrix Factorization models...\n")

# free up some memory...
gc(verbose = my_gc_verbose, full = TRUE)

################################################################################
# 2.8. "Matrix Factorization" models
################################################################################
# following 1) https://cran.r-project.org/web/packages/recosystem/vignettes/introduction.html
# following 2) https://cran.r-project.org/web/packages/recosystem/recosystem.pdf

if(!require(recosystem)) install.packages("recosystem", repos = "http://cran.us.r-project.org")
library(recosystem)

################################################################################
# 2.8.1. Generation of training, test and validation rating matrices for all data sets

# Generate training, test and validation rating matrices based on observed values
# Referring to 1), "Each cell with number in it is the rating given by some user on a specific item"
# The data file for training set needs to be arranged in sparse matrix triplet form,
# i.e., each line in the file contains three numbers: "user_index", "item_index", and "rating"

edx_set_factorization <- edx %>% select(userId, movieId, rating)
write.table(edx_set_factorization, file = "C:\\tmp\\edx_set.txt", sep = " ",
  row.names = FALSE, col.names = FALSE)

edx_set_fs_factorization <- edx_fs %>% select(userId, movieId, rating)
write.table(edx_set_fs_factorization, file = "C:\\tmp\\edx_set_fs.txt", sep = " ",
  row.names = FALSE, col.names = FALSE)

edx_set_hs_factorization <- edx_hs %>% select(userId, movieId, rating)
write.table(edx_set_hs_factorization, file = "C:\\tmp\\edx_set_hs.txt", sep = " ",
  row.names = FALSE, col.names = FALSE)


#training_set_factorization <- training_set %>% select(userId, movieId, rating)
#write.table(training_set_factorization, file = "C:\\tmp\\training_set.txt", sep = " ",
#            row.names = FALSE, col.names = FALSE)

#training_set_fs_factorization <- training_set_fs %>% select(userId, movieId, rating)
#write.table(training_set_fs_factorization, file = "C:\\tmp\\training_set_fs.txt", sep = " ",
#            row.names = FALSE, col.names = FALSE)

#training_set_hs_factorization <- training_set_hs %>% select(userId, movieId, rating)
#write.table(training_set_hs_factorization, file = "C:\\tmp\\training_set_hs.txt", sep = " ",
#            row.names = FALSE, col.names = FALSE)


#test_set_factorization <- test_set %>% select(userId, movieId, rating)
#write.table(test_set_factorization, file = "C:\\tmp\\test_set.txt", sep = " ",
#            row.names = FALSE, col.names = FALSE)

#test_set_fs_factorization <- test_set_fs %>% select(userId, movieId, rating)
#write.table(test_set_fs_factorization, file = "C:\\tmp\\test_set_fs.txt", sep = " ",
#            row.names = FALSE, col.names = FALSE)

#test_set_hs_factorization <- test_set_hs %>% select(userId, movieId, rating)
#write.table(test_set_hs_factorization, file = "C:\\tmp\\test_set_hs.txt", sep = " ",
#            row.names = FALSE, col.names = FALSE)

# The original, "full stars" and "half stars" data sets are no more needed
rm(edx, edx_fs, edx_hs)

# The split training and test sets are also no more needed.
rm(training_set, training_set_fs, training_set_hs)
rm(test_set, test_set_fs, test_set_hs)

# The intermediate *_factorization are also no more needed
rm(edx_set_factorization, edx_set_fs_factorization, edx_set_hs_factorization)
#rm(training_set_factorization, training_set_fs_factorization, training_set_hs_factorization)
#rm(test_set_factorization, test_set_fs_factorization, test_set_hs_factorization)
gc(verbose = TRUE, full = TRUE)


# Do the same for the validation rating matrices 
validation_factorization <- validation %>% select(userId, movieId, rating)
write.table(validation_factorization, file = "C:\\tmp\\validation_set.txt", sep = " ",
  row.names = FALSE, col.names = FALSE)

validation_fs_factorization <- validation_fs %>% select(userId, movieId, rating)
write.table(validation_fs_factorization, file = "C:\\tmp\\validation_set_fs.txt", sep = " ",
  row.names = FALSE, col.names = FALSE)

validation_hs_factorization <- validation_hs %>% select(userId, movieId, rating)
write.table(validation_hs_factorization, file = "C:\\tmp\\validation_set_hs.txt", sep = " ",
  row.names = FALSE, col.names = FALSE)

rm(validation, validation_fs, validation_hs)
rm(validation_factorization, validation_fs_factorization, validation_hs_factorization)
gc(verbose = TRUE, full = TRUE)

# Initialize the training, test and validation sets
edx_set <- data_file("C:\\tmp\\edx_set.txt", package = "recosystem")
edx_set_fs <- data_file("C:\\tmp\\edx_set_fs.txt", package = "recosystem")
edx_set_hs <- data_file("C:\\tmp\\edx_set_hs.txt", package = "recosystem")

#training_set <- data_file("C:\\tmp\\training_set.txt", package = "recosystem")
#training_set_fs <- data_file("C:\\tmp\\training_set_fs.txt", package = "recosystem")
#training_set_hs <- data_file("C:\\tmp\\training_set_hs.txt", package = "recosystem")

#test_set <- data_file("C:\\tmp\\test_set.txt", package = "recosystem")
#test_set_fs <- data_file("C:\\tmp\\test_set_fs.txt", package = "recosystem")
#test_set_hs <- data_file("C:\\tmp\\test_set_hs.txt", package = "recosystem")

validation_set <- data_file("C:\\tmp\\validation_set.txt", package = "recosystem")
validation_set_fs <- data_file("C:\\tmp\\validation_set_fs.txt", package = "recosystem")
validation_set_hs <- data_file("C:\\tmp\\validation_set_hs.txt", package = "recosystem")

# Extract the true ratings from the test set and validation set files
#true_ratings_test <- read.table("C:\\tmp\\test_set.txt", header = FALSE, sep = "")$V3
#true_ratings_fs_test <- read.table("C:\\tmp\\test_set_fs.txt", header = FALSE, sep = "")$V3
#true_ratings_hs_test <- read.table("C:\\tmp\\test_set_hs.txt", header = FALSE, sep = "")$V3

true_ratings_val <- read.table("C:\\tmp\\validation_set.txt", header = FALSE, sep = "")$V3
true_ratings_fs_val <- read.table("C:\\tmp\\validation_set_fs.txt", header = FALSE, sep = "")$V3
true_ratings_hs_val <- read.table("C:\\tmp\\validation_set_hs.txt", header = FALSE, sep = "")$V3

################################################################################
# 2.8.2 "Matrix factorization" model and its RMSE on the original data sets
# 
# IN ORDER TO AVOID VERY LONG COMPUTE TIME(s) aND BECAUSE TUNING DEPENS ON
# DATA SET SPECIFIC RATING MATRICES, THE MODEL IS RUN ONLY ON THE ORIGINAL
# "EDX" AND "VALIDATION" DATA SETS

# Create a model object
r = Reco()

# Select the best tuning parameters
#
# Note: This script is developed on a PC running MS Windows 10 Home (version 20H2)
# on a 4 cores/8 threads Intel i7-4770K CPU ("K" but wo. overclocking) with 32GB RAM
# Experience has proven to be "safe" to use max. 6 threads (i.e. max. 3 of 4 CPU cores)

# Tuning run #1 - on training and test data sets

# The initial tuning parameters are according to 1),
# with more threads (4 instead of 1) and nmf=TRUE (all values are positive)
# Per default, the loss function is "l2", i.e. squared error
# Per default, the "l1" loss function parameters are set to 0, however,
# I recommend to keep them out of the optimization run
# by setting them explicitly to 0 (I don't know why, that spares a lot of run time...)
# r$tune shall find best values for:
# - dim: the number of latent factors
# - costp_l2: L2 regularization parameter for "user" factors
# - costq_l2: L2 regularization parameter for "item" (i.e. "movie") factors
# - lrate: learning rate (can be thought of as the step size in gradient descent)
# The number of iterations is reduced to 10 (default: 20), due to very long run time even with 4 threads

set.seed(my_seed, sample.kind="Rounding")
opts1 = r$tune(edx_set, opts = list(dim = c(10, 20, 30), lrate = c(0.1, 0.2),
                                      costp_l1 = 0, costq_l1 = 0,
                                      nthread = 6, niter = 10, nmf = TRUE))
opts1

cat("Tuning run #1, loss function minimum:",
  sprintf(opts1$min$loss_fun, fmt = "%#.7f"), "\n")
# loss_fun: 0.8065943

# Train the model with the best tuning parameters from the 1st r$tune() run
# More than 1 thread only decrease run time at the end of training, when results are shown...
# From experience, performance gains after 50 iterations are very marginal.

set.seed(my_seed, sample.kind="Rounding")
r$train(edx_set, opts = c(opts1$min, nthread = 6, niter = 50, nmf = TRUE))
# tr_rmse1[49] 0.7041

predictions1 = r$predict(validation_set, out_memory())

mf_rmse1 <- rmse(true_ratings_val, predictions1)
# opts1, mf_rmse1: 0.7862522

all_rmse_results <- bind_rows(all_rmse_results,
                      tibble(Model = "Matrix Factorization, tuning run #1",
                        `Data Sets` = "original",
                        RMSE = sprintf(mf_rmse1, fmt = "%#.7f")))

# Tuning run #2

# With this 2nd optimization run, we narrow the grid around the best opts1 values for dim and lrate

opt_dims <- c(opts1$min$dim-5, opts1$min$dim, opts1$min$dim+5)
opt_lrates <- c(opts1$min$lrate-0.05, opts1$min$lrate, opts1$min$lrate+0.05)

set.seed(my_seed, sample.kind="Rounding")
opts2 = r$tune(edx_set, opts = list(dim = opt_dims, lrate = opt_lrates,
                                      costp_l1 = 0, costq_l1 = 0,
                                      nthread = 6, niter = 10, nmf = TRUE))
opts2

cat("Tuning run #2, loss function minimum:",
  sprintf(opts2$min$loss_fun, fmt = "%#.7f"), "\n")
# loss_fun: 0.8060519

set.seed(my_seed, sample.kind="Rounding")
r$train(edx_set, opts = c(opts2$min, nthread = 6, niter = 50, nmf = TRUE))
# tr_rmse2[49] 0.7046

predictions2 = r$predict(validation_set, out_memory())

mf_rmse2 <- rmse(true_ratings_val, predictions2)
# opts2, mf_rmse2: 0.7850665

all_rmse_results <- bind_rows(all_rmse_results, tibble(Model = "Matrix Factorization, tuning run #2",
                                                  `Data Sets` = "original",
                                                  RMSE = sprintf(mf_rmse2, fmt = "%#.7f")))

rm(r, opts1, opts2, opt_dims, opt_lrates, predictions1, predictions2)
rm(edx_set, validation_set, true_ratings_val)

# "Matrix factorization" model's RMSE on original data sets after 2 tuning runs

# Generate "Matrix factorization" results table
tmp <- all_rmse_results %>% filter(str_detect(Model, "Matrix Factorization") &
                                     str_detect(`Data Sets`, "original"))

table_inx <- table_inx + 1

tmp %>% kable(caption = paste("Table #", toString(table_inx),
                          " - Results of \"Matrix factorization\" models on original data sets", sep = ""),
          col.names = c("Model", "Data Sets", "RMSE")) %>%
          row_spec(0, bold = TRUE) %>%
          kable_styling(position = "center", full_width = FALSE)

cat("Matrix Factorization allowed to substantially reduce RMSE.\n")
cat("The best resulting RMSE (", sprintf(min(mf_rmse1, mf_rmse2), fmt = "%#.7f"),
  ") is quite better than the final goal of this exercise (RMSE < ",
  sprintf(rmse_goal_4, fmt = "%#.7f"), ")\n", sep = "")
cat("However, this is at the cost of a very long processing time...")

rm(tmp, mf_rmse1, mf_rmse2)
gc(verbose = my_gc_verbose, full = TRUE)

################################################################################
# 2.8.3. "Matrix factorization" model and its RMSE on the "full stars" data sets
# The procedure is obviously the same as with the original data sets,
# therefore, this code section will have far less comments...

# Create a model object
r_fs = Reco()

# Tuning run fs#1
set.seed(my_seed, sample.kind="Rounding")
opts_fs1 = r_fs$tune(edx_set_fs, opts = list(dim = c(10, 20, 30), lrate = c(0.1, 0.2),
                                              costp_l1 = 0, costq_l1 = 0,
                                              nthread = 6, niter = 10, nmf = TRUE))
opts_fs1
cat("Tuning run fs#1, loss function minimum:", sprintf(opts_fs1$min$loss_fun, fmt = "%#.7f"), "\n")
# loss_fun: 0.8474413

set.seed(my_seed, sample.kind="Rounding")
r_fs$train(edx_set_fs, opts = c(opts_fs1$min, nthread = 6, niter = 50, nmf = TRUE))
# tr_rmse1[49] 0.7466

predictions_fs1 = r_fs$predict(validation_set_fs, out_memory())

mf_fs_rmse1 <- rmse(true_ratings_fs_val, predictions_fs1)
# opts_fs1, mf_fs_rmse1: 0.8271299

all_rmse_results <- bind_rows(all_rmse_results, tibble(Model = "Matrix Factorization, tuning run #1",
                                                  `Data Sets` = "full stars",
                                                  RMSE = sprintf(mf_fs_rmse1, fmt = "%#.7f")))
# Tuning run #2
opt_fs_dims <- c(opts_fs1$min$dim-5, opts_fs1$min$dim, opts_fs1$min$dim+5)
opt_fs_lrates <- c(opts_fs1$min$lrate-0.05, opts_fs1$min$lrate, opts_fs1$min$lrate+0.05)

set.seed(my_seed, sample.kind="Rounding")
opts_fs2 = r_fs$tune(edx_set_fs, opts = list(dim = opt_fs_dims, lrate = opt_fs_lrates,
                                              costp_l1 = 0, costq_l1 = 0,
                                              nthread = 6, niter = 10, nmf = TRUE))
opts_fs2
cat("Tuning run fs#2, loss function minimum:", sprintf(opts_fs2$min$loss_fun, fmt = "%#.7f"), "\n")
# loss_fun: 0.8457846

set.seed(my_seed, sample.kind="Rounding")
r_fs$train(edx_set_fs, opts = c(opts_fs2$min, nthread = 6, niter = 50, nmf = TRUE))
#tr_rmse2[49] 0.7347

predictions_fs2 = r_fs$predict(validation_set_fs, out_memory())

mf_fs_rmse2 <- rmse(true_ratings_fs_val, predictions_fs2)
# opts_fs2, mf_fs_rmse2: 0.8260818

all_rmse_results <- bind_rows(all_rmse_results, tibble(Model = "Matrix Factorization, tuning run #2",
                                                  `Data Sets` = "full stars",
                                                  RMSE = sprintf(mf_fs_rmse2, fmt = "%#.7f")))

rm(r_fs, opts_fs1, opts_fs2, opt_fs_dims, opt_fs_lrates, predictions_fs1, predictions_fs2)
rm(edx_set_fs, validation_set_fs, true_ratings_fs_val)
rm(mf_fs_rmse1, mf_fs_rmse2)
gc(verbose = my_gc_verbose, full = TRUE)


################################################################################
# 2.8.4. "Matrix factorization" model and its RMSE on the "half stars" data sets
# The procedure is obviously the same as before...

# Create a model object
r_hs = Reco()

# Tuning run hs#1
set.seed(my_seed, sample.kind="Rounding")
opts_hs1 = r_hs$tune(edx_set_hs, opts = list(dim = c(10, 20, 30), lrate = c(0.1, 0.2),
                                                  costp_l1 = 0, costq_l1 = 0,
                                                  nthread = 6, niter = 10, nmf = TRUE))
opts_hs1
cat("Tuning run hs#1, loss function minimum:", sprintf(opts_hs1$min$loss_fun, fmt = "%#.7f"), "\n")
# loss_fun: 0.7638150

set.seed(my_seed, sample.kind="Rounding")
r_hs$train(edx_set_hs, opts = c(opts_hs1$min, nthread = 6, niter = 50, nmf = TRUE))
# tr_rmse1[49] 0.6409

predictions_hs1 = r_hs$predict(validation_set_hs, out_memory())

mf_hs_rmse1 <- rmse(true_ratings_hs_val, predictions_hs1)
# opts_hs1, mf_hs_rmse1: 0.7500847

all_rmse_results <- bind_rows(all_rmse_results, tibble(Model = "Matrix Factorization, tuning run #1",
                                                  `Data Sets` = "half stars",
                                                  RMSE = sprintf(mf_hs_rmse1, fmt = "%#.7f")))
# Tuning run #2
opt_hs_dims <- c(opts_hs1$min$dim-5, opts_hs1$min$dim, opts_hs1$min$dim+5)
opt_hs_lrates <- c(opts_hs1$min$lrate-0.05, opts_hs1$min$lrate, opts_hs1$min$lrate+0.05)

set.seed(my_seed, sample.kind="Rounding")
opts_hs2 = r_hs$tune(edx_set_hs, opts = list(dim = opt_hs_dims, lrate = opt_hs_lrates,
                                                  costp_l1 = 0, costq_l1 = 0,
                                                  nthread = 6, niter = 10, nmf = TRUE))
opts_hs2
cat("Tuning run hs#2, loss function minimum:", sprintf(opts_hs2$min$loss_fun, fmt = "%#.7f"), "\n")
# loss_fun: 0.7620662

set.seed(my_seed, sample.kind="Rounding")
r_hs$train(edx_set_hs, opts = c(opts_hs2$min, nthread = 6, niter = 50, nmf = TRUE))
#tr_rmse2[49] 0.6276

predictions_hs2 = r_hs$predict(validation_set_hs, out_memory())

mf_hs_rmse2 <- rmse(true_ratings_hs_val, predictions_hs2)
# opts_hs2, mf_hs_rmse2: 0.7512074

all_rmse_results <- bind_rows(all_rmse_results, tibble(Model = "Matrix Factorization, tuning run #2",
                                                  `Data Sets` = "half stars",
                                                  RMSE = sprintf(mf_hs_rmse2, fmt = "%#.7f")))

rm(r_hs, opts_hs1, opts_hs2, opt_hs_dims, opt_hs_lrates, predictions_hs1, predictions_hs2)
rm(edx_set_hs, validation_set_hs, true_ratings_hs_val)
rm(mf_hs_rmse1, mf_hs_rmse2)
gc(verbose = my_gc_verbose, full = TRUE)

################################################################################
# 2.8.5. Results of "Matrix Factorization" models on "full stars" and "half stars" data sets

tmp <- all_rmse_results %>% filter(str_detect(Model, "Matrix Factorization") &
                                     !str_detect(`Data Sets`, "original"))
table_inx <- table_inx + 1

tmp %>% kable(caption = paste("Table #", toString(table_inx),
                          " - Results of \"Matrix factorization\" models on +
                          \"full stars\" + and \"half stars\" data sets", sep = ""),
          col.names = c("Model", "Data Sets", "RMSE")) %>%
          row_spec(0, bold = TRUE) %>%
          kable_styling(position = "center", full_width = FALSE)

rm(tmp)


################################################################################
################################################################################
# 3. RESULTS
#
################################################################################
################################################################################

# Summary of model results on original data sets
tmp <- all_rmse_results %>% filter(str_detect(`Data Sets`, "original")) %>% select(-`Data Sets`)

table_inx <- table_inx + 1

tmp %>% kable(caption = paste("Table #", toString(table_inx),
                          " - Results of all models on original data sets", sep = ""),
          col.names = c("Model", "RMSE")) %>%
          row_spec(0, bold = TRUE) %>%
          kable_styling(position = "center", full_width = FALSE)

rm(tmp)

cat("The RMSE goal of less than", sprintf(rmse_goal_4, fmt = "%#.7f"),
"is achieved with the \"Regularized Movie and User effects\" model and outperformed 
by the \"Matrix Factorization\" model, this however at the cost of a very long processing time.\n")

cat("The original data sets contain ratings done on a \"full stars\" scale until ",
format(edx_date_start_hp, "%B %d, %Y"),"\n",
"and a \"half stars\" scale after this date. It seemed therefore appropriate to test the models'
performance not only on the original data sets, but also on data sets before / after this
change in the ratings scale.\n", sep = "")

tmp1 <- all_rmse_results %>% filter(str_detect(`Data Sets`, "full stars")) %>% select(-`Data Sets`)
tmp2 <- all_rmse_results %>% filter(str_detect(`Data Sets`, "half stars")) %>% select(RMSE)
tmp <- bind_cols(tmp1, tmp2)

table_inx <- table_inx + 1

tmp %>% kable(caption = paste("Table #", toString(table_inx),
                          " - Results of all models on \"full stars\" + and \"half stars\" data sets", sep = ""),
          col.names = c("Model", "RMSE \"full stars\"", "RMSE \"half stars\"")) %>%
          row_spec(0, bold = TRUE) %>%
          kable_styling(position = "center", full_width = FALSE)

rm(tmp1, tmp2, tmp)

cat("All models used in this script show, that performance is consistently better
on \"half stars\" data sets. This could - intuitively - be related to the finer granularity of the rating scale.\n")


################################################################################
# Final clean up
################################################################################

Sys.setlocale(my_LC_TIME)

rm(edx, validation, rmse_results)
gc(verbose = my_gc_verbose, full = TRUE)

