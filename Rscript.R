################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
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

#######################
# Dataset exploration #
#######################

head (edx)

str (edx)

summary (edx)

####################
# Data preparation #
####################

# Create the train and test set
# The test set is 20% of the edx set
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE)
train_set <- edx[-test_index,]
temp <- edx[test_index,]

# Make sure userId and movieId in test set are also in train set
test_set <- temp %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Conversion of timestamp class and extraction of movie review year
train_set$timestamp <- as.POSIXct(train_set$timestamp, origin="1970-01-01") %>% 
  year()

test_set$timestamp <- as.POSIXct(test_set$timestamp, origin="1970-01-01") %>% 
  year()

validation$timestamp <- as.POSIXct(validation$timestamp, origin="1970-01-01") %>% 
  year()

# Detection and extraction of release year
train_set$releaseyear <- train_set$title %>% 
  str_extract("\\(\\d{4}\\)") %>% 
  str_extract("\\d{4}") %>% 
  as.numeric()

test_set$releaseyear <- test_set$title %>% 
  str_extract("\\(\\d{4}\\)") %>% 
  str_extract("\\d{4}") %>% 
  as.numeric()

validation$releaseyear <- validation$title %>% 
  str_extract("\\(\\d{4}\\)") %>% 
  str_extract("\\d{4}") %>% 
  as.numeric()

# Computation of movie age when sharing the review
train_set$MovieAge <- train_set$timestamp- train_set$releaseyear
test_set$MovieAge <- test_set$timestamp- test_set$releaseyear
validation$MovieAge <- validation$timestamp- validation$releaseyear

# Detection of review year errors
head(train_set %>% 
       filter (MovieAge < 0)%>% 
       select(timestamp,title,releaseyear, MovieAge))

# Showing the number of errors in the train set
train_set %>% 
  filter (MovieAge < 0)%>% 
  select(timestamp,title,releaseyear, MovieAge) %>% 
  summarize ("number of errors" = n())

# Set time difference to 0 when an error is detected
train_set$MovieAge <- ifelse (train_set$MovieAge <0,0, train_set$MovieAge)
test_set$MovieAge <- ifelse (test_set$MovieAge <0,0, test_set$MovieAge)
validation$MovieAge <- ifelse (validation$MovieAge <0,0, validation$MovieAge)

# Showing there are no more errors
train_set %>% filter (MovieAge < 0)%>% 
  select(timestamp,title,releaseyear, MovieAge) %>% 
  summarize ("Train set number of errors" = n())

test_set %>% filter (MovieAge < 0)%>% 
  select(timestamp,title,releaseyear, MovieAge) %>% 
  summarize ("Test set number of errors" = n())

validation %>% 
  filter (MovieAge < 0)%>% 
  select(timestamp,title,releaseyear, MovieAge) %>% 
  summarize ("Validation set number of errors" = n())

# Genres separation
# Structure of the train_set
str (train_set)

# We divide the train set in two subsets to reduce memory usage when separating rows
train_set_A <-train_set [1:3500000,]

train_set_B <-train_set [3500001:7200043,]

train_set_A <- train_set_A %>% 
  separate_rows(genres, sep = "\\|")

train_set_B <- train_set_B %>% 
  separate_rows(genres, sep = "\\|")

# We join the subsets to have one only train set
train_set <- rbind(train_set_A, train_set_B)

rm (train_set_A, train_set_B)

head (train_set)

# Genres separation in the test set
test_set <- test_set %>% 
  separate_rows(genres, sep = "\\|")

# Genres separation in the validation set
validation <- validation %>% 
  separate_rows(genres, sep = "\\|")

# The RMSE formula
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings-predicted_ratings)^2, na.rm=TRUE))}

#############################
# Data analysis and methods #
#############################

# Average movie rating method #

# Computation of average movie rating
mu <- mean(train_set$rating)

# Computation of RMSE
avg_rmse <- RMSE(mu, test_set$rating)

# Showing the table with RMSE obtained with average movie ratings
rmse_results <- data_frame(method = "Average movie rating model", RMSE = avg_rmse)
rmse_results %>% knitr::kable()

# Movie effect method #

# Histogram of movies rating
train_set %>% 
  group_by(movieId) %>% 
  summarize ("R"=mean(rating)) %>%
  ggplot(aes(R)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  labs (x="Average movie rating", y = "Number of movies")+
  ggtitle("Movie effect")

# Movie effect model
b_i <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

# Computation of prediction
predicted_ratings <- test_set %>%
  left_join(b_i, by = "movieId") %>%
  mutate(pred = mu + b_i) %>%
  .$pred

# Computation of RMSE
b_i_rmse <- RMSE(predicted_ratings, test_set$rating)

# Showing the table with RMSE obtained with movie effect
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",  
                                     RMSE = b_i_rmse ))

rmse_results %>% knitr::kable()

# Regularized movie effect method #

# Histogram of movies reviews
train_set %>% 
  group_by(movieId) %>% 
  summarize ("n"=n()) %>%
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  labs (x="Number of movie reviews", y = "Number of movies")+
  ggtitle("Movie effect")

# Regularized movie effect model
# Lambda value is set to 1.5 and the optimal value will be calculated at the end
l <- 1.5

b_i <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+l))

# Computation of prediction
 predicted_ratings <- test_set %>%
  left_join(b_i, by = "movieId") %>%
  mutate(pred = mu + b_i) %>%
  .$pred

# Computation of RMSE
b_i_rmse <- RMSE(predicted_ratings, test_set$rating)

# Showing the table with RMSE obtained with regularized movie effect model
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie Effect Model",  
                                     RMSE = b_i_rmse ))

rmse_results %>% knitr::kable()

# User effect method #

# Histogram of users rating
train_set %>% 
  group_by(userId) %>% 
  summarize ("Rating"=mean (rating)) %>%
  ggplot(aes(Rating)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  labs (x="Average user rating", y = "Number of users")+
  ggtitle("User effect")

# Histogram of users activity
train_set %>% 
  group_by(userId) %>% 
  summarize ("Reviews"=n()) %>%
  ggplot(aes(Reviews)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  labs (x="Number of user reviews", y = "Number of users")+
  ggtitle("User effect")

# Regularized user effect model
b_u <- train_set %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+l))

# Computation of prediction
predicted_ratings <- 
  test_set %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

# Computation of RMSE
 b_u_rmse <- RMSE(predicted_ratings, test_set$rating)

# Showing the table with RMSE obtained with regularized user effect model
 rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie + User Effect Model",  
                                     RMSE = b_u_rmse ))

rmse_results %>% knitr::kable()

# Regularized movie age effect model #

# First movie dates
train_set %>% 
  select (title, releaseyear) %>% 
  arrange (releaseyear) %>% 
  distinct (title) %>% 
  head()

# First review dates
train_set %>% 
  select(movieId, title,  timestamp) %>% 
  group_by(movieId)%>% arrange (timestamp) %>% 
  distinct (title, timestamp) %>% 
  select (title, timestamp) %>%
  head()

# Movie age influence on movie rating
train_set %>% 
  group_by(MovieAge)%>%
  summarize (Rating = mean(rating)) %>% 
  ggplot(aes(MovieAge, Rating))+ 
  geom_point()+
  geom_smooth(method=loess, formula = y ~ x)+
  labs (x="Years", y="Average rating")+
  ggtitle("Movie age effect")

# Reviews distribution
train_set %>% 
  group_by(MovieAge)%>%
  summarize (Reviews = n()) %>% 
  ggplot(aes(MovieAge, Reviews))+ 
  geom_point()+
  geom_smooth(method=loess, formula = y ~ x)+
  labs (x="Years", y="Number of reviews")+
  ggtitle("Movie age effect")

# Regularized Movie age effect
b_ma <- train_set %>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by = "userId") %>%
  group_by(MovieAge)%>%
  summarize(b_ma = sum(rating - b_u - b_i - mu)/(n()+l))

# Computation of prediction
predicted_ratings <- 
  test_set %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_ma, by = "MovieAge") %>%
    mutate(pred = mu + b_i + b_u + b_ma) %>%
  .$pred

# Computation of RMSE
b_ma_rmse <- RMSE(predicted_ratings, test_set$rating)

# Showing the table with RMSE obtained with regularized movie + user + movie age effect model
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie + User + Movie age Effect Model",  
                                     RMSE = b_ma_rmse ))

rmse_results %>% knitr::kable()

# Regularized genre model #

# Rating bar plot by genre
train_set %>% 
  group_by (genres) %>%
  summarize (Rating = mean(rating)) %>% 
  arrange (genres) %>% 
  ggplot(aes(genres, Rating))+ 
  geom_col()+
  labs (x="Genres", y ="Average genre rating")+
  ggtitle("Genres effect")+ 
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

# Number of reviews by genre
train_set %>% 
  group_by (genres) %>%
  summarize (Reviews = n()) %>% 
  arrange (genres) %>% 
  ggplot(aes(genres, Reviews))+ 
  geom_col()+
  labs (x="Genres", y ="Number of reviews")+
  ggtitle("Genres effect")+ 
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

# Regularized genre effect
b_g <- train_set %>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_ma, by = "MovieAge") %>%
  group_by(genres)%>%
  summarize(b_g = sum(rating - b_u - b_i - b_ma - mu)/(n()+l))

# Computation of prediction
predicted_ratings <- 
  test_set %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_ma, by = "MovieAge") %>%
  left_join(b_g, by = "genres") %>%
  mutate(pred = mu + b_i + b_u + b_ma + b_g) %>%
  .$pred

# Computation of RMSE
b_g_rmse <- RMSE(predicted_ratings, test_set$rating)

# Showing the table with RMSE obtained with regularized movie + user + movie age + genre effect model
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie + User+ Movie age + Genre Effect Model",  
                                     RMSE = b_g_rmse ))

rmse_results %>% knitr::kable()

# Tuning the final model #
# Lambda "l" cross-validation
lambdas <- seq(10, 15, 0.25)
rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  b_ma <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by = "userId") %>%
    group_by(MovieAge)%>%
    summarize(b_ma = sum(rating - b_u - b_i - mu)/(n()+l))
  b_g <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_ma, by = "MovieAge") %>%
    group_by(genres)%>%
    summarize(b_g = sum(rating - b_u - b_i - b_ma - mu)/(n()+l))
  
  predicted_ratings <-
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_ma, by = "MovieAge") %>%
    left_join(b_g, by = "genres") %>%
    mutate(pred = mu + b_i + b_u + b_ma + b_g) %>%
    .$pred
  
  return(RMSE(predicted_ratings, test_set$rating))
})

# Lambda curve to see the best value
plot (lambdas, rmses)

# Best lambda
lambdas[which.min(rmses)]

# Final model #
# Setting lambda value in the final model
l <- 13.5

mu <- mean(train_set$rating)
b_i <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+l))
b_u <- train_set %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+l))
b_ma <- train_set %>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by = "userId") %>%
  group_by(MovieAge)%>%
  summarize(b_ma = sum(rating - b_u - b_i - mu)/(n()+l))
b_g <- train_set %>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_ma, by = "MovieAge") %>%
  group_by(genres)%>%
  summarize(b_g = sum(rating - b_u - b_i - b_ma - mu)/(n()+l))

# Computation of prediction
predicted_ratings <-
  test_set %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_ma, by = "MovieAge") %>%
  left_join(b_g, by = "genres") %>%
  mutate(pred = mu + b_i + b_u + b_ma + b_g) %>%
  .$pred

# Computation of RMSE
tuned_rmse <- RMSE(predicted_ratings, test_set$rating)

# Showing the table with RMSE obtained with tuned regularized movie + user + movie age + genre effect model
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Final Model tuned",  
                                     RMSE = tuned_rmse ))

rmse_results %>% knitr::kable()

###########
# Results #
###########

# Computation of prediction on VALIDATION set
predicted_ratings <- 
  validation %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_g, by = "genres") %>%
  left_join(b_ma, by = "MovieAge") %>%
  mutate(pred = mu + b_i + b_u + b_ma + b_g) %>%
  .$pred

# RMSE with VALIDATION set
validation_rmse <- RMSE(predicted_ratings, validation$rating)

rmse_results <- data_frame(method = "Final model with VALIDATION set", RMSE = validation_rmse)

rmse_results %>% knitr::kable()

#######
# END #
#######
