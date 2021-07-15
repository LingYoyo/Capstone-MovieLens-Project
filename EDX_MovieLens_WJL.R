#title: EDX Capstone MovieLens Scripts
#author: "Wanjun Ling"
#date: "7/12/2021"


##  Data Preparation

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

#check any missing value in ratings at the beginning so that we can know whether there is any issue on source data
anyNA(ratings)
head(ratings)

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

#check any missing value in movies at the beginning so that we can know whether there is any issue on source data
anyNA(movies)
head(movies)

# if using R 3.6 or earlier:
#movies <- as.data.frame(movies) %>% mutate(movieId = #as.numeric(levels(movieId))[movieId],
#                                            title = as.character(title),
#                                            genres = as.character(genres))
# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                            title = as.character(title),
                                            genres = as.character(genres))
#check any missing value in movies so that we can know whether there is any issue after first mutate action.

anyNA(movies)
head(movies)

movielens <- left_join(ratings, movies, by = "movieId")

#check any missing value in movielens so that we can know whether there is any issue after first left_join.
anyNA(movielens)
head(movielens)

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

#remove the temporary files and objects from the working directory
rm(dl, ratings, movies, test_index, temp, movielens, removed)

##   Data Exploration

# Load extra libraries for data analysis and visualization
if(!require(dslabs)) install.packages("dslabs", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
library(dslabs)
library(ggplot2)
library(lubridate)

#check any missing value in training data set "edx"
anyNA(edx)
#check any missing value in final hold-out test data set "validation"
anyNA(validation)
#list amount of observations and variables in training data set "edx"
dim(edx)
#list amount of observations and variables in final hold-out test data set "validation"
dim(validation)
head(edx)
summary(edx)

##    Data Pre-processing
# convert timestamp to year rated and add it to edx
edx <- edx %>% mutate(year_rated = year(as_datetime(timestamp)))

# double check any invalid value after convertion of timestamp
unique(edx$year_rated)

# extract the year released from title and add it to edx
edx <- edx %>% mutate(year_released = as.numeric(str_sub(title,-5,-2)))

# double check any invalid value of year released
unique(edx$year_released)

# calculate the movie age when movie was rated and add it to edx
edx <- edx %>% mutate(ages = as.numeric(year_rated)-as.numeric(year_released))

# double check any invalid value of ages
unique(edx$ages)

# Check odd values portion
sum(edx$ages == -1)/nrow(edx)
sum(edx$ages == -2)/nrow(edx)

# do the same data pre-processing for validation set
validation <- validation %>% mutate(year_rated = year(as_datetime(timestamp)))
validation <- validation %>% mutate(year_released = as.numeric(str_sub(title,-5,-2)))
validation <- validation %>% mutate(ages = as.numeric(year_rated)-as.numeric(year_released))

##   Data Analysis
# Number of unique users, movies 
edx %>% summarize(unique_users = n_distinct(userId), unique_movies = n_distinct(movieId))

# list all rating values
unique(edx$rating)

# Top ten ratings
edx %>% group_by(rating) %>% 
  summarize(rating_sum_number = n()) %>% top_n(10) %>% arrange(desc(rating_sum_number))

#Plot rating counts for each rating value
edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  ggtitle("Rating Values Count") + theme(plot.title = element_text(hjust = 0.5)) + 
  labs(subtitle ="number of ratings by UserId", x="Rating Values" , y="Rating Counts") + theme(plot.subtitle = element_text(hjust = 1)) + geom_line()


# Explore whether movies effects exists
# Plot histogram on number of ratings by "movieId"
edx %>% 
  count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 10, color = "yellow") + 
  scale_x_log10() + 
  ggtitle("Movies Rating Distributions") + theme(plot.title = element_text(hjust = 0.5)) + 
  labs(subtitle ="number of ratings by movieId", x="movieId" ,y="number of ratings") + theme(plot.subtitle = element_text(hjust = 1)) 

# Explore whether users effects exists
# plot histogram of number of ratings by "userId"
edx %>% count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 20, color = "green") + 
  scale_x_log10() + 
  ggtitle("Users Rating Distributions") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(subtitle ="number of ratings by UserId", x="userId" , y="number of ratings") + theme(plot.subtitle = element_text(hjust = 1))

# explore whether time effects exists
# Average Rating versus year_rated of movies
edx %>% group_by(year_rated) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(year_rated, rating)) +
  geom_point() +
  geom_smooth() +
  geom_hline(yintercept = mean(edx$rating), col = "red", linetype = "dashed") +
  ggtitle("Year Rated Effect") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(subtitle = "Year_rated VS Average ratings", x="Year Rated" , y="Average Ratings") + theme(plot.subtitle = element_text(hjust = 1))

# Average Rating versus year_released of movies
edx %>% group_by(year_released) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(year_released, rating)) +
  geom_point() +
  geom_smooth(size = 5) +
  geom_hline(yintercept = mean(edx$rating), col = "red", linetype = "dashed") +
  ggtitle("Year Released Effect") + theme(plot.title =element_text(hjust = 0.5)) +
  labs(subtitle = "Year_released VS Average ratings", x="Year Released" , y="Average Ratings") + theme(plot.subtitle = element_text(hjust = 1))


# Visualize amounts of "ages VS rating"
ggplot(data = edx) + aes(x=ages,fill=rating) + geom_histogram(binwidth =5) + ggtitle("Ages Rating Distributions") + theme(plot.title = element_text(hjust = 0.5))

# Average Rating versus age of movies by geom_line
edx %>% group_by(ages) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(ages, rating)) +
  geom_line(color = "green") + 
  scale_x_continuous(breaks=seq(0, 100, by= 5)) +
  #theme_solarized(light = FALSE) +
  geom_smooth() +
  geom_hline(yintercept = mean(edx$rating), col = "red", linetype = "dashed") +
  ggtitle("Movie Ages Effect") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(subtitle = "Movies Age VS Average ratings", x="Movies Age" , y="Average Ratings") + theme(plot.subtitle = element_text(hjust = 1))

# Average Rating versus ages of movies by geom_point
edx %>% group_by(ages) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(ages, rating)) +
  geom_point() +
  geom_smooth() +
  geom_hline(yintercept = mean(edx$rating), col = "red", linetype = "dashed") +
  ggtitle("Movie Ages Effect") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(subtitle = "Movies Age VS Average Ratings", x="Movies Age" , y="Average Ratings") + theme(plot.subtitle = element_text(hjust = 1))

#Top 10 rated title and genres counting majority of ratings
edx %>% group_by(title, genres) %>%summarize(count=n()) %>%top_n(10,count) %>%arrange(desc(count))

# Explore whether "genres" effects exists
# split genres in "edx" and create a new data set "edx_split_genres" for analysis
edx_split_genres <- edx %>% separate_rows(genres, sep ="\\|")
head(edx_split_genres)

#Plot moviecounts per Genre
edx_split_genres %>% group_by(genres) %>% summarize(movie_num_per_genres = n_distinct(movieId)) %>% arrange(-movie_num_per_genres) %>% ggplot(aes(reorder(genres, movie_num_per_genres), movie_num_per_genres, fill= movie_num_per_genres)) +
  geom_bar(stat = "identity") + coord_flip() + ggtitle("Movie Counts Per Genres") + 
  labs(x = "Split_Genres", y = "Movie_counts")

#Plot Ratingcounts per Genre 
edx_split_genres %>% 
  group_by(genres) %>%
  summarize(ratingcounts = n()) %>%
  arrange(desc(ratingcounts)) %>% ggplot(aes(reorder(genres,ratingcounts),ratingcounts)) + 
  geom_bar(aes(fill = genres),stat = "identity")+ 
  ggtitle("Distribution of Genres by Ratingcounts") + labs(x = "Genres", y = "Rating_counts") +
  theme(axis.text.x  = element_text(angle= 90))

#Average rating for each genre
Genres_avg_rating <- edx_split_genres %>%
  group_by(genres) %>%
  summarize(mean_rating_by_genre=mean(rating)) %>%
  arrange(-mean_rating_by_genre)
Genres_avg_rating

#General average rating for genres
avg_rating <- edx_split_genres %>% summarize(mean(rating))
avg_rating 
#mean(rating) 3.53

#Plot Mean Rating per Genre compared with general average rating
Genres_avg_rating %>%
  ggplot(aes(reorder(genres, mean_rating_by_genre), mean_rating_by_genre, fill= mean_rating_by_genre)) +
  geom_bar(stat = "identity") + coord_flip() + ggtitle("Average Rating of Per Genre") + 
  scale_fill_distiller(palette="RdYlGn") +
  labs(y = "Mean Rating", x = "Genre") + geom_hline(yintercept = 3.53, col = "red", linetype = "dashed") 

# MODELING AND RESULTS

#define RMSE short for residual mean squared error.

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

##   Model 1: Only by average rating


#the simplest possible model: we predict the same rating for all movies regardless of user.
mu_hat <- mean(edx$rating)
mu_hat

naive_rmse <- RMSE(validation$rating, mu_hat)
naive_rmse

rmse_results <- data_frame(Model = "Only by average rating", RMSE = naive_rmse)
rmse_results %>% knitr::kable()

## Model 2: Modeling with movieId effect

#calculating and plot b_m distribution
movie_effects <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_m = mean(rating - mu_hat))
movie_effects %>% qplot(b_m, geom ="histogram", bins = 10, data = ., color = I("blue"))

#calculating predicted ratings
predicted_ratings_m <- mu_hat + validation %>% 
  left_join(movie_effects, by='movieId') %>%
  pull(b_m)

#calculating model_m_rmse with consideration of movie effects
model_m_rmse <- RMSE(validation$rating,predicted_ratings_m) 
model_m_rmse

#insert new result into DF rmse_result
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Movie Effect Model",  
                                     RMSE = model_m_rmse))
rmse_results %>% knitr::kable()

## Model 3: Modeling with movieId + userId effects

#calculating and plot b_u distribution
user_effects <- edx %>% 
  left_join(movie_effects, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu_hat - b_m))
user_effects %>% qplot(b_u, geom ="histogram", bins = 10, data = ., color = I("blue")) 

#calculating predicted ratings
predicted_ratings_m_u <- validation %>% 
  left_join(movie_effects, by='movieId') %>%
  left_join(user_effects, by='userId') %>%
  mutate(prediction = mu_hat + b_m + b_u) %>%
  pull(prediction)

#calculating model_m_rmse with consideration of movie + user effects  
model_m_u_rmse <- RMSE(validation$rating,predicted_ratings_m_u)

rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Movie + User Effects Model",  
                                     RMSE = model_m_u_rmse))
rmse_results %>% knitr::kable()

## Model 4: Modeling with movieId + userId + movieAges effects
#calculating and plot b_a distribution
ages_effects <- edx %>% 
  left_join(movie_effects, by='movieId') %>%
  left_join(user_effects, by='userId') %>%
  group_by(ages) %>%
  summarize(b_a = mean(rating - mu_hat - b_m - b_u))
ages_effects %>% qplot(b_a, geom ="histogram", bins = 10, data = ., color = I("blue"))

#calculating predicted ratings
predicted_ratings_m_u_a <- validation %>% 
  left_join(movie_effects, by='movieId') %>%
  left_join(user_effects, by='userId') %>%
  left_join(ages_effects, by='ages') %>%
  mutate(prediction = mu_hat + b_m + b_u + b_a) %>%
  pull(prediction)

#calculating model_m_rmse with consideration of movie + user + age effects  
model_m_u_a_rmse <- RMSE(validation$rating,predicted_ratings_m_u_a)

rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Movie + User + Age Effects Model",
                                     RMSE = model_m_u_a_rmse))
rmse_results %>% knitr::kable()

## Model 5: Regularization of movieId + userId effects 
lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){
mu_reg <- mean(edx$rating)

#regulation movie effect
b_m_reg <- edx %>%
group_by(movieId) %>%
summarize(b_m_reg = sum(rating - mu_reg)/(n()+l))

#regulation user effect
b_u_reg <- edx %>%
left_join(b_m_reg, by="movieId") %>%
group_by(userId) %>%
summarize(b_u_reg = sum(rating - b_m_reg - mu_reg)/(n()+l))

#calculating predicted ratings
predicted_ratings_b_m_u <-
validation %>%
left_join(b_m_reg, by = "movieId") %>%
left_join(b_u_reg, by = "userId") %>%
mutate(prediction = mu_reg + b_m_reg + b_u_reg) %>%
.$prediction
return(RMSE(validation$rating,predicted_ratings_b_m_u))
})

qplot(lambdas, rmses)

#For the full model, the optimal lambda is given as
lambda <- lambdas[which.min(rmses)]
lambda

#calculating regularization model RMSE: model_m_u_reg_rmse
model_m_u_reg_rmse <- min(rmses)
model_m_u_reg_rmse

rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Movie + User Regularization Model",
                                     RMSE = model_m_u_reg_rmse))
rmse_results %>% knitr::kable()

## Model 6: Modeling with movieId + userId + movieAges + genres effects

# Create data set validation_split_genres with split genres from data set validation 

validation_split_genres <- validation %>% separate_rows(genres, sep ="\\|")

mu_hat_s <- mean(edx_split_genres$rating)

movie_effects_s <- edx_split_genres %>% 
  group_by(movieId) %>% 
  summarize(b_m_s = mean(rating - mu_hat_s))

user_effects_s <- edx_split_genres %>% 
  left_join(movie_effects_s, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u_s = mean(rating - mu_hat_s - b_m_s))

ages_effects_s <- edx_split_genres %>% 
  left_join(movie_effects_s, by='movieId') %>%
  left_join(user_effects_s, by='userId') %>%
  group_by(ages) %>%
  summarize(b_a_s = mean(rating - mu_hat_s - b_m_s - b_u_s))

#calculating and plot b_g distribution
genres_effects_s <- edx_split_genres %>% 
  left_join(movie_effects_s, by='movieId') %>%
  left_join(user_effects_s, by='userId') %>%
  left_join(ages_effects_s, by='ages') %>%
  group_by(genres) %>%
  summarize(b_g_s = mean(rating - mu_hat_s - b_m_s - b_u_s - b_a_s))
genres_effects_s %>% qplot(b_g_s, geom ="histogram", bins = 10, data = ., color = I("blue"))

#calculating predicted ratings
predicted_ratings_m_u_a_g_s <- validation_split_genres %>% 
  left_join(movie_effects_s, by='movieId') %>%
  left_join(user_effects_s, by='userId') %>%
  left_join(ages_effects_s, by='ages') %>%
  left_join(genres_effects_s, by='genres') %>%
  mutate(prediction = mu_hat_s + b_m_s + b_u_s + b_a_s + b_g_s) %>%
  pull(prediction)

#calculating model_m_rmse with consideration of movie + user + age effects  
model_m_u_a_g_s_rmse <- RMSE(validation_split_genres$rating,predicted_ratings_m_u_a_g_s)

rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Movie + User + Age + Genres Effects Model",
                                     RMSE = model_m_u_a_g_s_rmse))
rmse_results %>% knitr::kable()
