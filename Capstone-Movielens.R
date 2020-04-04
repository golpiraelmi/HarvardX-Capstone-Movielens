################################
# Create edx set, validation set
################################
options(digits=7)

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
library(kableExtra)
library(lubridate)

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



# printing a few rows to inesrigate edx dataset
head(edx)

# Determining the number of distinct users, movies, and genres
distinct <- edx %>%
  summarize(n_users = n_distinct(userId), 
            n_movies = n_distinct(movieId),
            n_genres=n_distinct(genres))
distinct

# Plot of movie ratings distribution
edx %>%
  ggplot(aes(rating)) +
  geom_histogram(binwidth = 0.25, fill = "blue", color="white") +
  ggtitle("Distribution of Movie Ratings") + 
  theme(plot.title = element_text(hjust = 0.5))  # centre the title

# Extracting movie released date from column "title"
released_date <- stringi::stri_extract(edx$title, regex = "(\\d{4})", comments = TRUE ) %>% 
  as.numeric()

#Add the released date to edx dataset
edx <- edx %>% 
  mutate(year = released_date, 
         year_rated = year(as_datetime(timestamp))) %>% 
  select(-timestamp)
head(edx)

#Check to see if there is any error in extractive movie released date
any_errors<- edx %>% 
  group_by(movieId, title, year) %>% 
  filter(year>2020 | year<1900) %>% 
  distinct (year)
any_errors

#Fixing the incorrect dates
edx[edx$movieId == "6290", "year"] <- 2003
edx[edx$movieId == "1422", "year"] <- 1997
edx[edx$movieId == "671", "year"] <- 1996
edx[edx$movieId == "8198", "year"] <- 1960
edx[edx$movieId == "6645", "year"] <- 1971
edx[edx$movieId == "5310", "year"] <- 1985
edx[edx$movieId == "4159", "year"] <- 2001
edx[edx$movieId == "8905", "year"] <- 1992
edx[edx$movieId == "27266", "year"] <- 2004
edx[edx$movieId == "8864", "year"] <- 2004
edx[edx$movieId == "53953", "year"] <- 2007
edx[edx$movieId == "5472", "year"] <- 1972
edx[edx$movieId == "2308", "year"] <- 1973
edx[edx$movieId =="4311", "year"] <- 1998

# Adding  movie age column to edx
edx <- edx %>% mutate(movie_age = 2020 - year)
head(edx)

#Calculating movie average ratings and rating frequency
movie_avgs <- edx %>% 
  group_by(title) %>% 
  summarize(number_of_movie_ratings=n(), avg_movie_rating = mean(rating)) %>%
  arrange(desc(avg_movie_rating)) 
head(movie_avgs)

# #Plot of movie average ratings and rating frequency
movie_avgs %>% ggplot(aes(number_of_movie_ratings, avg_movie_rating)) +
  geom_point() + 
  geom_smooth(method="loess") + 
  ggtitle("Relationship between average movie ratings and frequency of ratings") +
  theme(plot.title = element_text(hjust = 0.5))  # centre the title

#Calculating user average ratings and rating frequency
user_avgs <- edx %>% 
  group_by(userId) %>% 
  summarize(number_of_user_ratings=n(), avg_user_rating = mean(rating)) %>% 
  filter(number_of_user_ratings> 100) %>%
  arrange(desc(avg_user_rating))
head(user_avgs)

# User who gave maximum average rating
max(user_avgs$avg_user_rating)
user_avgs$userId[which.max(user_avgs$avg_user_rating)] 

# User who gave minimum average rating
min(user_avgs$avg_user_rating)
user_avgs$userId[which.min(user_avgs$avg_user_rating)] 

# Separating movie genres
single_genres <- edx %>% separate_rows(genres, sep ="\\|")
head(single_genres)

# Determining the number of movies in each genre
number_of_movies_genres <- single_genres %>% 
  group_by(genres) %>% 
  summarize(number_movies_genre = n())
head(number_of_movies_genres)

#Calculating genre distribution
genre_distribution <- single_genres %>%
  group_by(genres) %>%
  summarize(n=n()) %>%
  ungroup() %>%
  mutate(rating_per_genre = n/sum(n)) %>%
  arrange(desc(rating_per_genre)) %>% select(-n)

#Plotting movie ratings per genre (rating frequency)
genre_distribution %>%  
  ggplot(aes(reorder(genres, -rating_per_genre), rating_per_genre)) +
  geom_bar(stat = "identity", color="white", fill="blue") + 
  ggtitle("Plot of movie ratings per genre") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5))  # centre the title

#Calculating average movie ratings per genre
mean_rating_per_genre<- single_genres %>%
  group_by(genres) %>%
  summarize(mean_rating_by_genre=mean(rating)) %>%
  arrange(-mean_rating_by_genre)
head(mean_rating_per_genre)

#Plot of average movie ratings per genre
single_genres %>% group_by(genres) %>%
  summarize(n = n(), avgerge_rating = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 1000) %>%
  mutate(genres = reorder(genres, avgerge_rating)) %>%
  ggplot(aes(x = genres, y = avgerge_rating, ymin = avgerge_rating - 2*se, 
             ymax = avgerge_rating + 2*se)) +
  geom_point() +
  geom_errorbar() +
  ggtitle ("Plot of average ratings based on genres") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5))  # centre the title

# PREDICTIONS
# Dividing edx dataset into training and test sets
edx_test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE)
training_set <- edx[-edx_test_index,] 
test_set <- edx[edx_test_index,]

# Defining RMSE function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2, na.rm =TRUE))
}

# Base model-averaging movie ratings over all movies in the training set
mu_hat <- mean(training_set$rating) 
mu_hat

# Calculating RMSE for base model
naive_rmse <- RMSE(test_set$rating, mu_hat) 

# Creating a table to record all RMSE calculations
rmse_results <- tibble(method = "Base model_Averaging", RMSE_on_training_set = naive_rmse, RMSE_on_validation_set="NA")
rmse_results

# Movie effect, determining bias distribution for movies
mu <- mean(training_set$rating)
movie_avgs <- training_set %>%
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

#Plotting distribution of movie bias
qplot(b_i, data = movie_avgs, bins = 20, 
      color = I("white"), fill=I("blue"), 
      ylab = "Number of movies",
      main = 'Distribution of movie bias') + 
  theme(plot.title = element_text(hjust = 0.5))

# Predictive model with movie effect
predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>% 
  pull(b_i)

# Calculating RMSE for movie effect
RMSE_movies<- RMSE(test_set$rating, predicted_ratings)

# adding the results to the rmse tibble for comparison
rmse_results<- add_row(rmse_results, method="Movie_Effect", 
                       RMSE_on_training_set=RMSE_movies, RMSE_on_validation_set="NA")
rmse_results

# User+Movie effect, determining bias distribution for movies and users 
# Plotting user bias distribution
training_set %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_u)) +
  geom_histogram(bins = 20, color="white", fill = "blue") +
  ggtitle("User bias distribution for users who rated over 100 movies") +
  theme(plot.title = element_text(hjust = 0.5))  # centre the title

user_avgs <- training_set %>% left_join(movie_avgs, by='movieId') %>% group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

# Predictive model with movie + user effect
predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>% 
  left_join(user_avgs, by='userId') %>% 
  mutate(pred = mu + b_i + b_u) %>% 
  pull(pred)

# Calculating movie+user model RMSE

RMSE_user_movie<- RMSE(test_set$rating, predicted_ratings)

# addind the results to the rmse tibble for comparison
rmse_results<- add_row(rmse_results, method="User_Movie_Effect", RMSE_on_training_set=RMSE_user_movie, RMSE_on_validation_set="NA")
rmse_results

# Determining tuning parameter
lambdas <- seq(0,10,0.25)
rmses <- sapply(lambdas, function(lam){
mu <- mean(training_set$rating)
  
  b_i <- training_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n() + lam))
  
  b_u <- training_set %>%
    left_join(b_i, by='movieId') %>% 
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n() +lam))

  predicted_ratings <- test_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i +  b_u) %>% .$pred
  
  return(RMSE(predicted_ratings, test_set$rating)) #Note that the test_set here is part of edx dataset and is different from final validation set
})

plot(lambdas, rmses, main="Plot of RMSE versus lambda")

#lambda that minimizes RMSEs for MOVIE + USER
RMSE_REG_MOVIE_USER<-min(rmses)
lam<-lambdas[which.min(rmses)]  
lam

RMSE_REG_MOVIE_USER #Minimum RMSE_REG_MOVIE_USER

# Applying the tuned prediction on Validation set and calculating final RMSE
# lam OBTAINED FROM TUNING RMSE_REG_MOVIE_USER
mu <- mean(validation$rating)

b_i <- validation %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n() + lam))

b_u <- validation %>%
  left_join(b_i, by='movieId') %>% 
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n() +lam))

predicted_ratings <- validation %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mu + b_i +  b_u) %>% .$pred

RMSE_validation<- RMSE(predicted_ratings, validation$rating)
RMSE_validation<- round(RMSE_validation, 7)

rmse_results<- add_row(rmse_results, method="regularized_User_Movie", RMSE_on_training_set=RMSE_REG_MOVIE_USER, 
                       RMSE_on_validation_set=RMSE_validation)
head(rmse_results)

