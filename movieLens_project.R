# Besm Allah
# Arwa Ashi
# Jan 29, 2022
# Harvard PH125.9x
# Data Science: Capstone
# Final Project
# -------------------------

##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

#if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
#if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
#if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
#if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
#if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
#if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
#if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
#if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
#if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

# downloading the data
# --------------------------------------
#dl <- tempfile()
#download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

# ---------------------------------------
# ---------------------------------------
# data cleaning, (01)
# ---------------------------------------
# ---------------------------------------
#ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
#                 col.names = c("userId", "movieId", "rating", "timestamp"))

#movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)

#colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later:
# --------------------------------------
#movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
#                                           title = as.character(title),
#                                           genres = as.character(genres))

#movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
# --------------------------------------
#set.seed(1, sample.kind="Rounding")
#test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
#edx <- movielens[-test_index,]
#temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
# --------------------------------------
#validation <- temp %>% 
#  semi_join(edx, by = "movieId") %>%
#  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
# --------------------------------------
#removed <- anti_join(temp, validation)
#edx <- rbind(edx, removed)

# Remove extra files 
# --------------------------------------
#rm(dl, ratings, movies, test_index, temp, movielens, removed)


# save final files 
# --------------------------------------
#write.csv(edx,"edx.csv", row.names = FALSE)
#write.csv(validation,"validation.csv", row.names = FALSE)


# ---------------------------------------
# ---------------------------------------
# Importing data after data cleaning, (01)
# ---------------------------------------
# ---------------------------------------
library(tidyverse)
library(caret)
library(data.table)
library(stringr)
library(ggplot2)
library(knitr)
library(kableExtra)
library(scales)
library(forcats)
library(lubridate)
library(ggthemes)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

edx <- read.csv(file = 'edx.csv')
validation <-read.csv(file = 'validation.csv')

# ---------------------------------------
# ---------------------------------------
# data cleaning, (02)
# ---------------------------------------
# ---------------------------------------
edx <- mutate(edx, date = as_datetime(timestamp))
validation <- mutate(validation, date = as_datetime(timestamp))

# ---------------------------------------
# ---------------------------------------
# data exploration, 
# ---------------------------------------
# ---------------------------------------
# Quiz: MovieLens Dataset

nrow(edx) # total row
ncol(edx) # total column

sum(edx$rating==0) # total rate = 0
sum(edx$rating==3) # total rate = 3
format(sum(edx$rating==3),big.mark=",",scientific=F)

mean(edx$rating)   # average rating

format(head(sort(-table(edx$rating)),5),big.mark=",",scientific=F)

# distribution over rates
format(data.frame(table(edx$rating)),big.mark=",",scientific=F)

edx%>%summarize(n_movie=n_distinct(movieId), # total unique movie id
                n_user=n_distinct(userId))   # total unique user id

# movie title
edx %>% 
  group_by(title) %>% 
  summarise(number = n()) %>%
  arrange(desc(number))


# total row by genres
nrow(edx%>%filter(str_detect(genres,'Drama')))
nrow(edx%>%filter(str_detect(genres,'Comedy')))
nrow(edx%>%filter(str_detect(genres,'Thriller')))
nrow(edx%>%filter(str_detect(genres,'Romance')))

# total Drama move
nrow(edx%>%filter(str_detect(genres,'Drama')))
# and percentage in move is equal to 
nrow(edx%>%filter(str_detect(genres,'Drama')))/nrow(edx)*100

# total Comedy movie is
nrow(edx%>%filter(str_detect(genres,'Comedy')))
# and percentage in move is equal to
nrow(edx%>%filter(str_detect(genres,'Comedy')))/nrow(edx)*100

# total Thriller movie is
nrow(edx%>%filter(str_detect(genres,'Thriller')))
# and percentage in move is equal to
nrow(edx%>%filter(str_detect(genres,'Thriller')))/nrow(edx)*100

# total Romance movie is
nrow(edx%>%filter(str_detect(genres,'Romance')))
# and percentage is equal to
nrow(edx%>%filter(str_detect(genres,'Romance')))/nrow(edx)*100

# Drama is the most rate it movie with ~ 43% and Romance is the least rated movie with ~19% of total ratting

# total Drama movie
nrow(validation%>%filter(str_detect(genres,'Drama')))
# and percentage in movie is equal to 
nrow(validation%>%filter(str_detect(genres,'Drama')))/nrow(validation)*100

# total Comedy movie is
nrow(validation%>%filter(str_detect(genres,'Comedy')))
# and percentage in move is equal to
nrow(validation%>%filter(str_detect(genres,'Comedy')))/nrow(validation)*100

# total Thriller moive is
nrow(validation%>%filter(str_detect(genres,'Thriller')))
# and percentage in movie is equal to
nrow(validation%>%filter(str_detect(genres,'Thriller')))/nrow(validation)*100

# total Romance movie is
nrow(validation%>%filter(str_detect(genres,'Romance')))
# and percentage is equal to
nrow(validation%>%filter(str_detect(genres,'Romance')))/nrow(validation)*100

# Drama is the most rate it movie with ~ 43% and Romance is the least rated movie with ~19% of total ratting

df <- data.frame(movie_type=rep(c('Drama', 'Comedy', 'Thriller','Romance','Total'), each=1),
                 edx_total_movie=rep(c(
                   nrow(edx%>%filter(str_detect(genres,'Drama'))),
                   nrow(edx%>%filter(str_detect(genres,'Comedy'))),
                   nrow(edx%>%filter(str_detect(genres,'Thriller'))),
                   nrow(edx%>%filter(str_detect(genres,'Romance'))),
                   nrow(edx%>%filter(str_detect(genres,'Drama'))) +nrow(edx%>%filter(str_detect(genres,'Comedy'))) +nrow(edx%>%filter(str_detect(genres,'Thriller')))+nrow(edx%>%filter(str_detect(genres,'Romance')))
                 ), each=1),
                 edx_percentage_movie=rep(c(
                   nrow(edx%>%filter(str_detect(genres,'Drama')))/nrow(edx)*100,
                   nrow(edx%>%filter(str_detect(genres,'Comedy')))/nrow(edx)*100,
                   nrow(edx%>%filter(str_detect(genres,'Thriller')))/nrow(edx)*100,
                   nrow(edx%>%filter(str_detect(genres,'Romance')))/nrow(edx)*100,
                   +nrow(edx%>%filter(str_detect(genres,'Drama')))/nrow(edx)*100 +nrow(edx%>%filter(str_detect(genres,'Comedy')))/nrow(edx)*100 +nrow(edx%>%filter(str_detect(genres,'Thriller')))/nrow(edx)*100 +nrow(edx%>%filter(str_detect(genres,'Romance')))/nrow(edx)*100
                 ), each=1),
                 Valid_total_movie=rep(c(
                   nrow(validation%>%filter(str_detect(genres,'Drama'))),
                   nrow(validation%>%filter(str_detect(genres,'Comedy'))),
                   nrow(validation%>%filter(str_detect(genres,'Thriller'))),
                   nrow(validation%>%filter(str_detect(genres,'Romance'))),
                   nrow(validation%>%filter(str_detect(genres,'Drama'))) +nrow(validation%>%filter(str_detect(genres,'Comedy'))) +nrow(validation%>%filter(str_detect(genres,'Thriller'))) +nrow(validation%>%filter(str_detect(genres,'Romance')))
                 ), each=1),
                 Valid_percentage_movie=rep(c(
                   nrow(validation%>%filter(str_detect(genres,'Drama')))/nrow(validation)*100,
                   nrow(validation%>%filter(str_detect(genres,'Comedy')))/nrow(validation)*100,
                   nrow(validation%>%filter(str_detect(genres,'Thriller')))/nrow(validation)*100,
                   nrow(validation%>%filter(str_detect(genres,'Romance')))/nrow(validation)*100,
                   nrow(validation%>%filter(str_detect(genres,'Drama')))/nrow(validation)*100 +nrow(validation%>%filter(str_detect(genres,'Comedy')))/nrow(validation)*100 +nrow(validation%>%filter(str_detect(genres,'Thriller')))/nrow(validation)*100 +nrow(validation%>%filter(str_detect(genres,'Romance')))/nrow(validation)*100
                 ), each=1))
df <- format(df,big.mark=",",scientific=F)
df

# finding how many unique genres
unique(edx$genres)

unique(validation$genres)

# Table for unique movie and user ids and genres
edx%>%summarize(n_movie=n_distinct(movieId),
                n_user=n_distinct(userId),
                n_genres=n_distinct(genres))

# ---------------------------------------
# ---------------------------------------
# visualization, 
# ---------------------------------------
# ---------------------------------------

# rate distribution overtime
edx %>% mutate(year = year(as_datetime(timestamp, origin="1970-01-01"))) %>%
  ggplot(aes(x=year)) +
  geom_histogram() +
  theme_classic()

# min record date
date(as_datetime(min(edx$timestamp)))

# max record date
date(as_datetime(max(edx$timestamp)))
  
# distribution unique movies over genres
edx %>% group_by(genres) %>%
  summarize(n_movie = n_distinct(movieId))%>%
  mutate(genres = reorder(genres,n_movie)) %>%
  filter(n_movie >= 300) %>%
  arrange(n_movie,genres) %>%
  ggplot(aes(x = n_movie, y = genres, 
             fill=genres)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5)+
  #coord_flip()+
  labs(title="Top Movies' distrbuted by unique genres",
            subtitle = "showing 300 distribution and more",
            caption = "Capstone Project") 

# distribution unique movie id over genres, rating
edx %>% group_by(rating,genres) %>%
  summarize(n_movie = n_distinct(movieId))%>%
  filter(n_movie >= 1000)%>%
  arrange(n_movie,genres) %>%
  ggplot(aes(x = n_movie, y = rating, 
             fill=genres)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_bar(stat = "identity")+
  #coord_flip()+
  labs(title="Top Movies' distrbuted by unique genres and rating",
       subtitle = "showing 100 distribution and more",
       caption = "Capstone Project"
  )
    
# distribution unique user id over genres
edx %>% group_by(genres) %>%
    summarize(n_user = n_distinct(userId))%>%
    mutate(genres = reorder(genres,n_user))%>%
    filter(n_user >= 50000) %>%
    arrange(n_user,genres) %>%
    ggplot(aes(x = n_user, y = genres, 
               fill=genres)) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.5)+
    #coord_flip()+
    labs(title="Top users' distrbuted by unique genres",
         subtitle = "showing 50000 distribution and more",
         caption = "Capstone Project"
    ) 

# distribution unique user id over genres and rating
edx %>% group_by(rating, genres) %>%
  summarize(n_user = n_distinct(userId))%>%
  filter(n_user >= 30000) %>%
  arrange(n_user,genres) %>%
  ggplot(aes(x = n_user, y = rating, 
             fill=genres)) + 
  geom_bar(stat = "identity", position = "dodge")+
  #coord_flip()+
  labs(title="Top users' distrbuted by unique genres",
       subtitle = "showing 30000 distribution and more",
       caption = "Capstone Project"
  )


# every user rate different number of movie by different rate
# 1 movie
keep <- edx %>%
  dplyr::count(movieId) %>%
  top_n(5) %>%
  pull(movieId)

keep

# 2 match to user and rate
tab <- edx %>%
  filter(userId %in% c(13:20))%>%
  filter(movieId %in% keep) %>%
  select(userId, title, rating)%>%
  spread(title, rating)

tab %>% knitr::kable()

tab

users <- sample(unique(edx$userId),100)
users

edx %>% 
  dplyr::count(movieId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color='black') + 
  scale_x_log10() +
  ggtitle('Movies')

edx %>% 
  dplyr::count(userId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color='black') + 
  scale_x_log10() +
  ggtitle('Users')

# ---------------------------------------
# ---------------------------------------
# modeling approach
# ---------------------------------------
# ---------------------------------------
set.seed(755)

# 1 splitting the data
test_index <- createDataPartition(y = edx$rating,
                                  times = 1,
                                  p = 0.2,
                                  list = FALSE)
train_set <- edx[-test_index,]
test_set<- edx[test_index,]

test_set <- test_set %>%
  semi_join(train_set, by='movieId') %>%
  semi_join(train_set, by= 'userId')
test_set

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2,na.rm=TRUE))
}

RMSE

# ---------------------------------------
# ---------------------------------------
# A first model
# $ Y_{u,i} = \mu + \epsilon_{u,i}$
# ---------------------------------------
# ---------------------------------------
mu_hat <- mean(train_set$rating)
mu_hat

# test set
naive_rmse <- RMSE(test_set$rating, mu_hat)
naive_rmse

predictions <- rep(3, nrow(test_set))
predictions

model_0_rmse<- RMSE(test_set$rating, predictions)

rmse_results <- tibble(method = "Just the average", RMSE = naive_rmse)
rmse_results

# Validation set
val_naive_rmse <- RMSE(validation$rating, mu_hat)
val_naive_rmse

val_predictions <- rep(3, nrow(validation))
val_predictions

val_model_0_rmse <-RMSE(validation$rating, val_predictions)
val_model_0_rmse

#rmse_results <- bind_rows(rmse_results,
#                          data_frame(method = "Just the average - validation set", 
#                          RMSE = val_model_0_rmse))
#rmse_results

# ---------------------------------------
# ---------------------------------------
# A Second model - Modeling movie effects
# $ Y_{u,i} = \mu + + b_i + \epsilon_{u,i}$
# ---------------------------------------
# ---------------------------------------
mu <- mean(train_set$rating) 
mu

movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))
movie_avgs

# plot of bi
qplot(b_i, data = movie_avgs, bins = 10, color = I("black"))

# test set
predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)
predicted_ratings

model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
model_1_rmse

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",
                                     RMSE = model_1_rmse ))
rmse_results

# validation set
val_predicted_ratings <- mu + validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)
val_predicted_ratings

val_model_1_rmse <- RMSE(val_predicted_ratings,validation$rating)
val_model_1_rmse

#rmse_results <- bind_rows(rmse_results,
#                          data_frame(method="Movie Effect Model - validation set",
#                                     RMSE = val_model_1_rmse ))
#rmse_results

rmse_results %>% knitr::kable()

# ---------------------------------------
# ---------------------------------------
# A Third model - Modeling movie effects + user effects
# $ Y_{u,i} = \mu + + b_i + b_u +\epsilon_{u,i}$
# ---------------------------------------
# ---------------------------------------

# plot user effect
train_set %>% 
  group_by(userId) %>% 
  filter(n()>=100) %>%
  summarize(b_u = mean(rating)) %>% 
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black")

# test set
user_avgs <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
model_2_rmse

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = model_2_rmse ))
rmse_results

# validation set
val_user_avgs <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

val_predicted_ratings <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(val_user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

val_model_2_rmse <- RMSE(val_predicted_ratings, validation$rating)
val_model_2_rmse

#rmse_results <- bind_rows(rmse_results,
#                          data_frame(method="Movie + User Effects Model - validation set",  
#                                     RMSE = val_model_2_rmse ))
#rmse_results
#rmse_results %>% knitr::kable()

# ---------------------------------------
# ---------------------------------------
# A Fourth model - Modeling movie effects + user + genres effects
# $ Y_{u,i} = \mu + + b_i + b_u +\epsilon_{u,i}$
# ---------------------------------------
# ---------------------------------------

# plot genres effect
train_set %>% 
  group_by(genres) %>% 
  filter(n()>=100) %>%
  summarize(b_g = mean(rating)) %>% 
  ggplot(aes(b_g)) + 
  geom_histogram(bins = 30, color = "black")

# test set
user_avgs <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

genres_avgs <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu - b_i - b_u))

predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genres_avgs, by='genres') %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  pull(pred)

model_3_rmse <- RMSE(predicted_ratings, test_set$rating)
model_3_rmse

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User + Genres Effects Model",  
                                     RMSE = model_3_rmse ))
rmse_results

# validation set
val_user_avgs <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

val_genres_avgs <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(val_user_avgs, by='userId') %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu - b_i - b_u))

val_predicted_ratings <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(val_user_avgs, by='userId') %>%
  left_join(val_genres_avgs, by='genres') %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  pull(pred)

val_model_3_rmse <- RMSE(val_predicted_ratings, validation$rating)
val_model_3_rmse

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User + Genres Effects Model - validation set",  
                                     RMSE = val_model_3_rmse ))
rmse_results
#rmse_results %>% knitr::kable()

# ---------------------------------------
# ---------------------------------------
# Regularization
# $\frac{1}{N}\sum_{u,i} (y_{u,i} - \mu - b_i)^2 + \gamma (\sum_i b^2_i + \sum_u b^2_u)$
# where $\gamma (\sum_i b^2_i + \sum_u b^2_u) $ penalty term
# ---------------------------------------
# ---------------------------------------

# test set
test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  mutate(residual = rating - (mu + b_i)) %>%
  arrange(desc(abs(residual))) %>% 
  select(title,  residual) %>% slice(1:10) %>% knitr::kable()

# validation set
validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  mutate(residual = rating - (mu + b_i)) %>%
  arrange(desc(abs(residual))) %>% 
  select(title,  residual) %>% slice(1:10) %>% knitr::kable()

# top movies
movie_titles <- edx %>% 
  select(movieId, title) %>%
  distinct()

movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i) %>% 
  slice(1:10) %>%  
  knitr::kable()

# worst movie
movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i) %>% 
  slice(1:10) %>%  
  knitr::kable()

# how many time they get rated in the training set for good
train_set %>% dplyr::count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()

# how many time they get rated in the training set for bad
train_set %>% dplyr::count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()

# new model
lambda <- 3.0

mu <- mean(train_set$rating)
mu 

movie_reg_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n()) 

# plot avg
data_frame(original = movie_avgs$b_i, 
           regularlized = movie_reg_avgs$b_i, 
           n = movie_reg_avgs$n_i) %>%
  ggplot(aes(original, regularlized, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.5)

# Best movie after b
train_set %>%
  dplyr::count(movieId) %>% 
  left_join(movie_reg_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()

# worst movie after b
train_set %>%
  dplyr::count(movieId) %>% 
  left_join(movie_reg_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()

# test set
predicted_ratings <- test_set %>% 
  left_join(movie_reg_avgs, by='movieId') %>%
  mutate(pred = mu + b_i) %>%
  .$pred

model_3_rmse <- RMSE(predicted_ratings, test_set$rating)
model_3_rmse

#rmse_results <- bind_rows(rmse_results,
#                          data_frame(method="Regularized Movie Effect Model",  
#                                     RMSE = model_3_rmse ))
#rmse_results

# validation set
val_predicted_ratings <- validation %>% 
  left_join(movie_reg_avgs, by='movieId') %>%
  mutate(pred = mu + b_i) %>%
  .$pred

val_model_3_rmse <- RMSE(val_predicted_ratings, validation$rating)
val_model_3_rmse

#rmse_results <- bind_rows(rmse_results,
#                          data_frame(method="Regularized Movie Effect Model - validation set",  
#                                     RMSE = val_model_3_rmse ))
#rmse_results
#rmse_results %>% knitr::kable()


# movie effect
# -------------------------------
lambdas <- seq(0, 10, 0.25)

mu <- mean(train_set$rating)
mu

just_the_sum <- train_set %>% 
  group_by(movieId) %>% 
  summarize(s = sum(rating - mu), n_i = n())

# test set
rmses <- sapply(lambdas, function(l){
  predicted_ratings <- test_set %>% 
    left_join(just_the_sum, by='movieId') %>% 
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    .$pred
  return(RMSE(predicted_ratings, test_set$rating))
})
rmses

qplot(lambdas, rmses)  
lambdas[which.min(rmses)] # is 2

# validation set
val_rmses <- sapply(lambdas, function(l){
  predicted_ratings <- validation %>% 
    left_join(just_the_sum, by='movieId') %>% 
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    .$pred
  return(RMSE(predicted_ratings, validation$rating))
})
val_rmses

qplot(lambdas, val_rmses)  
lambdas[which.min(val_rmses)] #is 2.25

# user effect
# ------------------------------------------
lambdas <- seq(0, 10, 0.25)

# test set
rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))

b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))

predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred

return(RMSE(predicted_ratings, test_set$rating))
})

qplot(lambdas, rmses)  

# for the full model using movie and user effect 
# the optimal lambda is 5
lambda <- lambdas[which.min(rmses)]
lambda

#rmse_results <- bind_rows(rmse_results,
#                          data_frame(method="Regularized Movie + User Effect Model",  
#                                     RMSE = min(rmses)))
#rmse_results

# validation set
val_rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- 
    validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  
  return(RMSE(predicted_ratings, validation$rating))
})

qplot(lambdas, val_rmses)  

# for the full model using movie and user effect 
# the optimal lambda is 5
val_lambda <- lambdas[which.min(val_rmses)]
val_lambda

#rmse_results <- bind_rows(rmse_results,
#                          data_frame(method="Regularized Movie + User Effect Model - validation set",  
#                                     RMSE = min(val_rmses)))
#rmse_results

#rmse_results %>% knitr::kable()

# Genres effect
# ------------------------------------------
lambdas <- seq(0, 10, 0.25)

# test set
rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  b_g <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - b_u - b_i - mu)/(n()+l))
  
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    mutate(pred = mu + b_i + b_u + b_g) %>%
    .$pred
  
  return(RMSE(predicted_ratings, test_set$rating))
})

qplot(lambdas, rmses)  

# for the full model using movie, user and genres effect 
# the optimal lambda is 4.75
lambda <- lambdas[which.min(rmses)]
lambda

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie + User + Genres Effect Model",  
                                     RMSE = min(rmses)))
rmse_results

# validation set
val_rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  b_g <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - b_u - b_i - mu)/(n()+l))
  
  predicted_ratings <- 
    validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    mutate(pred = mu + b_i + b_u + b_g) %>%
    .$pred
  
  return(RMSE(predicted_ratings, validation$rating))
})

qplot(lambdas, val_rmses)  

# for the full model using movie, user and genres effect 
# the optimal lambda is 5
val_lambda <- lambdas[which.min(val_rmses)]
val_lambda

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie + User + Genres Effect Model - validation set",  
                                     RMSE = min(val_rmses)))
rmse_results
write.csv(rmse_results,"rmse_results.csv", row.names = FALSE)

rmse_results %>% knitr::kable()

