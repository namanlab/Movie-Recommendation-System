##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(recosystem)) install.packages("recosystem", repos = "http://cran.us.r-project.org")
if(!require(ggcorrplot)) install.packages("ggcorrplot", repos = "http://cran.us.r-project.org")
if(!require(ggridges)) install.packages("ggridges", repos = "http://cran.us.r-project.org") 
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org") 
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(rmarkdown)) install.packages("rmarkdown", repos = "http://cran.us.r-project.org")

#installing tinytex (for report)
if(!"tinytex" %in% installed.packages()) { tinytex::install_tinytex()} 

#Loading the required packages:
library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(recosystem)
library(ggcorrplot)
library(ggridges)
library(tinytex)
library(kableExtra)
library(knitr)
library(gridExtra)
library(rmarkdown)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

#Creating a temporary file to store the downloaded data:
dl <- tempfile()

#Downloading the required file:
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)


#Extracting and reading the data:
ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)

colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
#movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
#                                           title = as.character(title),
#                                           genres = as.character(genres))
# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

#Final data set:
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

#Removing elements we don't require:
rm(dl, ratings, movies, test_index, temp, movielens, removed)


#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------

#Additional wrangling:
#Making changes in data sets to include year of movie release (from movie-title), month of rating (from timestamp), and year of rating (from timestamp)
#These variables will be accounted for exploratory analysis and also to develop new algorithms.
#We won't use genre effect in model

edx <- edx %>% mutate(month_rating = month(as_datetime(timestamp)), 
                      year_release = as.numeric(str_sub(title, -5, -2)),
                      year_rating = year(as_datetime(timestamp)),
                      years_lapsed = year_rating - year_release) %>%
  select(-c("genres", "timestamp", "title"))

validation <- validation %>% mutate(month_rating = month(as_datetime(timestamp)), 
                                    year_release = as.numeric(str_sub(title, -5, -2)),
                                    year_rating = year(as_datetime(timestamp)),
                                    years_lapsed = year_rating - year_release) %>%
  select(-c("genres", "timestamp", "title"))
#------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------

#Exploratory Data Analysis:


#Summary statistics
summary(edx) 
#Structure of data set
str(edx) 


#Correlation heat map of features:
ggcorrplot::ggcorrplot(cor(edx), hc.order = TRUE, type = "lower",
                       lab = TRUE, legend.title = "Correlation") +
  theme(plot.title = element_text(hjust = 0.5)) +                   #Specifying relevant details in plot
  labs(title="Correlation Heatmap for Selected Features",
       caption="0 indicates that the correlation is very small")


#Number of unique movies and users in data set
edx %>% 
  summarise(number_of_users = n_distinct(userId),
            nunmber_of_movies = n_distinct(movieId))


#Distribution of Movies by Frequency of Ratings
p1 <- edx %>% 
  dplyr::count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black", fill = "firebrick") + 
  scale_x_log10() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs(title = "Distribution of Movies by \n Frequency of Ratings",
       x = "MovieId",
       y = "Frequency of Ratings")
#Some movies get rated  more than others.


#Distribution of Users by Frequency of Ratings
p2 <- edx %>%
  dplyr::count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black", fill =" firebrick") + 
  scale_x_log10() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Distribution of Users by \n Frequency of Ratings",
       x = "UserId",
       y = "Frequency of Ratings")
#Some users are more active than others.

#grid plot of p1 and p2:
grid.arrange(p1, p2, ncol = 2)

#Removing variables we don't require any more: 
#This will be done every time to free up space in Global Environment and increase the computational time.
rm("p1", "p2")


#Count by no. of ratings
edx %>% 
  ggplot(aes(rating)) + 
  geom_histogram(color = "black", fill="firebrick") +
  scale_y_continuous(labels = scales::comma) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Distribution of Ratings",
       x = "Ratings",
       y = "Frequency")
#Users have higher tendency to rate in whole number.
#Users tend to give higher ratings in general (mostly 3 or4)



#Variation of Ratings with Month of Ratings (ridge plot)
edx %>% 
  ggplot(aes(x=rating, y = factor(month_rating))) + 
  geom_density_ridges(fill = "firebrick") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Variation of Ratings with Month of Rating",
       x = "Rating",
       y = "Month of Rating")
#Ratings don't vary much across months


#Variation of ratings with Year of Ratings (histogram)
edx %>% 
  ggplot(mapping = aes(x = year_rating, fill = factor(round(rating)))) + 
  geom_histogram() + 
  scale_fill_brewer(palette="YlOrRd")+ 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Variation of Ratings with Years of Ratings",
       x = "Year of Ratings",
       y = "Frequency of Ratings",
       fill = "Ratings (rounded)")
#The variations show that the year of rating must be an important factor


#Density plot of Release Year of Movie against Ratings: (stacked density plot)
edx %>% 
  ggplot(aes(year_release,  fill= factor(round(rating)))) + 
  geom_density(position="fill", adjust=7) + 
  scale_fill_brewer(palette="YlOrRd") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Variation of Ratings with Year of Release",
       x = "Year of Release",
       y = "Density",
       fill = "Ratings (rounded)")
#Older movies tend to have higher proportion of extreme ratings (5 or 0)


#Density plot of Years Lapsed against Ratings: (stacked density plot)
edx %>% 
  ggplot(aes(years_lapsed,  fill = factor(round(rating)))) + 
  geom_density(position="fill", adjust=7) + 
  scale_fill_brewer(palette = "YlOrRd") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Variation of Ratings with Years Lapsed",
       x = "Years Lapsed",
       y = "Density",
       fill = "Ratings (rounded)")
#Greater Years lapsed for a movie tends to result in a higher proportion of extreme ratings (5 or 0)

#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------

#Defining RMSE:

#The true ratings: ratings of validation set (we want to predict)
true_ratings <- validation$rating
#The RMSE function:
RMSE <- function(predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}
#------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------

#MACHINE LEARNING PREDICTIONS:


#UNREGULARIZED APPROACH:
#------------------------------------------------------------------------------------------------------------

#1) Model_1: Baseline Model:
#Defining mean ratings (mu):
mu <- mean(edx$rating)

#Applying the model on validation set:
predicted_ratings_model_1 <- validation %>%
  mutate(pred = mu ) %>%
  pull(pred)

#Calculating the RMSE:
model_1_rmse <- RMSE(predicted_ratings_model_1) 
model_1_rmse

#Removing variables we don't require any more: 
rm("predicted_ratings_model_1")

#------------------------------------------------------------------------------------------------------------

#2)Model_2: Baseline + MovieId effect:

#Defining movie effect:
movie_avg <- edx %>% 
  group_by(movieId) %>%
  summarise(b_i = mean(rating-mu))

#Applying the model on validation set:
predicted_ratings_model_2 <- validation %>%
  left_join(movie_avg, by = "movieId") %>%
  mutate(pred = mu + b_i) %>%
  pull(pred)

#Calculating the RMSE:
model_2_rmse <- RMSE(predicted_ratings_model_2)
model_2_rmse

#Removing variables we don't require any more:
rm("predicted_ratings_model_2")

#Plotting movie effect against the movieId:
movie_avg %>% 
  ggplot(aes(movieId, b_i)) + 
  geom_point() +
  scale_x_log10(labels = scales::comma) + 
  geom_smooth(col = "red") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Variation of Movie Effect",
       x = "MovieId",
       y = "Movie Effect (b_i)")
       
#------------------------------------------------------------------------------------------------------------

#3) Model_3: Baseline + MovieId + UserId effect:

#Defining user effect:
user_avg <- edx %>%
  left_join(movie_avg, by="movieId") %>%
  group_by(userId) %>%
  summarise(b_u = mean(rating-mu-b_i))

#Applying the model on validation set:
predicted_ratings_model_3 <- validation %>%
  left_join(movie_avg, by = "movieId") %>%
  left_join(user_avg, by = "userId") %>%
  mutate(pred = mu + b_i + b_u ) %>%
  pull(pred)

#Calculating the RMSE:
model_3_rmse<-RMSE(predicted_ratings_model_3)
model_3_rmse

#Removing variables we don't require any more:
rm("predicted_ratings_model_3")

#Plotting user effect against userId:
user_avg %>% 
  ggplot(aes(userId, b_u)) + 
  geom_point() +
  scale_x_continuous(labels = scales::comma) + 
  geom_smooth(col = "red") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Variation of User Effect",
       x = "UserId",
       y = "User Effect (b_u)")

#------------------------------------------------------------------------------------------------------------

#4) Model_4: Baseline + MovieId + UserId effect + month of rating effect:

#Defining month of rating effect:
month_rating_effect <- edx %>%
  left_join(movie_avg, by = "movieId") %>%
  left_join(user_avg, by = "userId") %>%
  group_by(month_rating) %>%
  summarise(b_m = mean(rating-mu-b_i-b_u))

#Applying the model on validation set:
predicted_ratings_model_4 <- validation %>%
  left_join(movie_avg, by = "movieId") %>%
  left_join(user_avg, by = "userId") %>%
  left_join(month_rating_effect, by = "month_rating") %>%
  mutate(pred = mu + b_i + b_u + b_m ) %>%
  pull(pred)

#Calculating the RMSE:
model_4_rmse <- RMSE(predicted_ratings_model_4) 
model_4_rmse

#Removing variables we don't require any more:
rm("predicted_ratings_model_4")

#Plotting month of rating effect against month:
month_rating_effect %>% 
  ggplot(aes(month_rating, b_m)) + 
  geom_point() + 
  geom_smooth(col="red") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Variation of Month of Rating Effect",
       x = "Month",
       y = "Month of Rating Effect (b_m)")

#------------------------------------------------------------------------------------------------------------

#5) Model_5: Baseline + MovieId + UserId effect + month of rating effect + year release effect:

#Defining year of release effect:
year_release_effect <- edx %>%
  left_join(movie_avg, by="movieId") %>%
  left_join(user_avg, by="userId") %>%
  left_join(month_rating_effect, by="month_rating") %>%
  group_by(year_release) %>%
  summarise(b_y = mean(rating-mu-b_i-b_u-b_m))

#Applying the model on validation set:
predicted_ratings_model_5 <- validation %>%
  left_join(movie_avg, by="movieId") %>%
  left_join(user_avg, by="userId") %>%
  left_join(month_rating_effect, by="month_rating") %>%
  left_join(year_release_effect, by="year_release") %>%
  mutate(pred = mu + b_i + b_u + b_m +b_y) %>%
  pull(pred)

#Calculating the RMSE:
model_5_rmse <- RMSE(predicted_ratings_model_5)
model_5_rmse

#Removing variables we don't require any more:
rm("predicted_ratings_model_5")

#Plotting year of release effect against year:
year_release_effect %>%
  ggplot(aes(year_release, b_y)) + 
  geom_point() + 
  geom_smooth(col="red") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Variation of Year of Release Effect",
       x = "Year",
       y = "Year of Release Effect (b_y)")

#------------------------------------------------------------------------------------------------------------

#6) Model_6: Baseline + MovieId + UserId effect + month of rating effect + year release effect + year_of_rating:

#Defining year of rating effect:
year_rating_effect <- edx %>%
  left_join(movie_avg, by="movieId") %>%
  left_join(user_avg, by="userId") %>%
  left_join(month_rating_effect, by="month_rating") %>%
  left_join(year_release_effect, by="year_release") %>% 
  group_by(year_rating) %>%
  summarise(b_yr = mean(rating-mu-b_i-b_u-b_m-b_y))

#Applying the model on validation set:
predicted_ratings_model_6 <- validation %>%
  left_join(movie_avg, by="movieId") %>%
  left_join(user_avg, by="userId") %>%
  left_join(month_rating_effect, by="month_rating") %>%
  left_join(year_release_effect, by="year_release") %>%
  left_join(year_rating_effect, by="year_rating") %>%
  mutate(pred = mu + b_i + b_u + b_m + b_y + b_yr) %>%
  pull(pred)

#Calculating the RMSE:
model_6_rmse <- RMSE(predicted_ratings_model_6)  
model_6_rmse

#Removing variables we don't require any more:
rm("predicted_ratings_model_6")

#Plotting year of rating effect against year:
year_rating_effect %>% 
  ggplot(aes(year_rating, b_yr)) + 
  geom_point() + 
  geom_smooth(col="red") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Variation of Year of Rating Effect",
       x = "Year",
       y = "Year of Rating Effect (b_yr)")

#------------------------------------------------------------------------------------------------------------

#7) Model_7: Baseline + MovieId + UserId effect + month of rating effect + year release effect + year_of_rating + years_lapsed effect:

#Defining year lapsed effect:
years_lapsed_effect <- edx %>%
  left_join(movie_avg, by="movieId") %>%
  left_join(user_avg, by="userId") %>%
  left_join(month_rating_effect, by="month_rating") %>%
  left_join(year_release_effect, by="year_release") %>% 
  left_join(year_rating_effect, by="year_rating") %>%
  group_by(years_lapsed) %>%
  summarise(b_yl = mean(rating-mu-b_i-b_u-b_m-b_y-b_yr))

#Applying the model on validation set:
predicted_ratings_model_7 <- validation %>%
  left_join(movie_avg, by="movieId") %>%
  left_join(user_avg, by="userId") %>%
  left_join(month_rating_effect, by="month_rating") %>%
  left_join(year_release_effect, by="year_release") %>%
  left_join(year_rating_effect, by="year_rating") %>%
  left_join(years_lapsed_effect, by="years_lapsed") %>%
  mutate(pred = mu + b_i + b_u + b_m + b_y + b_yr + b_yl) %>%
  pull(pred)

#Calculating the RMSE:
model_7_rmse <- RMSE(predicted_ratings_model_7)  
model_7_rmse

#Removing variables we don't require any more:
rm("predicted_ratings_model_7")

#Plotting years lapsed effect against year:
years_lapsed_effect %>% 
  ggplot(aes(years_lapsed, b_yl)) + 
  geom_point() + 
  geom_smooth(col="red") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Variation of Years Lapsed Effect",
       x = "Years Lapsed",
       y = "Years Lapsed Effect (b_yl)")

#---------------------------------------------------------------------------------------
#Removing variables we don't require any more:
rm("movie_avg", "user_avg", "month_rating_effect", "year_release_effect", "year_rating_effect", "mu", "years_lapsed_effect")
#---------------------------------------------------------------------------------------


#Model_8: REGULARIZED APPROACH: For model 7, controlled for error term.


#Determine lambda (l) using cross validation:
#Splitting edx into edx_train and edx_test:

set.seed(820, sample.kind = "Rounding")
#Creating the text index
edx_test_index <- createDataPartition(edx$rating, times=1, p=0.2, list = FALSE)

#Splitting appropriately
edx_train <- edx[-edx_test_index,] #used for training
edx_test <- edx[edx_test_index,] #used for tuning

#To ensure only those movies and users are present in edx_test for which we have observations in edx_train
edx_test <- edx_test %>% 
  semi_join(edx_train, by = "movieId") %>%
  semi_join(edx_train, by = "userId")


#Computing the accuracy on edx_test for different values of l and determining the appropriate lambda
#(note: we cannot use validation set for determining lambda as validation set can be used only for final testing (not tuning the parameters))

#Defining a sequence of values for lambda(l)
l <- seq(0, 10, 1)

#Calculating different values of RMSE for different values of l in edx_test:
rmses <- sapply(l, function(l){
  #Mean Ratings:
  mu <- mean(edx_train$rating)
  #Movie Effect:
  movie_avg <- edx_train %>% 
    group_by(movieId) %>%
    summarise(b_i = sum(rating-mu)/(n()+l))
  #User Effect
  user_avg <- edx_train %>%
    left_join(movie_avg, by="movieId") %>%
    group_by(userId) %>%
    summarise(b_u = sum(rating-mu-b_i)/(n()+l))
  #Month of Rating Effect:
  month_rating_effect <- edx_train %>%
    left_join(movie_avg, by="movieId") %>%
    left_join(user_avg, by="userId") %>%
    group_by(month_rating) %>%
    summarise(b_m = sum(rating-mu-b_i-b_u)/(n()+l))
  #Year of Release Effect:
  year_release_effect <- edx_train %>%
    left_join(movie_avg, by="movieId") %>%
    left_join(user_avg, by="userId") %>%
    left_join(month_rating_effect, by="month_rating") %>%
    group_by(year_release) %>%
    summarise(b_y = sum(rating-mu-b_i-b_u-b_m)/(n()+l))
  #Year of Rating Effect:
  year_rating_effect <- edx_train %>%
    left_join(movie_avg, by="movieId") %>%
    left_join(user_avg, by="userId") %>%
    left_join(month_rating_effect, by="month_rating") %>%
    left_join(year_release_effect, by="year_release") %>% 
    group_by(year_rating) %>%
    summarise(b_yr = sum(rating-mu-b_i-b_u-b_m-b_y)/(n()+l))
  #Years Lapsed Effect:
  years_lapsed_effect <- edx_train %>%
    left_join(movie_avg, by="movieId") %>%
    left_join(user_avg, by="userId") %>%
    left_join(month_rating_effect, by="month_rating") %>%
    left_join(year_release_effect, by="year_release") %>% 
    left_join(year_rating_effect, by="year_rating") %>%
    group_by(years_lapsed) %>%
    summarise(b_yl = sum(rating-mu-b_i-b_u-b_m-b_y-b_yr)/(n()+l))
  #Applying the model on edx_test set:
  predicted_ratings <- edx_test %>%
    left_join(movie_avg, by="movieId") %>%
    left_join(user_avg, by="userId") %>%
    left_join(month_rating_effect, by="month_rating") %>%
    left_join(year_release_effect, by="year_release") %>%
    left_join(year_rating_effect, by="year_rating") %>%
    left_join(years_lapsed_effect, by="years_lapsed") %>%
    mutate(pred = mu + b_i + b_u + b_m + b_y + b_yr + b_yl) %>%
    pull(pred)
  #Calculating the RMSE:
  sqrt(mean((edx_test$rating - predicted_ratings)^2))
})

#plotting RMSE against lambda (l)
data.frame(l = l, RMSE = rmses) %>%
  ggplot(aes(l, RMSE)) + geom_point() + 
  geom_line()  + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Variation of RMSE (Cross-Validation) with \u03bb",
       x = "\u03bb",
       y = "RMSE")

l[which.min(rmses)]
#lambda = 5 maximises accuracy on edx_test

#Removing variables we don't require any more:
rm("edx_test_index", "edx_test", "edx_train")

#Implementing the required value of l and getting final RMSE on validation set:
l <- l[which.min(rmses)]
#Mean Ratings:
mu <- mean(edx$rating)

#Movie Effect:
movie_avg <- edx %>% 
  group_by(movieId) %>%
  summarise(b_i = sum(rating-mu)/(n()+l))

#User Effect:
user_avg <- edx %>%
  left_join(movie_avg, by="movieId") %>%
  group_by(userId) %>%
  summarise(b_u = sum(rating-mu-b_i)/(n()+l))

#Month of Rating Effect:
month_rating_effect <- edx %>%
  left_join(movie_avg, by="movieId") %>%
  left_join(user_avg, by="userId") %>%
  group_by(month_rating) %>%
  summarise(b_m = sum(rating-mu-b_i-b_u)/(n()+l))

#Year of Release Effect:
year_release_effect <- edx %>%
  left_join(movie_avg, by="movieId") %>%
  left_join(user_avg, by="userId") %>%
  left_join(month_rating_effect, by="month_rating") %>%
  group_by(year_release) %>%
  summarise(b_y = sum(rating-mu-b_i-b_u-b_m)/(n()+l))

#Year of Rating Effect:
year_rating_effect <- edx %>%
  left_join(movie_avg, by="movieId") %>%
  left_join(user_avg, by="userId") %>%
  left_join(month_rating_effect, by="month_rating") %>%
  left_join(year_release_effect, by="year_release") %>% 
  group_by(year_rating) %>%
  summarise(b_yr = sum(rating-mu-b_i-b_u-b_m-b_y)/(n()+l))

#Years Lapsed Effect:
years_lapsed_effect <- edx %>%
  left_join(movie_avg, by="movieId") %>%
  left_join(user_avg, by="userId") %>%
  left_join(month_rating_effect, by="month_rating") %>%
  left_join(year_release_effect, by="year_release") %>% 
  left_join(year_rating_effect, by="year_rating") %>%
  group_by(years_lapsed) %>%
  summarise(b_yl = sum(rating-mu-b_i-b_u-b_m-b_y-b_yr)/(n()+l))

#Applying the model on validation set:
predicted_ratings_regularization <- validation %>%
  left_join(movie_avg, by="movieId") %>%
  left_join(user_avg, by="userId") %>%
  left_join(month_rating_effect, by="month_rating") %>%
  left_join(year_release_effect, by="year_release") %>%
  left_join(year_rating_effect, by="year_rating") %>%
  left_join(years_lapsed_effect, by="years_lapsed") %>%
  mutate(pred = mu + b_i + b_u + b_m + b_y + b_yr + b_yl) %>%
  pull(pred) 

#Calculating the RMSE:
rmse_regularization <- RMSE(predicted_ratings_regularization)
rmse_regularization

#Removing variables we don't require any more:
rm("predicted_ratings_regularization")
rm("movie_avg", "user_avg", "month_rating_effect", "year_release_effect", "year_rating_effect", "mu", "l", "years_lapsed_effect")

#---------------------------------------------------------------------------------------

#Model_9: Matrix Factorization:

set.seed(820, sample.kind = "Rounding")

# Converting the 'edx' and 'validation' sets to the recosystem input format (data_memory specifies a data set from R objects)
edx_reco <-  with(edx, data_memory(user_index = userId, 
                                   item_index = movieId, 
                                   rating = rating))
validation_reco  <-  with(validation, data_memory(user_index = userId, 
                                                  item_index = movieId, 
                                                  rating = rating))

# Creating the model object (reco() is used for constructing a Recommender System Object)
r <-  recosystem::Reco()

# Training the required model (outputting the factorization matrices)
# nthreads: number of threads for parallel computing:
r$train(edx_reco, opts = c(nthread = 4))

# Calculating the prediction ratings: (out_memory(): Result should be returned as R objects)
predicted_ratings_reco <-  r$predict(validation_reco, out_memory())

#Calculating the RMSE:
rmse_reco <- RMSE(predicted_ratings_reco)
rmse_reco

#Removing variables we don't require any more:
rm("predicted_ratings_reco", "r", "edx_reco", "validation_reco", "true_ratings")

#------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------

#Results:

#Creating the Required Data frame of all model results:
rmse_results <- data.frame(Model_1 = model_1_rmse,
                         Model_2 = model_2_rmse,
                         Model_3 = model_3_rmse,
                         Model_4 = model_4_rmse,
                         Model_5 = model_5_rmse,
                         Model_6 = model_6_rmse,
                         Model_7 = model_7_rmse,
                         Model_8 = rmse_regularization,
                         Model_9 = rmse_reco) %>% as.matrix() %>% t() %>% as.data.frame()
rownames(rmse_results) <- NULL #Removing row names
Models<-c("Intermediate Model 1", "Intermediate Model 2", "Intermediate Model 3", "Intermediate Model 4", "Intermediate Model 5", "Intermediate Model 6", "Intermediate Model 7", "Hybrid Model", "Collaborative Filtering Model")
rmse_results<-cbind(Models, rmse_results) #Adding a column to indicate the name of the model.
colnames(rmse_results)<-c("Model", "RMSE") #Setting the column names
rm("Models") #Removing variable we don't require
rmse_results <- rmse_results %>% arrange(desc(RMSE)) #Arranging by RMSE


#Final Lollipop chart:

rmse_results %>%
  ggplot(aes(x=Model, y=RMSE, label=(round(RMSE, 5)) )) +
  geom_segment(aes(x=Model, xend=Model, y=0, yend=RMSE)) +   #To define a line segment (for the plot)
  geom_point() +
  geom_point(size=5, color="firebrick", fill=alpha("orange", 0.3), alpha=0.7) + 
  geom_text( position = position_nudge(y = 0.12), size=3.5) +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size=9),
        axis.text.y = element_text(size=9)) +
  labs(title="Validation Set RMSE by Different Models",
       x="Model",
       y="RMSE on Validation Set")


