# libraries used

library(recommenderlab)
library(tidyverse)
library(ggplot2)
library(caTools)
library(knitr)
library(reshape2)

# first lets read the dataset

Reviews <- read.csv("Reviews.csv")
str(Reviews)
summary(Reviews) # no missing values!!!

# we will do some EDA and preprocessing. We then will build the sparse matrix and start building our recommender system
# since we are doing collaborative filtering. The only data of interest to us is the Score values

# let us find out the distribution of ratings

ratings <- Reviews$Score
table(ratings)

barplot(table(ratings),main = "Distribution of Ratings",ylab = "Count",xlab = "Ratings")


# let us count the number of products
# we need to group the Review dataset by product and get the no of rows of that data frame
prdct_groupby <- Reviews %>%
                  group_by(ProductId) %>%
                  summarise(count = n())
total_no_prdcts <- nrow(prdct_groupby)

# there are 74258 products

# lets count the number of users. Same algo used for counting the no of products
user_groupby <- Reviews %>%
                  group_by(UserId) %>%
                  summarise(count = n())
total_no_users <- nrow(user_groupby)

# there are 256059 users in our dataset

# let us see the distribution of user ratings i.e how many time each user has rated.

# we can try to remove users who do not contribute much to the ratings as they might have rated very less times.

ggplot(user_groupby,aes(x = count)) + geom_histogram()

# the histogram is really skewed.
# by the histogram we see that most users have rated 1 time. So it is probably not a good idea to remove users.

# let us sort the data and see the numbers for ourself.

user_groupby_sorted <- user_groupby[order(user_groupby$count,decreasing = TRUE),]
head(user_groupby_sorted)

# let us see the proportion of the users who rated once. A number will be confirming.
# I am going to do this in a long way. Please bear with me!

# get all users who scored more than once
users_rated_more_than_once <- user_groupby %>%
                                filter(count > 1)
no_of_users_rated_more_than_once <- nrow(users_rated_more_than_once)

# get all users who scored once
user_rated_once <- user_groupby %>%
                    filter(count == 1)
no_of_user_rated_once <- nrow(user_rated_once)

# the proportion of users who rated once
prop_rate_once <- (no_of_user_rated_once / total_no_users)*100
prop_rate_once

# 68.5% of people of users rated the products once!!! I don't think it is a good idea to remove all of these users.
# Our matrix is gonna be soooooooooo sparse xD

# lets drop the unneeded columns

Reviews_data <- select(Reviews,-c("Id","ProfileName","HelpfulnessNumerator","HelpfulnessDenominator","Time","Summary","Text"))

# clean up the userid

Reviews_data$UserId <- gsub("#oc-",'',Reviews_data$UserId)

# we have a lot of data. It would be useful if we drop all the products who have reviews less than median count of reviews

median_prdct <- median(prdct_groupby$count)

# drop all products with count less than median

# lets merge the count with the dataframe

Reviews_data <- merge(Reviews_data,prdct_groupby,by.x = 'ProductId',by.y = 'ProductId',all.x=T)

# now use the count column to drop the values

Reviews_data <- Reviews_data %>%
                  filter(count >= median_prdct)

# remove the unneeded columns
Reviews_data <- Reviews_data[,c(1,2,3)]

# remove duplicates
Reviews_data <- Reviews_data[!duplicated(Reviews_data[,c(1,2)]),]

# have a look at the data
head(Reviews_data) 

# now we need to train the model
# convert the data to a matrix

Reviews_data.matrix <- as.matrix(acast(Reviews_data,UserId ~ ProductId))
