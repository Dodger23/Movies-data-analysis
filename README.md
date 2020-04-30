---
title: "The 1M$ movie idea"
author: "Tarek Hassan"
date: "4/30/2020"
output: html_document
---

```{r setup}
```

## This project is just for fun
Using a movies reviews by users dataset provided by <https://grouplens.org/datasets/movielens/latest/> We will try (by analyzing the data) to predict the most profitable movie we can produce to have a lot of mony

I should mention that this is the **1 MB** developing data, which is **Small**: **100,000 ratings** and **3,600 tag** applications applied to **9,000 movies** by **600  users.** Last updated **9/2018**.

They also provide 265 MB data which I would love to work on, but my labtop won't :(

The following 7 chunks of code will be the booring cleaning and preproccessing the data 
But basicly what we will do is to find the best genre from the rates and views, then take the best movies in this genre and see what tags they have 

first of all the libraries we will use
```{r message=FALSE, , eval=TRUE, results='hide'}
library(reshape2)
library(ggplot2)
library(hrbrthemes)
library(stringr)
library(dplyr)
library(wordcloud2) 
options(scipen = 999)
```

## Reading the Data

The data provided by 3 csv files which can know more about it from 
but here is a brief view from each table 
```{r , eval=TRUE}
movies = read.csv("Data/movies.csv")
ratings = read.csv("Data/ratings.csv")
tags = read.csv("Data/tags.csv")
head(movies)
head(ratings)
head(tags)
```

## Classing the data 

assigning data columns classes to the right classes
```{r , eval=TRUE}
#Classing data
ratings$timestamp = as.POSIXct(ratings$timestamp , origin = '1970-1-1' , tz = "UTC") 
tags$timestamp = as.POSIXct(tags$timestamp , origin = '1970-1-1' , tz = "UTC") 
tags$tag = as.character(tags$tag)
tags$movieId = as.character(tags$movieId)
movies$title = as.character(movies$title)
movies$genres = as.character(movies$genres)

#for future use 
movies2 = movies
```

## Cleaning the movies data 
Seperating the movie title from the year

```{r , eval=TRUE}
#cleaning data
year = vector()
for(i in 1:nrow(movies))
{
  length =  str_length(movies[i,"title"] ) 
  year = c(year, 
           substr(as.character(movies[i  ,"title"]) ,
                  length - 4 , length -1
           )  
  )
  movies[i,"title"] = substr(movies[i, "title"], 1 ,
                             length - 7)
}
movies$year  = year 
head(movies)
```

creating a new data frame to contain all the genres with the total rate , views for each one
```{r , eval=TRUE}
#creating a new data frame to contain all the genres with the total rate , views for each one
genres = data.frame(genre = "", 
                    rate =0 , 
                    views = 0 , 
                    year = 0
                    )
```

Looping through each movie's genres and add 1 to views of that genre and add the rate of the movie to the rate of the genre 
```{r , eval=TRUE}
#
for(i in 1:nrow(ratings) )
{
  id = ratings[i , "movieId"] 
  rate = ratings[i, "rating"]
  year = format(as.Date(as.POSIXct( ratings[i , "timestamp"] , tz = "UTC")), "%Y") 
  g = strsplit(  movies[movies$movieId==id , "genres"]  , "\\|")
  for(j in g[[1]] )
  {
   
    if(sum(genres$year==year)>=1 & sum(genres$genre == j))
    {
      genres[genres$genre == j , "views"] = genres[genres$genre == j , "views"] + 1
      genres[genres$genre == j  , "rate"] = genres[genres$genre == j , "rate"] + rate
    }
    else
    {
      r = data.frame(genre = j , rate = rate , views = 1, year = year)
      genres = rbind(genres , r)
    }
  }
}
```

The "genres" dataframe after some cleaning 
```{r , eval=TRUE}
#erasing The temp row and filtering the no genres 
genres = genres[2:nrow(genres), ]
genres = genres %>% filter(genre != "(no genres listed)")
#show head of genres
head(genres)
```

## ----------------------------------------------------------------------------------
## ----------------------------------------------------------------------------------

Now let's say that we want to determine our movie's genre.
Maybe we want to know which genre has the best rate, because that means people will love our movie, right ?
Let's see the all the genres rates
```{r , eval=TRUE , echo=FALSE}
genres_rates = genres[, !names(genres) %in% "year"]
genres_views = genres_rates
```
summing rows with the same genre together by summing the rate and views
```{r , eval=TRUE}
genres_rates = as.data.frame(genres_rates %>% group_by(genre) %>% summarise_all(sum) ) 
#calculating the rate by : sum of rates / number of reviews
genres_rates$rate = genres_rates$rate / genres_rates$views

```

```{r warning=FALSE, , eval=FALSE , results='hide'}

ggplot(genres_rates, aes(x= reorder(genre , rate ), y=rate , fill = genre)) + 
  geom_bar(stat = "identity") +
  labs(x = "Genre" , y= "Rate"  )+
  ggtitle("Comparing with the rate of each genre accorfing to the rate itself")+
  theme_ft_rc() +
  theme(axis.text.x = element_text(angle = 90) )

```
![genres-rates-rate](https://raw.githubusercontent.com/Dodger23/The-movie-project/develop/images/genres-rates-rate.png)


But who wants the people to love his movie !! , we're talking money**$$$** here 
So let's reorder the genres by the number of views
```{r warning=FALSE, , eval=FALSE, results='hide'}
ggplot(genres_rates, aes(x= reorder(genre , views ), y=rate , fill = genre)) + 
  geom_bar(stat = "identity") +
  labs(x = "Genre" , y= "Rate"  )+
  ggtitle("Comparing with the rate of each genre accorfing to the views")+
  theme_ft_rc() +
  theme(axis.text.x = element_text(angle = 90) )
```
![genres-rates-views](https://raw.githubusercontent.com/Dodger23/The-movie-project/develop/images/genres-rates-views.png)


Okay okay let's calm down for a moment,  because the diffirence in rates are very little, let's just compare all genres by the number of views
```{r , eval=FALSE , echo =FALSE}
genres_views = as.data.frame(genres_views %>% group_by(genre) %>% summarise_all(sum) ) 

```

```{r warning=FALSE, eval=FALSE, results='hide'}
# plot genre and views 
ggplot(genres_views, aes(x= reorder(genre , views ), y=views , fill = genre)) + 
  geom_bar(stat = "identity") +
  labs(x = "Genre" , y= "Views"  )+
  ggtitle("Comparing with the views of each genre")+
  theme_ft_rc() +
  theme(axis.text.x = element_text(angle = 90) )
```
![genres-views](https://raw.githubusercontent.com/Dodger23/The-movie-project/develop/images/genres-views.png)


Yes we want money, but we also need people to like our movie so maybe they will watch it again and again and.
I'm saying this because "Comedy" is more in views numbers, but "Drama" is so close from it and "Drama" also ahead of "Comedy" in rate

So maybe we need to consider making our movie's genre "Drama"

## ----------------------------------------------------------------------------------
## ----------------------------------------------------------------------------------

Okay Now our movie main genre is "Drama", but what's the main keywords it will include ? 

luckly we have a tag table includes the movie id and the tag, so all we need to do is to get all the movies under "Drama" genre and see what tags they have

let's collect the ids from the movies table 
```{r , eval=TRUE}
# collecting "Drama" movies ids by checking if the movie has the word "Drama" in its genre
ids = vector()
for(i in 1:nrow(movies2))
{
  sp = strsplit(movies2[i , "genres"] , "\\|")[[1]]
  if("Drama" %in% sp)
  ids = c(ids , movies2[i , "movieId"])
}
```


We want the best "Drama" movies by rate, so we will filter the ratings table to take :
- movie id , movie rating columns 
- movies which has id in our collected ids so we know it's "Drama"
- group that by the movie, because there was more than one rating for each movie 
- take the mean of the grouped movie rates
- keep only movies have 5 start rate
```{r , eval=TRUE}
drama = ratings[, names(ratings) %in% c("movieId" , "rating")]
drama = as.data.frame(drama %>% filter(movieId %in% ids ) 
                      %>% group_by(movieId)
                      %>% summarise_all(mean)
                        )

drama = drama[order( - drama$rating) , ]
drama = drama[drama$rating == 5, ]
head(drama)
```


Now let's Store the choosen movies titles to plot them 
```{r , eval=TRUE }
temp = data.frame(movie = movies2[movies2$movieId %in% drama$movieId , "title"] )
temp$freq = round(runif(nrow(temp) , 1 , 20))

```

And hereis a word cloud of some of the choosen movies 
```{r , eval=FALSE , results='hide'}
wordcloud2( temp )
```
![movies-wordcloud](https://raw.githubusercontent.com/Dodger23/The-movie-project/develop/images/movies-wordcloud.png)

Now let's take the tags the have 
```{r , eval=TRUE}
drama$movieId = as.character(drama$movieId)
filt = c("In Netflix queue" , "free to download" )
tags = tags %>% filter(movieId %in% drama$movieId & ! tag %in% filt )
tags = tags[order(tags$timestamp) , ]
tags = tags[1 :nrow(tags), "tag"]
temp = data.frame(word = tags)
head(temp)
```
 and plot them 
```{r , eval=FALSE , results='hide'}
temp$freq = round(runif(nrow(temp) , 1 ,10))
wordcloud2( temp )
```
![tags-wordcloud](https://raw.githubusercontent.com/Dodger23/The-movie-project/develop/images/tags-wordcloud.png)


So we finally have our movie, it'll be an atmospheric, no dialogue, dystopian, harsh, disturbing and bleak gritty movie involve a story of creaetivity and imagination that happens or involves "England"

Hmmm, Not very much the expected.... but I guess the internet has a very weird taste


We made it, We have our movie, we will be millionaires .... The only problem is we need money first to produce the movie :D
