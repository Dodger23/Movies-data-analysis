library(reshape2)
library(ggplot2)
library(hrbrthemes)
library(stringr)
library(dplyr)
library(wordcloud2) 

options(scipen = 999)
movies = read.csv("Data/movies.csv")
ratings = read.csv("Data/ratings.csv")
tags = read.csv("Data/tags.csv")

#cleaning data
ratings$timestamp = as.POSIXct(ratings$timestamp , origin = '1970-1-1' , tz = "UTC") 
tags$timestamp = as.POSIXct(tags$timestamp , origin = '1970-1-1' , tz = "UTC") 
tags$tag = as.character(tags$tag)
tags$movieId = as.character(tags$movieId)
movies$title = as.character(movies$title)
movies$genres = as.character(movies$genres)



### Could be vectorized ------------

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


genres = data.frame(genre = "", 
                    rate =0 , 
                    views = 0 , 
                    year = 0
                    )

for(i in 1:nrow(ratings) )
{
  print(i) 
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

#erasing The temp row 
genres = genres[2:nrow(genres), ]
genres = genres %>% filter(genre != "(no genres listed)")


genres_rates = genres[, !names(genres) %in% "year"]
genres_views = genres_rates


genres_rates = as.data.frame(genres_rates %>% group_by(genre) %>% summarise_all(sum) ) 
genres_rates$rate = genres_rates$rate / genres_rates$views

# plot genre and rate
png(filename = "images/genres-rates-rate.png" , width = 1366 , height = 768 , units = "px")
ggplot(genres_rates, aes(x= reorder(genre , rate ), y=rate , fill = genre)) + 
  geom_bar(stat = "identity") +
  labs(x = "Genre" , y= "Rate"  )+
  ggtitle("Comparing with the rate of each genre accorfing to the rate itself")+
  theme_ft_rc() +
  theme(axis.text.x = element_text(angle = 90) )
dev.off()
#![genres-rates-rate](https://github.com/Dodger23/The-movie-project/blob/develop/images/genres-rates-rate.png)

# plot genre and rate 
png(filename ="images/genres-rates-views.png" , width = 1366 , height = 768 , units = "px")
ggplot(genres_rates, aes(x= reorder(genre , views ), y=rate , fill = genre)) + 
  geom_bar(stat = "identity") +
  labs(x = "Genre" , y= "Rate"  )+
  ggtitle("Comparing with the rate of each genre accorfing to the views")+
  theme_ft_rc() +
  theme(axis.text.x = element_text(angle = 90) )
dev.off()
#![genres-rates-views](https://github.com/Dodger23/The-movie-project/blob/develop/images/genres-rates-views.png)


genres_views = as.data.frame(genres_views %>% group_by(genre) %>% summarise_all(sum) ) 

# plot genre and views 
png(filename = "images/genres-views.png" , width = 1366 , height = 768 , units = "px")
ggplot(genres_views, aes(x= reorder(genre , views ), y=views , fill = genre)) + 
  geom_bar(stat = "identity") +
  labs(x = "Genre" , y= "Views"  )+
  ggtitle("Comparing with the views of each genre")+
  theme_ft_rc() +
  theme(axis.text.x = element_text(angle = 90) )
dev.off()
#![genres-rates-views](https://github.com/Dodger23/The-movie-project/blob/develop/images/genres-views.png)

if(FALSE)
{
  

ids = vector()
for(i in 1:nrow(movies2))
{
  sp = strsplit(movies2[i , "genres"] , "\\|")[[1]]
  if("Drama" %in% sp)
  ids = c(ids , movies2[i , "movieId"])
}
drama = ratings[, names(ratings) %in% c("movieId" , "rating")]
drama = as.data.frame(drama %>% filter(movieId %in% ids ) 
                      %>% group_by(movieId)
                      %>% summarise_all(mean)
                        )

drama = drama[order( - drama$rating) , ]
drama = drama[drama$rating == 5, ]

temp = data.frame(movie = movies2[movies2$movieId %in% drama$movieId , "title"] )
temp$freq = round(runif(nrow(temp) , 1 , 20))


png(filename = "images/movies-wordcloud.png" , width = 1366 , height = 768 , units = "px")
wordcloud2( temp )
dev.off()



drama$movieId = as.character(drama$movieId)
filt = c("In Netflix queue" , "free to download" )
tags = tags %>% filter(movieId %in% drama$movieId & ! tag %in% filt )
tags = tags[order(tags$timestamp) , ]
tags = tags[1 :nrow(tags), "tag"]
temp = data.frame(word = tags)
head(temp)
temp$freq = round(runif(nrow(temp) , 1 ,10))

png(filename = "images/tagswordcloud.png" , width = 1366 , height = 768 , units = "px")
wordcloud2( temp )
dev.off()


}



