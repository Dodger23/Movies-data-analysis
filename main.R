library(reshape2)
library(ggplot2)
#library(hrbrthemes)
library(stringr)

movies = read.csv("Data/movies.csv")
ratings = read.csv("Data/ratings.csv")
tags = read.csv("Data/tags.csv")


#cleaning ratings data 
ratings$timestamp = as.POSIXct(ratings$timestamp , origin = '1970-1-1' , tz = "UTC") 


#cleaning tags data 
tags$timestamp = as.POSIXct(tags$timestamp , origin = '1970-1-1' , tz = "UTC") 


# Cleaning movies data

### Could be vectorized ------------
movies$title = as.character(movies$title)
movies$genres = as.character(movies$genres)
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


# Getting unique genres 
sp = strsplit(as.character(movies$genres) ,  "\\|" )
vec  = vector()
for(i in sp)
{
  vec = unique(c(vec , i ))
}
vec = vec[vec!="(no genres listed)"]

genres = data.frame(genre = vec , rate = rep(0,length(vec))  , views = rep(0,length(vec)) )

for(i in 1:nrow(ratings))
{
  id = ratings[i , "movieId"] 
  rate = ratings[i, "rating"]
  g = strsplit(  movies[movies$movieId==id , "genres"]  , "\\|")
  for(j in g[[1]] )
  {
    genres[genres$genre == j , "views"] = genres[genres$genre == j , "views"] + 1
    genres[genres$genre == j  , "rate"] = genres[genres$genre == j , "rate"] + rate
  }
}

genres$rate = genres$rate / genres$views

# plot genre and rate 
ggplot(genres, aes(x= reorder(genre , rate ), y=rate , fill = genre)) + 
  geom_bar(stat = "identity") +
  labs(x = "Genre" , y= "Rate"  )+
  ggtitle("Comparing with the rate of each genre")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90) )


# plot genre and views 
ggplot(genres, aes(x= reorder(genre , views ), y=views , fill = genre)) + 
  geom_bar(stat = "identity") +
  labs(x = "Genre" , y= "Views"  )+
  ggtitle("Comparing with the views of each genre")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90) )














