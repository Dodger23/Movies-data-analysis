library(reshape2)
library(ggplot2)
library(hrbrthemes)
library(stringr)

#movies = read.csv("Data/movies.csv")
#ratings = read.csv("Data/ratings.csv")
#tags = read.csv("Data/tags.csv")


#cleaning ratings data 
ratings$timestamp = as.POSIXct(ratings$timestamp , origin = '1970-1-1' , tz = "UTC") 


#cleaning tags data 
tags$timestamp = as.POSIXct(tags$timestamp , origin = '1970-1-1' , tz = "UTC") 


# Cleaning movies data

### Could be vectorized ------------
movies$title = as.character(movies$title)
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
vec = vec[genres!="(no genres listed)"]

genres = data.frame(genre = vec , rate = rep(0,length(vec))  , views = rep(0,length(vec)) )














