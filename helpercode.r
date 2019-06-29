library(proxy)
library(recommenderlab)
library(reshape2)
movies <- read.csv("movies.csv", header = TRUE, stringsAsFactors=FALSE)
ratings <- read.csv("ratings.csv", header = TRUE)
movies2 <- movies[-which((movies$movieId %in% ratings$movieId) == FALSE),]
search<-read.csv('search.csv',stringsAsFactors = F)
movie_year<-search[,c('movieId','year')]
movies2<-merge(movies2,movie_year,by='movieId')
ratings<-merge(ratings,movie_year,by='movieId')


movie_recommendation <- function(input,input2,input3,year1,year2,genreFilter) 
{
  #year1=2010
  #year2=2015
  #genreFilter=c('Film.Noir')
  #input = "Gladiator (2000)"
  #input2 = "Aeon Flux (2005)"
  #nput3 = "Alexander (2004)"

  # Error proofing for no Genres selected
  if(is.null(genreFilter)){
    genreFilter<-c("Action", "Adventure", "Animation", "Children", 
                   "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                   "Film.Noir", "Horror", "Musical", "Mystery","Romance",
                   "Sci.Fi", "Thriller", "War", "Western")
  }
  #Year filter
  ratings<-ratings[ratings$year>=year1 & ratings$year<=year2 | ratings$movieId %in%  movies2[movies2$title %in% c(input,input2,input3),'movieId'] ,]
  movies2<-movies2[movies2$year>=year1 & movies2$year<=year2 | movies2$movieId %in%  movies2[movies2$title %in% c(input,input2,input3),'movieId'] ,]
  # Genre Filter
  
  #Error proofing
  if(length(genreFilter)>1)
  {ratings<-ratings[ ratings$movieId %in% search[rowSums(search[,genreFilter])>0,'movieId'] | ratings$movieId %in%  movies2[movies2$title %in% c(input,input2,input3),'movieId'],]
  movies2<-movies2[ movies2$movieId %in% search[rowSums(search[,genreFilter])>0,'movieId'] | movies2$movieId %in%  movies2[movies2$title %in% c(input,input2,input3),'movieId'],] 
  }else{
    ratings<-ratings[ ratings$movieId %in% search[search[,genreFilter]>0,'movieId'] | ratings$movieId %in%  movies2[movies2$title %in% c(input,input2,input3),'movieId'],]
    movies2<-movies2[ movies2$movieId %in% search[search[,genreFilter]>0,'movieId'] | movies2$movieId %in%  movies2[movies2$title %in% c(input,input2,input3),'movieId'],] 
    
  }
  # error Proofing ( if selection goes out of bounds )
  if(nrow(movies2)< 4){
      no_result[1,1] <- "Sorry, there is not enough information in our database on the movies you've selected. Try to select different movies you like."
      colnames(no_result) <- "No results"
      return(no_result) 
    
  }
  row_num <- which(movies2[,'title'] == input)
  row_num2 <- which(movies2[,'title'] == input2)
  row_num3 <- which(movies2[,'title'] == input3)
  userSelect <- matrix(NA,nrow(movies2))
  userSelect[row_num] <- 5 
  userSelect[row_num2] <- 5 
  userSelect[row_num3] <- 5
  userSelect <- t(userSelect)
  

  # Subsetting movie list to the years specified by the user
  

  ratingmat <- dcast(ratings, userId~movieId, value.var = "rating", na.rm=FALSE)
  ratingmat <- ratingmat[,-1]
  colnames(userSelect) <- colnames(ratingmat)
  ratingmat2 <- rbind(userSelect,ratingmat)
  ratingmat2 <- as.matrix(ratingmat2)
  
  #Convert rating matrix into a sparse matrix
  ratingmat2 <- as(ratingmat2, "realRatingMatrix")
  ratingmat2 <- normalize(ratingmat2)
  #Create Recommender Model. "UBCF" stands for user-based collaborative filtering
  suppressWarnings(
  recommender_model <- Recommender(ratingmat2, method = "UBCF",param=list(method="Cosine",nn=30)))
  suppressWarnings(
  recom <- predict(recommender_model, ratingmat2[1], n=10))
  recom_list <- as(recom, "list")
  no_result <- data.frame(matrix(NA,1))
  recom_result <- data.frame(matrix(NA,10))
  if (as.character(recom_list[1])=='character(0)'){
    no_result[1,1] <- "Sorry, there is not enough information in our database on the movies you've selected. Try to select different movies you like."
    colnames(no_result) <- "No results"
    return(no_result) 
  } else {
    for (i in 1:length(recom_list[[1]])){
      recom_result[i,1] <- as.character(subset(movies, 
                                               movies$movieId == as.integer(recom_list[[1]][i]))$title)
    }
    colnames(recom_result) <- "Recommended Movies"
    return(recom_result)
  }
}


# Testing Movie Reccom

year1=2001
year2=2015
genreFilter=c('Film.Noir')
input = "Gladiator (2000)"
input2 = "Aeon Flux (2005)"
input3 = "Alexander (2004)"
movie_recommendation(input = input,input2 = input2,input3 = input3,year1 = year1,year2 = year2,genreFilter = genreFilter)
