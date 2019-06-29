setwd('/Users/apple/Documents/PURDUE/Study Material/R for Analytics/gitFolder/Data/ml-latest-small')
links<-read.csv('links.csv',stringsAsFactors = F)
movies<-read.csv('movies.csv',stringsAsFactors = F)
ratings<-read.csv('ratings.csv',stringsAsFactors = F)
meta<-read.csv('movies_metadata.csv',stringsAsFactors = F)
ratings <- read.csv("ratings.csv", header = TRUE)
movies2 <- movies[-which((movies$movieId %in% ratings$movieId) == FALSE),]
movies2<-merge(x=movies2,y = links,by.x = 'movieId',by.y ='movieId',all.x=T)
movies2$tmdbId<-NULL
# getting proper IMDB id
cleanImdb<-function(imdb_Id){
  if(nchar(imdb_Id)==3){return(paste0('tt0000',imdb_Id))}
  else if (nchar(imdb_Id)==4){return(paste0('tt000',imdb_Id))}
  else if (nchar(imdb_Id)==5){return(paste0('tt00',imdb_Id))}
  else if (nchar(imdb_Id)==6){return(paste0('tt0',imdb_Id))}
  else{return(paste0('tt0',imdb_Id))}
}

movies2$imdbId<-unlist(sapply(movies2$imdbId,cleanImdb))
metaNeeded<-meta[,c('imdb_id','adult','overview','popularity','poster_path')]
movies2<-merge(x=movies2, y = metaNeeded, by.x = 'imdbId', by.y ='imdb_id', all.x=T)
movies2$poster_path<-paste0('http://image.tmdb.org/t/p/w185/',movies2$poster_path)

www.themoviedb.org/person/

str(ratings)
str(links)
str(movies)
library(recommenderlab)
library(ggplot2)
# Movie Genre List
genre_list <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci-Fi", "Thriller", "War", "Western")
library(data.table)

moviematrix<-data.frame(matrix(nrow = 9125,ncol = 19))
names(moviematrix)<-c('title',genre_list)
moviematrix$title<-movies$title

# Create a funciton to create a movie matrix using regEx
createMovieMatrix<-function(rowList,genre_list){
  final_list<-c(rep(0,18))
  for (i in 1:18){
    if(grepl(genre_list[i],rowList,ignore.case = T)==T){
      final_list[i]=1
    }
  }
  return(final_list)
}
# A function to extract the year from the title
getYear<-function(title){
  year<-substr(title,start =nchar(title)-4  ,stop =nchar(title)-1)
  return (year)
}

# Create the actual movie matrix
for (i in 1:nrow(moviematrix)){
  moviematrix[i,c(2:19)]<-createMovieMatrix(movies$genres[i],genre_list)
}

# Create a new year column
moviematrix$Year<-sapply(X=moviematrix$title,FUN = getYear)
moviematrix$movieId<-movies$movieId
library(sqldf)
# Subsetting movies which have been given a rating
moviematrix<-sqldf('select * from moviematrix where movieId in (select distinct movieId from ratings)')


# Cleaning data
moviematrix$Year[moviematrix$title=='Hyena Road']<-'2015'
moviematrix$Year[moviematrix$title=='Big Bang Theory, The (2007-)']<-'2007'
moviematrix$Year[moviematrix$title=='Wake Wood (2010) ']<-'2010'
moviematrix$Year[moviematrix$title=='Hostel: Part III (2011) ']<-'2011'
moviematrix$Year[moviematrix$title=='96 Minutes (2011) ']<-'2011'
moviematrix$Year[moviematrix$title=='Family Band: The Cowsills Story (2011) ']<-'2011'
moviematrix$Year[moviematrix$title=='Stranger Things']<-'2016'
moviematrix$Year[moviematrix$title=='The Lovers and the Despot']<-'2016'
moviematrix$Year[moviematrix$title=='Rocky VI (1986)']<-'1986'

# Create a user- rating matrix. This is a high dimentional matrix, so do not try to view it from the environment.
userRatingMatrix<-as.data.frame(matrix(nrow = nrow(moviematrix), ncol =length(unique(ratings$userId))))
userRatingMatrix$title<-moviematrix$title
userRatingMatrix$movieId<-moviematrix$movieId
names(userRatingMatrix)<-c(as.character(1:671),'title','movieId')
movieidlist<-data.frame(userRatingMatrix[,'movieId'])
names(movieidlist)<-'movieId'
for (i in 1:671){
  p<-ratings[ratings$userId==i,]
  rowValues<-merge(x=p,movieidlist,by='movieId',all=T)[,3]
  userRatingMatrix[,i]<-rowValues

}

g<-userRatingMatrix
rownames(g)<-g$movieId
g$movieId<-NULL
g$title<-NULL
ratingMatrix<-t(as.matrix(g))
dim(ratingMatrix)
ratingMatrix <- as(ratingMatrix, "realRatingMatrix")
# only keeping those movies rated by atleast 30 users and only those users who have rated more than 30 movies
ratingMatrix <- ratingMatrix[rowCounts(ratingMatrix) > 30,
                            colCounts(ratingMatrix) > 30]
ratingMatrix

#Normalizing ratings
ratingNormalized <- normalize(ratingMatrix)


#Setting Seed
set.seed(1000)
which_train <- sample(x = c(TRUE, FALSE), 
                      size = nrow(ratingMatrix),
                      replace = TRUE, 
                      prob = c(0.8, 0.2))
#head(which_train)

train_rows <- ratingMatrix[which_train, ]
test_rows <- ratingMatrix[!which_train, ]

# Getting the recommender system algorithms available in the recommenderlab algorithm
recommenderRegistry$get_entries(dataType = "realRatingMatrix")

recommender_models <- recommenderRegistry$get_entries(dataType ="realRatingMatrix")
recommender_models$UBCF_realRatingMatrix$parameters

 userBasedModel<- Recommender(data = train_rows, 
                          method = "UBCF",
                          parameter = list(nn = 30))



model_details <- getModel(userBasedModel)
model_details$description
model_details$nn

N_recommendations <- 10
recc_predicted <- predict(object = userBasedModel,
                          newdata = test_rows, 
                          n = N_recommendations) 
recc_predicted
recc_matrix <- sapply(recc_predicted@items, 
                      function(x){ as.integer(colnames(ratingMatrix)[x]) })
#dim(recc_matrix)
recc_matrix[, 1:4]

number_of_items <- factor(table(recc_matrix))

chart_title <- "Distribution of the number of items for UBCF"
qplot(number_of_items) + ggtitle(chart_title)




#Getting movie poster info
df<-as.data.frame(matrix(data = NA,nrow = 0,ncol = 4))
posterinfo<-as.data.frame(meta$belongs_to_collection)
for( i in 1:nrow(posterinfo)){
  p<-posterinfo[i,]
  p<-gsub("'",'"',p)
  row<-fromJSON(paste0('[ ',p,' ]'))
  df<-rbind(row)
  
}



