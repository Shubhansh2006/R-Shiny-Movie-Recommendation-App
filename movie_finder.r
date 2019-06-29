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



