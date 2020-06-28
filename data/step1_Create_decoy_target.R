rm(list=ls())
library(data.table)
setwd("C:/AE_paper/data")
load("step1_Create_decoy_target_input.RData")

own.cat.matrix <- matrix(0,nrow= 400, ncol = 400)

own.cat.matrix <- matrix(mapply(function (x, y) 
{sum(genre_info[x,unlist(genres)] %in% genre_info[y,unlist(genres)])},
rep(1:400, each = 400), rep(1:400, 400)), byrow = TRUE, ncol = 400,
dimnames = list(genre_info[,Title],genre_info[,Title]))
diag(own.cat.matrix) <- NA

#now I need a dataset with pairs of movies that have the highest no of common ratings
genre_info[, maxno := apply(own.cat.matrix, 1, function(x) max(x,na.rm = TRUE))]
decoy.pair.genre <- data.table("Movie_A_title" = character(), "Movie_B_title" = character(), 
                               "Movie_A_id" = character(), "Movie_B_id" = character(), "Strength_of_assoc" = numeric())

for (i in 1:nrow(genre_info)) {
  if (genre_info[i,maxno] != 0) {
    which(own.cat.matrix[i,] == genre_info[i,maxno])
    
    sample.dat <- data.table("Movie_B_title" = names(which(own.cat.matrix[i,] == genre_info[i,maxno])),
                             "Movie_B_id" = genre_info[which(own.cat.matrix[i,] == genre_info[i,maxno]),Movie.id],
                             "Movie_A_title" = genre_info[i,Title], "Movie_A_id" = genre_info[i,Movie.id],
                             "Strength_of_assoc" = genre_info[i,maxno])
    decoy.pair.genre <- rbind(decoy.pair.genre,sample.dat)
  }
  print(i)
}


decoy.pair.genre[, id := paste(sort(c(Movie_A_id, Movie_B_id)), collapse = " "), by = 1:nrow(decoy.pair.genre)]
decoy.pair.genre[, Mutual := .N, by = id]

decoy.pair.genre <- decoy.pair.genre[,list(Movie_A_title = Movie_A_title[1], Movie_B_title = Movie_B_title[1], 
                                           Movie_A_id = Movie_A_id[1], Movie_B_id = Movie_B_id[1],
                                           Strength_of_assoc = Strength_of_assoc[1], Mutual = Mutual[1]), by = id]
decoy.pairs <- rbind(decoy.pair.genre, decoy_pairs_semantic[!(id %in% decoy.pair.genre[,id]),], fill = TRUE)

# 
rm(list=ls()[! ls() %in% c("decoy.pairs", "own.cat.matrix", "genre_info")])
save.image("step1_Create_decoy_target_output.RData")
