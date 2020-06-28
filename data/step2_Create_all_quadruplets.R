rm(list=ls())
library(data.table)
setwd("C:/AE_paper/data")
load("step2_Create_all_quadruplets_input.RData")
#load("step1_Create_decoy_target_output.RData")
#chosen decoy pairs and all movies info
decoy_ratings[,no_1 := as.numeric(unlist(strsplit(as.character(ids), split = " "))[1]), 1:nrow(decoy_ratings)]
decoy_ratings[,no_2 := as.numeric(unlist(strsplit(as.character(ids), split = " "))[2]), 1:nrow(decoy_ratings)]
decoy_pairs <- decoy_ratings[mean>=4.5,c("no_1", "no_2", "mean") ]

#create order of the proximity of match
decoy_pairs[, prox := 1:.N]

setnames(decoy_pairs, c("no_1", "no_2"), c("Movie_A_no", "Movie_B_no"))
decoy_pairs <- merge(decoy_pairs, movie_data, by.x= "Movie_A_no", by.y = "movie.no",all.x = TRUE)
setnames(decoy_pairs, c("Movie.id", "Title"), c("Movie_A_id", "Movie_A_title"))
decoy_pairs <- merge(decoy_pairs, movie_data, by.x= "Movie_B_no", by.y = "movie.no",all.x = TRUE)
setnames(decoy_pairs, c("Movie.id", "Title"), c("Movie_B_id", "Movie_B_title"))

own.cat.matrix<- matrix(0,nrow= 400, ncol = 400)

own.cat.matrix <- matrix(mapply(function (x, y)
{sum(unique(genre_info[x,c(unlist(Categories), unlist(genres))]) %in% unique(genre_info[y,c(unlist(Categories), unlist(genres))]))},
rep(1:400, each = 400), rep(1:400, 400)), byrow = TRUE, ncol = 400,
dimnames = list(genre_info[,Title],genre_info[,Title]))
diag(own.cat.matrix) <- NA

decoy_pairs <- merge(decoy_pairs, data.table("no_movie_A" = 1:400, "Movie_A_title" = rownames(own.cat.matrix)), by ="Movie_A_title", all.x = TRUE)
decoy_pairs <- merge(decoy_pairs, data.table("no_movie_B" = 1:400, "Movie_B_title" = rownames(own.cat.matrix)), by ="Movie_B_title", all.x = TRUE)


competitor_pairs_matrix <- matrix(1000,nrow= nrow(decoy_pairs), ncol = nrow(decoy_pairs))
competitor_pairs_matrix <- matrix(mapply(function(x,y) {sum(own.cat.matrix[rep(unlist(decoy_pairs[x,c("no_movie_A","no_movie_B")]),2), rep(unlist(decoy_pairs[y,c("no_movie_A","no_movie_B")]),2)])},
                                         rep(1:nrow(decoy_pairs), each = nrow(decoy_pairs)), rep(1:nrow(decoy_pairs), nrow(decoy_pairs))),byrow = TRUE, ncol = nrow(decoy_pairs))
rownames(competitor_pairs_matrix) <- decoy_pairs[,paste(Movie_B_title, Movie_A_title, sep = ", "), by = 1:nrow(decoy_pairs)]$V1
colnames(competitor_pairs_matrix) <- decoy_pairs[,paste(Movie_B_title, Movie_A_title, sep = ", "), by = 1:nrow(decoy_pairs)]$V1



all_quadruplets <- data.table()

for (i in 1:nrow(decoy_pairs)) {
  all_quadruplets <- rbind(all_quadruplets, cbind(data.table("Movie_1A_title" = decoy_pairs[i,Movie_A_title], "Movie_2A_title" = decoy_pairs[i,Movie_B_title], "Movie_1A_id" = decoy_pairs[i,Movie_A_id], "Movie_2A_id" = decoy_pairs[i,Movie_B_id]),
                                                  decoy_pairs[competitor_pairs_matrix[i,]  == 0,c("Movie_A_title", "Movie_B_title", "Movie_A_id", "Movie_B_id")]))
}
setnames(all_quadruplets, c("Movie_A_title", "Movie_B_title", "Movie_A_id", "Movie_B_id"),
         c("Movie_1B_title", "Movie_2B_title", "Movie_1B_id", "Movie_2B_id"))

#add movie.no info 
all_quadruplets <- merge(all_quadruplets, movie_data[, c("Movie.id", "movie.no")], by.x = "Movie_1A_id", by.y = "Movie.id", all.x = TRUE)
setnames(all_quadruplets, "movie.no", "Movie_1A_no")
all_quadruplets <- merge(all_quadruplets, movie_data[, c("Movie.id", "movie.no")], by.x = "Movie_2A_id", by.y = "Movie.id", all.x = TRUE)
setnames(all_quadruplets, "movie.no", "Movie_2A_no")
all_quadruplets <- merge(all_quadruplets, movie_data[, c("Movie.id", "movie.no")], by.x = "Movie_1B_id", by.y = "Movie.id", all.x = TRUE)
setnames(all_quadruplets, "movie.no", "Movie_1B_no")
all_quadruplets <- merge(all_quadruplets, movie_data[, c("Movie.id", "movie.no")], by.x = "Movie_2B_id", by.y = "Movie.id", all.x = TRUE)
setnames(all_quadruplets, "movie.no", "Movie_2B_no")
all_quadruplets[, A_id := paste(sort(c(Movie_1A_no, Movie_2A_no)), collapse = " "), by = 1:nrow(all_quadruplets)]
all_quadruplets[, B_id := paste(sort(c(Movie_1B_no, Movie_2B_no)), collapse = " "), by = 1:nrow(all_quadruplets)]
decoy_pairs[, pair_id := paste(sort(c(Movie_B_no, Movie_A_no)), collapse = " "), by = 1:nrow(decoy_pairs)]

rm(list=ls()[! ls() %in% c("all_quadruplets", "decoy_pairs")])
save.image("step2_Create_all_quadruplets_output.RData")