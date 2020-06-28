rm(list=ls())
library(data.table)
setwd("C:/AE_paper/data")
load("step3_Create_bespoke_quadruplets_input.RData")

ratings_data[, movie.id := book.id]
ratings_data[, movie.no := book.no]
ratings_data <- data.table(data.table::dcast(ratings_data, worker.id+movie.id +movie.no~ trial.type, value.var = c("response", "rt")))

ratings_data <- merge(ratings_data, movie_data[,c("Movie.id", "Title")], by.x = "movie.id",
                      by.y = "Movie.id", all.x = TRUE)

generate.choices <- function(workerid) {
  
  results <- all_quadruplets
  ratings <- ratings_data[worker.id == workerid,c("Title", "response_rating", "response_seenbefore")]
  
  results <- merge(results, ratings, by.x = "Movie_1A_title", by.y = "Title", all.x = TRUE)
  setnames(results, c("response_seenbefore", "response_rating"), c("Seen_1A", "Rating_1A"))
  results <- merge(results, ratings, by.x = "Movie_2A_title", by.y = "Title", all.x = TRUE)
  setnames(results, c("response_seenbefore", "response_rating"), c("Seen_2A", "Rating_2A"))
  results <- merge(results, ratings, by.x = "Movie_1B_title", by.y = "Title", all.x = TRUE)
  setnames(results, c("response_seenbefore", "response_rating"), c("Seen_1B", "Rating_1B"))
  results <- merge(results, ratings, by.x = "Movie_2B_title", by.y = "Title", all.x = TRUE)
  setnames(results, c("response_seenbefore", "response_rating"), c("Seen_2B", "Rating_2B"))
  choices <- data.table()
  
  results[, c("Rating_1A","Rating_2A","Rating_1B","Rating_2B") := lapply(.SD, as.numeric),
          .SDcols=c("Rating_1A","Rating_2A","Rating_1B","Rating_2B")]
  results[, c("Seen_1A","Seen_2A","Seen_1B","Seen_2B") := lapply(.SD, as.character),
          .SDcols=c("Seen_1A","Seen_2A","Seen_1B","Seen_2B")]
  
  
  #1) there's an at least three units difference between the similar movies
  results <- results[(abs(Rating_1A - Rating_2A) >= 3) & (abs(Rating_1B - Rating_2B) >= 3), ]
  #2) the higher ones are the same
  results <- results[pmax(Rating_1A,Rating_2A) == pmax(Rating_1B, Rating_2B), ]
  #if (nrow(results) > 0) {
  results[,Seen_all := Seen_1A == "seen" & Seen_2A == "seen" & Seen_1B == "seen" & Seen_2B == "seen"]
  results[,Notseen_all := Seen_1A == "notseen" & Seen_2A == "notseen" & Seen_1B == "notseen"& Seen_2B == "notseen"]
  
  
  results <- results[Seen_all == TRUE | Notseen_all == TRUE,]
  
  results[, Diff_A := abs(Rating_1A-Rating_2A)]
  results[, Diff_B := abs(Rating_1B-Rating_2B)]
  
  if (nrow(results) > 0) {
    
    results[, c("Target_A_Title", "Decoy_A_Title", "Target_B_Title", "Decoy_B_Title") := list(c(as.character(Movie_1A_title), as.character(Movie_2A_title))[which.max(c(Rating_1A, Rating_2A))],
                                                                                              c(as.character(Movie_1A_title), as.character(Movie_2A_title))[which.min(c(Rating_1A, Rating_2A))],
                                                                                              c(as.character(Movie_1B_title), as.character(Movie_2B_title))[which.max(c(Rating_1B, Rating_2B))],
                                                                                              c(as.character(Movie_1B_title), as.character(Movie_2B_title))[which.min(c(Rating_1B, Rating_2B))]), by = 1:nrow(results)]
    
    results[, c("Target_A_id", "Decoy_A_id", "Target_B_id", "Decoy_B_id") := list(c(as.character(Movie_1A_id), as.character(Movie_2A_id))[which.max(c(Rating_1A, Rating_2A))],
                                                                                  c(as.character(Movie_1A_id), as.character(Movie_2A_id))[which.min(c(Rating_1A, Rating_2A))],
                                                                                  c(as.character(Movie_1B_id), as.character(Movie_2B_id))[which.max(c(Rating_1B, Rating_2B))],
                                                                                  c(as.character(Movie_1B_id), as.character(Movie_2B_id))[which.min(c(Rating_1B, Rating_2B))]), by = 1:nrow(results)]
    
    results[, c("Target_A_no", "Decoy_A_no", "Target_B_no", "Decoy_B_no") := list(c(Movie_1A_no, Movie_2A_no)[which.max(c(Rating_1A, Rating_2A))],
                                                                                  c(Movie_1A_no, Movie_2A_no)[which.min(c(Rating_1A, Rating_2A))],
                                                                                  c(Movie_1B_no, Movie_2B_no)[which.max(c(Rating_1B, Rating_2B))],
                                                                                  c(Movie_1B_no, Movie_2B_no)[which.min(c(Rating_1B, Rating_2B))]), by = 1:nrow(results)]
    
    results[, c("Target_A_rating", "Decoy_A_rating", "Target_B_rating", "Decoy_B_rating") := list(c(Rating_1A, Rating_2A)[which.max(c(Rating_1A, Rating_2A))],
                                                                                                  c(Rating_1A, Rating_2A)[which.min(c(Rating_1A, Rating_2A))],
                                                                                                  c(Rating_1B, Rating_2B)[which.max(c(Rating_1B, Rating_2B))],
                                                                                                  c(Rating_1B, Rating_2B)[which.min(c(Rating_1B, Rating_2B))]), by = 1:nrow(results)]
    
    
    results <- results[, seq(23,ncol(results)), with = FALSE]
    
    results <- results[order(-Diff_A, -Diff_B)]
    
    #now get info about prox
    results[, pair_A_id := paste(sort(c(Target_A_no,Decoy_A_no)), collapse = " "), by = 1:nrow(results)]
    results[, pair_B_id := paste(sort(c(Target_B_no,Decoy_B_no)), collapse = " "), by = 1:nrow(results)]
    
    results <- merge(results, decoy_pairs[, c("mean", "pair_id")], by.x = "pair_A_id", by.y = "pair_id", all.x = TRUE)
    setnames(results, "mean", "pair_A_mean")
    results <- merge(results, decoy_pairs[, c("mean", "pair_id")], by.x = "pair_B_id", by.y = "pair_id", all.x = TRUE)
    setnames(results, "mean", "pair_B_mean")
    
    results[, overall_sim := pair_A_mean + pair_B_mean]
    
    results[, duplicate_id := paste0(sort(c(Target_A_no, Target_B_no, Decoy_B_no, Decoy_A_no)), collapse = " "), by = 1:nrow(results)]
    results[, how.many := 1:.N ,by = duplicate_id]
    results <- results[how.many ==1,]
    
    results[, movie_no_list := list(list(c(Target_A_no, Target_B_no))), by = 1:nrow(results)]
    while (nrow(results) > 0) {
      #pick the lowest decoy distance
      choices <- rbind(choices, results[which.max(overall_sim),][1,])
      toexclude <- unique(unlist(choices[,movie_no_list]))
      results <- results[!(Target_A_no %in% toexclude | Target_B_no %in% toexclude),]
      #delete the ones that are already in the chosen targets
      #results <- results[!(Target_A_Title %in% toexclude | Target_B_Title %in% toexclude),]
    }
    return(choices)
  }
}


datalist = list()

sapply(1:length(unique(ratings_data$worker.id)), function (i) {
  # ... make some data
  dat <- generate.choices(unique(ratings_data$worker.id)[i])
  if (!is.null(dat)) {
    dat[, worker.id := unique(ratings_data$worker.id)[i]]
  }
  datalist[[i]] <<- dat # add it to your list
  print(i)
})

choice_data <- rbindlist(datalist)
choice_data[, how.many := .N, by = worker.id]
choice_data <- choice_data[how.many >=3, ]

rm(list=ls()[! ls() %in% c("all_quadruplets", "choice_data")])
save.image("step3_Create_bespoke_quadruplets_output.RData")
