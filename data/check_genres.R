rm(list=ls())
library(simpleboot)
library(data.table)
library(ggplot2)
library(ggthemes)
library(lme4)
library(stargazer)
setwd("C:/AE_paper/data")
load("step4_Prepare_choice_data_input.RData")
load("C:/AE_paper/data/step1_Create_decoy_target_output.RData")

#read in ratings data 
ratings <- trials[trial.type == "rating",]
ratings <- merge(ratings[trial.type == "rating",], genre_info, by.x = "book.id",
                      by.y = "Movie.id", all.x = T)
ratings[, response := as.numeric(factor(response, levels = as.character(seq(1,7))))]
genre_mean <- ratings[, list(mean_rating = mean(response)), .(worker.id, Category)]

load("step5_AE_results_input.RData")
#load("C:/AE_paper/data/step3_Create_bespoke_quadruplets_input.RData")

#calculate average rating for genre


#theres one NA, get rid of it
triplets <- triplets[!is.na(Which.chosen), ]
#add genre info

triplets <- merge(triplets, genre_info[, c("Movie.id", "Category")], 
                  by.x = "Target_id", by.y = "Movie.id", all.x = T)
setnames(triplets, c("Category"), c("Target_Category"))

triplets <- merge(triplets, genre_info[, c("Movie.id", "Category")], 
                  by.x = "Competitor_id", by.y = "Movie.id", all.x = T)
setnames(triplets, c("Category"), c("Competitor_Category"))


#get rid of trails where the deocy was chosen
triplets <- triplets[Which.chosen != "Decoy", ]


genre_check <- data.table(expand.grid("worker.id"  = unique(triplets$worker.id),
           "genre" =  unique(triplets$Competitor_Category)))



triplets[, chosen_cat := ifelse(Which.chosen == "Target", Target_Category, Competitor_Category)]

for (i in 1:nrow(genre_check)) {
  
genre_check[i, no_trials := nrow(triplets[worker.id == genre_check[i,worker.id] & (Target_Category == genre_check[i, genre] | Competitor_Category == genre_check[i,genre])])]
genre_check[i, no_chosen := nrow(triplets[worker.id == genre_check[i,worker.id] & (Target_Category == genre_check[i, genre] | Competitor_Category == genre_check[i,genre]) & chosen_cat == genre_check[i, genre]])]
  print(i)
}



genre_check[, prop := no_chosen/no_trials]
genre_check[no_trials == 0, prop := ]











triplets <- merge(triplets, genre_mean, by.x = c("worker.id", "Target_Category"), by.y = c("worker.id", "Category"),
                  all.x = T)

setnames(triplets, c("mean_rating"), c("Target_cat_mean"))

triplets <- merge(triplets, genre_mean, by.x = c("worker.id", "Competitor_Category"), by.y = c("worker.id", "Category"),
                  all.x = T)

setnames(triplets, c("mean_rating"), c("Comp_cat_mean"))


triplets[, chosen_cat_mean :=ifelse(Which.chosen == "Target", Target_cat_mean, Comp_cat_mean)]

