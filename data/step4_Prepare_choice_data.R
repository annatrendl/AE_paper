rm(list=ls())
library(data.table)
library(entropy)
setwd("C:/AE_paper/data")
load("step4_Prepare_choice_data_input.RData")

####################### FIRST FILTER THE DATA: DROP PARTICIPANTS WHO DID NOT DO THE RATING PROPERLY ############
ratings <- trials[trial.type == "rating",]
ratings[, response := factor(response, levels = as.character(seq(1,7)))]
ratings[,freq:=.N,by=list(sub.id, response)]

check.outliers <- ratings[,list(expt.duration = sum(rt),
                                autocorr = acf(response, plot=FALSE)$acf[2],
                                H=entropy(table(response)), workerid = worker.id[1]), by = sub.id]

check.outliers[,too.fast := expt.duration < quantile(expt.duration,.05)] # Fastest 5%
check.outliers[,low.H := H < quantile(H,.05)] # Lowest entropy responses
check.outliers[,extreme.r1 := autocorr < quantile(autocorr,.05) | autocorr > quantile(autocorr,.95)] # Most and least autocorrelated 5%s
check.outliers[,outlier:= too.fast | low.H | extreme.r1] # Any of the above
check.outliers[outlier == TRUE, workerid]

#keep the ones that are not outliers
trials <- trials[!( worker.id %in% check.outliers[outlier == TRUE, workerid]),]
choice_results <- choice_results[!( worker.id %in% check.outliers[outlier == TRUE, workerid]),]

similarity_results <- similarity_results[worker.id %in% choice_results$worker.id,]


trials <- trials[worker.id %in% unique(choice_results$worker.id),]


setnames(trials, c("book.id", "book.no"), c("movie.id","movie.no"))
trials <- data.table(dcast(trials, sub.id + movie.id + worker.id ~ trial.type, value.var = "response"))
trials[, rating := as.numeric(rating)]

trials <- merge(trials, alldata[,c("Movie.id", "Title")], by.x = "movie.id", by.y = "Movie.id", all.x = TRUE)

#get prepared choice sets and similarity ratings for each subject
#keep only the ones we have choice data for
quadruplets_for_each <- quadruplets_for_each[worker.id %in% unique(choice_results$worker.id),]
#exactly twice as many as the triplets

#all I need to know is which one was chosen

triplets_A <- quadruplets_for_each[, c("worker.id" ,"Target_A_Title", "Target_B_Title", "Decoy_A_Title",
                                       "Target_A_no","Target_B_no","Decoy_A_no",
                                       "Target_A_id", "Target_B_id", "Decoy_A_id", "Target_A_rating",
                                       "Target_B_rating","Decoy_A_rating", "Seen_all","Notseen_all",
                                       "pair_A_mean", "pair_B_mean", "overall_sim")]

setnames(triplets_A, old = c("worker.id" ,"Target_A_Title", "Target_B_Title", "Decoy_A_Title",
                             "Target_A_no","Target_B_no","Decoy_A_no",
                             "Target_A_id", "Target_B_id", "Decoy_A_id", "Target_A_rating",
                             "Target_B_rating","Decoy_A_rating","Seen_all","Notseen_all",
                             "pair_A_mean", "pair_B_mean", "overall_sim"),
         new = c("worker.id" ,"Target", "Competitor", "Decoy",
                 "Target_no","Competitor_no","Decoy_no",
                 "Target_id", "Competitor_id", "Decoy_id", "Target_rating",
                 "Competitor_rating","Decoy_rating","Allseen","Allnotseen",
                 "pair_A_mean", "pair_B_mean", "overall_sim")
)

triplets_B <- quadruplets_for_each[, c("worker.id" ,"Target_B_Title", "Target_A_Title", "Decoy_B_Title",
                                       "Target_B_no","Target_A_no","Decoy_B_no",
                                       "Target_B_id", "Target_A_id", "Decoy_B_id", "Target_B_rating",
                                       "Target_A_rating","Decoy_B_rating","Seen_all","Notseen_all",
                                       "pair_A_mean", "pair_B_mean", "overall_sim")]



setnames(triplets_B, old = c("worker.id" ,"Target_B_Title", "Target_A_Title", "Decoy_B_Title",
                             "Target_B_no","Target_A_no","Decoy_B_no",
                             "Target_B_id", "Target_A_id", "Decoy_B_id", "Target_B_rating",
                             "Target_A_rating","Decoy_B_rating","Seen_all","Notseen_all",
                             "pair_A_mean", "pair_B_mean", "overall_sim"),
         new =c("worker.id" ,"Target", "Competitor", "Decoy",
                "Target_no","Competitor_no","Decoy_no",
                "Target_id", "Competitor_id", "Decoy_id", "Target_rating",
                "Competitor_rating","Decoy_rating","Allseen","Allnotseen",
                "pair_A_mean", "pair_B_mean", "overall_sim"))


triplets <- rbind(triplets_A, triplets_B)
triplets[, id := paste(sort(c(as.character(Target_id), as.character(Competitor_id),as.character(Decoy_id))),
                       collapse = " "),by = 1:nrow(triplets)]


#get id of chosen movie
choice_results[, id := paste(sort(c(as.character(left.book.id), as.character(middle.book.id),as.character(right.book.id))),
                             collapse = " "),by = 1:nrow(choice_results)]
choice_results[, Chosen_id := ifelse(response == "rightmovie", as.character(right.book.id), ifelse(response == "leftmovie",
                                                                                                   as.character(left.book.id), as.character(middle.book.id))),by = 1:nrow(choice_results)]


triplets <- merge(triplets, choice_results[,c("worker.id", "id", "rt", "Chosen_id", "trial.no")], by = c("worker.id", "id"),
                  all.x = TRUE)

setnames(triplets, c("rt"), c("rt_choice"))

triplets[, Which.chosen := c("Target", "Competitor", "Decoy")[which(c(as.character(Target_id),as.character(Competitor_id),as.character(Decoy_id)) ==
                                                                      as.character(Chosen_id))], by = 1:nrow(triplets)]

triplets[, id_comp := paste(sort(c(as.character(Target_id), as.character(Competitor_id))),
                            collapse = " "),by = 1:nrow(triplets)]
triplets[, id_dec := paste(sort(c(as.character(Target_id), as.character(Decoy_id))),
                           collapse = " "),by = 1:nrow(triplets)]


similarity_results[, id := paste(sort(c(as.character(left.book.id), as.character(right.book.id))),
                                 collapse = " "),by = 1:nrow(similarity_results)]

similarity_results <- similarity_results[, list(rt = rt[1],
                                                response = response[1]), by = list(id, worker.id)]
#there seems to be 358 similarity ratings overall


triplets <- merge(triplets, similarity_results[, c("worker.id", "response", "rt", "id")],
                  by.x = c("worker.id", "id_comp"), by.y = c("worker.id", "id"), all.x = TRUE)

setnames(triplets, c("response", "rt"), c("Similarity_comp", "rt_compsim"))

triplets <- merge(triplets, similarity_results[, c("worker.id", "response", "rt", "id")],
                  by.x = c("worker.id", "id_dec"), by.y = c("worker.id", "id"), all.x = TRUE)

setnames(triplets, c("response", "rt"), c("Similarity_dec", "rt_decsim"))

triplets <- triplets[,-c("id_dec", "id_comp", "id")]

save.image("step4_Prepare_choice_data_output.RData")

