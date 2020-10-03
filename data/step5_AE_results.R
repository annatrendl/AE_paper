rm(list=ls())
library(simpleboot)
library(data.table)
library(ggplot2)
library(ggthemes)
library(lme4)
library(stargazer)
setwd("C:/AE_paper/data")
load("step5_AE_results_input.RData")

#theres one NA, get rid of it
triplets <- triplets[!is.na(Which.chosen), ]


#Figure 2
########################
ggplot(decoy_ratings, aes(x=mean)) + geom_histogram(color="black", fill="royalblue", binwidth = 0.2) +
  labs(x="Mean Similarity Rating", y = "Frequency") +
  geom_vline(xintercept = 4.5,linetype="dotted", size = 1.8) + theme_few() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size = 20)) +
  scale_x_continuous(breaks = 1:7, limits = c(0.5,7.5)) 
ggsave("figure2.pdf")

#Figure 4
########################
# triplets[, Subject.no := .GRP, by = Subjno]
# triplets[, Trial.no := 1:.N, .(Subjno)]
triplets[, Targetchosen := ifelse(Which.chosen == "Target",1,0)]
choices_distr <- triplets[Which.chosen != "Decoy", list(Target_prop =sum(Targetchosen)/.N,
                                                        No.choices = .N),by = worker.id]

library(boot)
calcmean <- function(data, index) {
  mean(data[index, Targetchosen])
}

bsTind <- boot(triplets[Which.chosen != "Decoy",],
               statistic=calcmean, strata = as.factor(triplets[Which.chosen != "Decoy",worker.id]), R=10000)
boot.ci(bsTind, conf=0.95, type=c("basic", "bca"))


cis <- data.table(Target_prop = 1, Lower = 1, Upper = 1)
pp <- one.boot(choices_distr[, Target_prop], mean, R=10^4)
cis[,Target_prop := pp$t0]
pp <- boot.ci(pp, type = "perc")
cis[,Lower := pp$percent[,4]]
cis[,Upper := pp$percent[,5]]

range(choices_distr$No.choices)
cis[, No.choices := 50]

ggplot(choices_distr, aes(No.choices, Target_prop)) + 
  geom_point(size = 1)+ theme_few() + geom_jitter(width = 1, height = 0.01)+
  geom_hline(aes(yintercept = 0.5), lty = 4) + xlim(c(0,52)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size = 17), aspect.ratio=4/3) + 
  geom_errorbar(data = cis, aes(ymin = Lower, ymax = Upper),width = 2.5, size = 1.2,colour = "firebrick") +
  labs(y = "Proportion of trials where the target was chosen", x = "Number of choices") +
  ylim(c(0,1)) + 
  scale_x_continuous(breaks = c(0,10,20,30,40, 50), labels = c("0", "10", "20", "30", "40","Mean \nof all "),limits = c(0,53))
ggsave("figure4.pdf")

#t-test, original
choices_distr <- triplets[Which.chosen != "Decoy", list(Target_prop =sum(Targetchosen)/.N,
                                                        No.choices = .N),by = worker.id]

t.test(choices_distr[,Target_prop], alternative = "greater", conf.level=0.95, mu = 0.5)



#Figure 5
########################
triplets[, Similarity_comp := factor(Similarity_comp,
                                     labels = c("1","2","3","4","5","6","7", "Don't know"))]
triplets[, Similarity_dec := factor(Similarity_dec,
                                    labels = c("1","2","3","4","5","6","7", "Don't know"))]
#2 target-comp and 1 target-dec similarity ratings are missing
similarity <- rbind(data.table("rating" = triplets[,Similarity_comp], "type" = "Target-Competitor"),
                    data.table("rating" = triplets[,Similarity_dec], "type" = "Target-Decoy"))
similarity <- similarity[,.N ,.(type, rating)]

ggplot(similarity[!is.na(rating),], aes(rating, N)) + geom_bar(stat = "identity",fill= "royalblue")+
  facet_wrap(~ type, ncol = 1) + theme_few()+
  labs(x= "Similarity Rating (1 = Least Similar, 7 = Most Similar)", y = "Frequency") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size = 18),strip.text = element_text(size = 17),  aspect.ratio=0.4)
ggsave("Figure5.pdf")



#Regression -- original
########################
triplets <- triplets[Which.chosen != "Decoy",]
triplets[, Similarity_dec := as.numeric(Similarity_dec)]
triplets[, Similarity_comp := as.numeric(Similarity_comp)]

triplets[Similarity_comp==8, Similarity_comp := NA]
triplets[Similarity_dec==8, Similarity_dec := NA]

triplets[, Similarity_comp := scale(Similarity_comp)]
triplets[, Similarity_dec := scale(Similarity_dec)]
triplets[, Target_decoy_ratingdiff := Target_rating - Decoy_rating]
triplets[, Target_decoy_ratingdiff := scale(Target_decoy_ratingdiff)]
triplets[, Seen := ifelse(Allseen == TRUE, 0.5, -0.5)]

triplets[, Targetchosen := ifelse(Which.chosen == "Target", 1,0)]

summary(m1 <- glmer(Targetchosen ~ 1 + (1|worker.id), data = triplets, family=binomial(link='logit')))

summary(m2 <- glmer(Targetchosen ~ Seen + Similarity_dec  + Similarity_comp +
              Target_decoy_ratingdiff + (1|worker.id), data = triplets, family=binomial(link='logit')))
#same results
CI.vector_1 <- exp(confint(m1))
OR.vector_1 <- exp(m1@beta)
p.values_1 <- summary(m1)$coefficients[,4]

CI.vector_2 <- exp(confint(m2))
OR.vector_2 <- exp(m2@beta)
p.values_2 <- summary(m2)$coefficients[,4]

stargazer(m1,m2, type = "latex",ci=TRUE,single.row = TRUE,
          coef = list(as.numeric(c(OR.vector_1)),as.numeric(c(OR.vector_2))),
          ci.custom = list(CI.vector_1,CI.vector_2),
          p =list(p.values_1, p.values_2),model.numbers = FALSE,
          covariate.labels = c("Intercept","Seen all", "TC similarity rating", "TD similarity rating",
                               "TD rating difference"),
          dep.var.labels = "Target chosen", column.labels = c("Model 1","Model 2"), 
          title = "", order = c(5,1:4),
          label = "latentattr_exp2reg")




###################################################################

#revisions round 1
rm(list = ls())
library(simpleboot)
library(data.table)
library(ggplot2)
library(ggthemes)
library(lme4)
library(stargazer)

load("step4_Prepare_choice_data_input.RData")
load("C:/AE_paper/data/step1_Create_decoy_target_output.RData")
load("step5_AE_results_input.RData")
triplets <- triplets[!is.na(Which.chosen), ]

#Reviewer comments replies 04/03/2020
#1)check if results are the same if we don't exclude people who chose the decoy (it is possible that people who do not choose the decoy would experience a similarity effect, limiting the attraction effect) 
#just add it to main text
triplets[, Targetchosen := ifelse(Which.chosen == "Target", 1,0)]

#2) run the analysis on the first of each of the choice pairs
#first assign a type to  each target competitor pair
triplets[, TC_pair := paste0(sort(c(Target_no, Competitor_no)), collapse = " "), by = 1:nrow(triplets)]
triplets[, type := .GRP, .(TC_pair, worker.id)]
triplets <- triplets[order(worker.id, type, trial.no)]
triplets[, nos := 1:.N, .(worker.id, type)]
#remove the pair of the missing choice
triplets <- triplets[!(worker.id == "YRVRX7922P" & type == 1039),]

choices_distr <- triplets[Which.chosen != "Decoy" & nos == 1, list(Target_prop =sum(Targetchosen)/.N,
                                                                   No.choices = .N),by = worker.id]

t.test(choices_distr[,Target_prop], conf.level=0.95, mu = 0.5)


choices_distr <- triplets[Which.chosen != "Decoy" & nos == 2, list(Target_prop =sum(Targetchosen)/.N,
                                                                   No.choices = .N),by = worker.id]

t.test(choices_distr[,Target_prop], conf.level=0.95, mu = 0.5)

library(boot)
calcmean <- function(data, index) {
  mean(data[index, Targetchosen])
}

bsTind <- boot(triplets[Which.chosen != "Decoy" & nos == 1,],
               statistic=calcmean, strata = as.factor(triplets[Which.chosen != "Decoy" & nos == 1,worker.id]), R=10000)
boot.ci(bsTind, conf=0.95, type=c("basic", "bca"))

bsTind <- boot(triplets[Which.chosen != "Decoy" & nos == 2,],
               statistic=calcmean, strata = as.factor(triplets[Which.chosen != "Decoy" & nos == 2,worker.id]), R=10000)
boot.ci(bsTind, conf=0.95, type=c("basic", "bca"))

# cis <- data.table(Target_prop = 1, Lower = 1, Upper = 1)
# pp <- one.boot(choices_distr[, Target_prop], mean, R=10^4)
# cis[,Target_prop := pp$t0]
# pp <- boot.ci(pp, type = "perc")
# cis[,Lower := pp$percent[,4]]
# cis[,Upper := pp$percent[,5]]


#3) run the analysis on all those who switched across the two trials and check the proportion of those who switched towards the attraction effect aganst those who did not
rowperTCpair <- triplets[,list(chosenones = paste0(c(Which.chosen), collapse = " "),
                               switched = length(unique(Chosen_id))),.(worker.id, type)]

#get rid of those where the decoy was chosen
rowperTCpair <- rowperTCpair[!grepl("Decoy", chosenones),]

rowperTCpair <- rowperTCpair[switched == 2,]

#testing the proportion of switches - group-level
table(rowperTCpair$chosenones)

prop.test(x = 48, n = 84, p = 0.5, 
          alternative = "greater")

bsTind <- boot(data.table(Targetchosen = c(rep(1,48), rep(0,36))),
               statistic=calcmean, R=10000)

boot.ci(bsTind, conf=0.95, type=c("basic", "bca"))



#next look at genre ratingdiff

rm(list = ls())
load("step4_Prepare_choice_data_input.RData")
load("C:/AE_paper/data/step1_Create_decoy_target_output.RData")
load("step5_AE_results_input.RData")

triplets[, Targetchosen := ifelse(Which.chosen == "Target",1,0)]
triplets <- triplets[!is.na(Which.chosen), ]


triplets[, Similarity_comp := factor(Similarity_comp,
                                     labels = c("1","2","3","4","5","6","7", "Don't know"))]
triplets[, Similarity_dec := factor(Similarity_dec,
                                    labels = c("1","2","3","4","5","6","7", "Don't know"))]


triplets[, Similarity_dec := as.numeric(Similarity_dec)]
triplets[, Similarity_comp := as.numeric(Similarity_comp)]

triplets[Similarity_comp==8, Similarity_comp := NA]
triplets[Similarity_dec==8, Similarity_dec := NA]

triplets[, Targetchosen := ifelse(Which.chosen == "Target", 1,0)]
triplets[, Target_decoy_ratingdiff := Target_rating - Decoy_rating]

triplets <- triplets[Which.chosen != "Decoy",]




ratings <- trials[trial.type == "rating",]
ratings <- merge(ratings[trial.type == "rating",], genre_info, by.x = "book.id",
                 by.y = "Movie.id", all.x = T)
ratings[, response := as.numeric(factor(response, levels = as.character(seq(1,7))))]
genre_mean <- ratings[, list(mean_rating = mean(response)), .(worker.id, Category)]




triplets[, Similarity_comp := scale(Similarity_comp)]
triplets[, Similarity_dec := scale(Similarity_dec)]
triplets[, Target_decoy_ratingdiff := Target_rating - Decoy_rating]
triplets[, Target_decoy_ratingdiff := scale(Target_decoy_ratingdiff)]
triplets[, Seen := ifelse(Allseen == TRUE, 0.5, -0.5)]

triplets[, Targetchosen := ifelse(Which.chosen == "Target", 1,0)]                                                                    


###################now we have to link it back to triplets dataset so need data on genre of each movie


triplets <- merge(triplets, genre_info[, c("Movie.id", "Category")], 
                  by.x = "Target_id", by.y = "Movie.id", all.x = T)
setnames(triplets, c("Category"), c("Target_Category"))

triplets <- merge(triplets, genre_info[, c("Movie.id", "Category")], 
                  by.x = "Competitor_id", by.y = "Movie.id", all.x = T)
setnames(triplets, c("Category"), c("Competitor_Category"))


triplets <- merge(triplets, genre_mean, by.y = c("worker.id", "Category"),
                  by.x = c("worker.id", "Target_Category"), all.x = T)

setnames(triplets, c("mean_rating"), c("target_genre_av"))

triplets <- merge(triplets, genre_mean, by.y = c("worker.id", "Category"),
                  by.x = c("worker.id", "Competitor_Category"), all.x = T)

setnames(triplets, c("mean_rating"), c("competitor_genre_av"))



triplets[, genre_ratingdiff := scale(target_genre_av - competitor_genre_av)]

triplets[, genre_ratingdiff_raw := target_genre_av - competitor_genre_av]



#summary(m1 <- glmer(Targetchosen ~ 1 + (1|worker.id), data = triplets, family=binomial(link='logit')))

#summary(m2 <- glmer(Targetchosen ~ Seen + Similarity_dec  + Similarity_comp +
#                      Target_decoy_ratingdiff + (1|worker.id), data = triplets, family=binomial(link='logit')))


triplets[, genrechoice := ifelse((target_genre_av > competitor_genre_av & Targetchosen == 1) |
                                   (competitor_genre_av > target_genre_av & Targetchosen == 0), 0.5, -0.5) ]


triplets <- triplets[Which.chosen != "Decoy",]


triplets[, TC_pair := paste0(sort(c(Target_no, Competitor_no)), collapse = " "), by = 1:nrow(triplets)]
triplets[, type := .GRP, .(TC_pair, worker.id)]
triplets <- triplets[order(worker.id, type, trial.no)]
triplets[, nos := 1:.N, .(worker.id, type)]

summary(m3 <- glmer(Targetchosen ~ genre_ratingdiff + (1|worker.id), data = triplets, family=binomial(link='logit')))
#summary(m4 <- glmer(Targetchosen ~ genrechoice + (1|worker.id), data = triplets, family=binomial(link='logit')))
#summary(m5 <- glmer(Targetchosen ~ genre_ratingdiff + genrechoice + (1|worker.id), data = triplets, family=binomial(link='logit')))


# summary(m3 <- glmer(Targetchosen ~ Seen + Similarity_dec  + Similarity_comp +
#                       Target_decoy_ratingdiff + genre_ratingdiff + (1|worker.id), data = triplets, family=binomial(link='logit')))
# 
summary(m4 <- glmer(Targetchosen ~ Seen + Similarity_dec  + Similarity_comp +
                       Target_decoy_ratingdiff + genre_ratingdiff  + (1|worker.id), data = triplets, family=binomial(link='logit')))
# 
# triplets[, genrechoices := ifelse((target_genre_av > competitor_genre_av & Targetchosen == 1) |
#                                    (competitor_genre_av > target_genre_av & Targetchosen == 0), 0.5, -0.5) ]


#same results

CI.vector_3 <- exp(confint(m3))
OR.vector_3 <- exp(m3@beta)
p.values_3 <- summary(m3)$coefficients[,4]


CI.vector_4 <- exp(confint(m4))
OR.vector_4 <- exp(m4@beta)
p.values_4 <- summary(m4)$coefficients[,4]


# CI.vector_5 <- exp(confint(m5))
# OR.vector_5 <- exp(m5@beta)
# p.values_5 <- summary(m5)$coefficients[,4]
# 
# 
# CI.vector_6 <- exp(confint(m6))
# OR.vector_6 <- exp(m6@beta)
# p.values_6 <- summary(m6)$coefficients[,4]

# 
# stargazer(m3, m4, m5, m6, type = "latex",ci=TRUE,no.space = T,
#           coef = list(as.numeric(c(OR.vector_3)),as.numeric(c(OR.vector_4)),
#                       as.numeric(c(OR.vector_5)),as.numeric(c(OR.vector_6))),
#           ci.custom = list(CI.vector_3,CI.vector_4,
#                            CI.vector_5,CI.vector_6),
#           p =list(p.values_3, p.values_4,p.values_5, p.values_6),model.numbers = FALSE,
#           dep.var.labels = "Target chosen", 
#           column.labels = c("Model 3","Model 4","Model 5","Model 6"), 
#           title = "")

stargazer(m3, m4, type = "latex",ci=TRUE,single.row = TRUE,
          coef = list(as.numeric(c(OR.vector_3)),as.numeric(c(OR.vector_4))),
          ci.custom = list(CI.vector_3,CI.vector_4),
          p =list(p.values_3, p.values_4),model.numbers = FALSE,
          dep.var.labels = "Target chosen", column.labels = c("Model 3","Model 4"),
          title = "")



summary(m3 <- glmer(Targetchosen ~ genre_ratingdiff + (1|worker.id), data = triplets[nos == 1,], family=binomial(link='logit')))
summary(m4 <- glmer(Targetchosen ~ Seen + Similarity_dec  + Similarity_comp +
                      Target_decoy_ratingdiff + genre_ratingdiff  + (1|worker.id), data = triplets[nos == 1,], family=binomial(link='logit')))


CI.vector_3 <- exp(confint(m3))
OR.vector_3 <- exp(m3@beta)
p.values_3 <- summary(m3)$coefficients[,4]


CI.vector_4 <- exp(confint(m4))
OR.vector_4 <- exp(m4@beta)
p.values_4 <- summary(m4)$coefficients[,4]


stargazer(m3, m4, type = "latex",ci=TRUE,single.row = TRUE,
          coef = list(as.numeric(c(OR.vector_3)),as.numeric(c(OR.vector_4))),
          ci.custom = list(CI.vector_3,CI.vector_4),
          p =list(p.values_3, p.values_4),model.numbers = FALSE,
          dep.var.labels = "Target chosen", column.labels = c("Model 3","Model 4"),
          title = "")






library(boot)
calcmean <- function(data, index) {
  mean(data[index, Targetchosen])
}

bsTind <- boot(triplets[abs(genre_ratingdiff_raw) < 0.25 & Which.chosen != "Decoy",],
               statistic=calcmean, R=10000)
boot.ci(bsTind, conf=0.95, type=c("basic", "bca"))


choices_distr <- triplets[abs(genre_ratingdiff_raw) < 0.25 & Which.chosen != "Decoy", list(Target_prop =sum(Targetchosen)/.N,
                                                        No.choices = .N),by = worker.id]

t.test(choices_distr[,Target_prop], alternative = "greater", conf.level=0.95, mu = 0.5)

##################################################################################

#plot the attraction effect as a function of similarity and decoy target rating difference



rm(list = ls())
load("step4_Prepare_choice_data_input.RData")
load("C:/AE_paper/data/step1_Create_decoy_target_output.RData")
load("step5_AE_results_input.RData")

triplets[, Targetchosen := ifelse(Which.chosen == "Target",1,0)]
triplets <- triplets[!is.na(Which.chosen), ]


triplets[, Similarity_comp := factor(Similarity_comp,
                                     labels = c("1","2","3","4","5","6","7", "Don't know"))]
triplets[, Similarity_dec := factor(Similarity_dec,
                                    labels = c("1","2","3","4","5","6","7", "Don't know"))]


triplets[, Similarity_dec := as.numeric(Similarity_dec)]
triplets[, Similarity_comp := as.numeric(Similarity_comp)]

triplets[Similarity_comp==8, Similarity_comp := NA]
triplets[Similarity_dec==8, Similarity_dec := NA]

triplets[, Targetchosen := ifelse(Which.chosen == "Target", 1,0)]
triplets[, Target_decoy_ratingdiff := Target_rating - Decoy_rating]

triplets <- triplets[Which.chosen != "Decoy",]

triplets[, TC_pair := paste0(sort(c(Target_no, Competitor_no)), collapse = " "), by = 1:nrow(triplets)]
triplets[, type := .GRP, .(TC_pair, worker.id)]
triplets <- triplets[order(worker.id, type, trial.no)]
triplets[, nos := 1:.N, .(worker.id, type)]



summary(m5 <- glm(Targetchosen ~ as.factor(Target_decoy_ratingdiff)+as.factor(Similarity_dec),
                  data = triplets, family=binomial(link='logit')))


newdata <- expand.grid(Target_decoy_ratingdiff = unique(triplets$Target_decoy_ratingdiff),
                       Similarity_dec = na.omit(unique(triplets$Similarity_dec)))


ilink <- family(m5)$linkinv

newdata <- cbind(newdata, predict(m5, newdata, type = "link", se.fit = TRUE)[1:2])
newdata <- transform(newdata, Fitted = ilink(fit), Upper = ilink(fit + (2 * se.fit)),
                Lower = ilink(fit - (2 * se.fit)))





ggplot(newdata, aes(Similarity_dec, Fitted, shape = as.factor(Target_decoy_ratingdiff))) + 
  theme_bw() +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), position = position_dodge(width = 0.5)) +
  geom_hline(aes(yintercept = 0.5), linetype = 4)  +
  ylim(c(0,1))+ theme(legend.position = "bottom", text = element_text(size = 20)) +
  scale_x_continuous(breaks = 1:7) +
  labs(x = "Target-Decoy similarity rating", y = "Predicted probability of\nchoosing the target") +
  guides(shape=guide_legend(title='Target-Decoy rating difference'))

ggsave("figure6.pdf", width = 8, height = 6)








calcmean <- function(data, index) {
  mean(data[index, Targetchosen])
}

toplot <- data.table(expand.grid(Target_rating = unique(triplets$Target_rating), Prop = 0, Lower = 0, Upper = 0))

for (i in 1:nrow(toplot)) {
  dataset <- triplets[Which.chosen != "Decoy" & Target_rating == toplot[i, Target_rating,] & nos == 1,]
  bsTind <- boot(dataset,
                 statistic=calcmean, strata = as.factor(dataset$worker.id), R=10000)
  bootcis<- boot.ci(bsTind, conf=0.95, type=c("basic", "bca"))
  toplot[i, Prop := bsTind$t0]
  toplot[i, Lower := bootcis$bca[4]]
  toplot[i, Upper := bootcis$bca[5]]
  print(i)
}


ggplot(toplot, aes(Target_rating, Prop)) + geom_point() + theme_bw()  +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2) +
  geom_hline(aes(yintercept = 0.5), linetype = 4) +
  ylim(c(0,1))+
  labs(x = "Target and Competitor rating", y = "Probability of\nchoosing the target") + 
  theme(text = element_text(size = 20))
  
ggsave("figure7.pdf", width = 8, height = 6)

#revisions round 2 28/06/2020
rm(list = ls())
library(simpleboot)
library(data.table)
library(ggplot2)
library(ggthemes)
library(lme4)
library(stargazer)
load("step4_Prepare_choice_data_input.RData")
load("C:/AE_paper/data/step1_Create_decoy_target_output.RData")
load("step5_AE_results_input.RData")

triplets[, Similarity_comp := factor(Similarity_comp,
                                     labels = c("1","2","3","4","5","6","7", "Don't know"))]
triplets[, Similarity_dec := factor(Similarity_dec,
                                    labels = c("1","2","3","4","5","6","7", "Don't know"))]


triplets[, Similarity_dec := as.numeric(Similarity_dec)]
triplets[, Similarity_comp := as.numeric(Similarity_comp)]

triplets[Similarity_comp==8, Similarity_comp := NA]
triplets[Similarity_dec==8, Similarity_dec := NA]
########################
triplets[, Targetchosen := ifelse(Which.chosen == "Target", 1,0)]
triplets <- triplets[Which.chosen != "Decoy",]
triplets[, Similarity_dec := as.numeric(Similarity_dec)]
triplets[, Similarity_comp := as.numeric(Similarity_comp)]

triplets[Similarity_comp==8, Similarity_comp := NA]
triplets[Similarity_dec==8, Similarity_dec := NA]

triplets[, Similarity_comp := scale(Similarity_comp)]
triplets[, Similarity_dec := scale(Similarity_dec)]
triplets[, Target_decoy_ratingdiff := Target_rating - Decoy_rating]
triplets[, Target_decoy_ratingdiff := scale(Target_decoy_ratingdiff)]
triplets[, Seen := ifelse(Allseen == TRUE, 0.5, -0.5)]
#limit analysis to first choice
#first need to find out order
triplets <- merge(triplets, choice_results[, c("worker.id", "trial.no", "left.book.no", 
                                               "middle.book.no", "right.book.no", "left.book.id", 
                                               "middle.book.id", "right.book.id")], by = c("worker.id", "trial.no"), all.x = T)

#create variable that captures order of options
for (i in 1:nrow(triplets)) {
  triplets[i, order := paste(c("Target", "Competitor", "Decoy")[match(triplets[i,c("Target_no", "Competitor_no", "Decoy_no")],triplets[i,c("left.book.no", "middle.book.no", "right.book.no")])], collapse = " ")]
}
triplets[, order := as.factor(order)]

triplets[, TC_pair := paste0(sort(c(Target_no, Competitor_no)), collapse = " "), by = 1:nrow(triplets)]
triplets[, type := .GRP, .(TC_pair, worker.id)]
triplets <- triplets[order(worker.id, type, trial.no)]
triplets[, nos := 1:.N, .(worker.id, type)]
#overall number of choices per pt
triplets[, no_choices := .N,.(worker.id)]

triplets[, RT_choice := scale(rt_choice)]


#add overall rating, 
#reorder trial no only including the first trial
#triplets[nos == 1, order_redefined := rank(trial.no), .(worker.id)]

summary(m1 <- glmer(Targetchosen ~ Seen + Similarity_dec  + Similarity_comp +
                      Target_decoy_ratingdiff +
                      (1|worker.id), data = triplets[nos == 1,],
                    family=binomial(link='logit')))


triplets[, Target_rating := scale(Target_rating)]
#triplets[, order_redefined := scale(order_redefined)]
#triplets[, no_choices := scale(no_choices)]


#scale everything else apart from order
summary(m2 <- glmer(Targetchosen ~ Seen + Similarity_dec  + Similarity_comp +
                      Target_decoy_ratingdiff + Target_rating + order + no_choices + trial.no + RT_choice +
                      (1|worker.id), data = triplets[nos == 1,],
                    family=binomial(link='logit')))

CI.vector_1 <- exp(confint(m1))
OR.vector_1 <- exp(m1@beta)
p.values_1 <- summary(m1)$coefficients[,4]

CI.vector_2 <- exp(confint(m2))
OR.vector_2 <- exp(m2@beta)
p.values_2 <- summary(m2)$coefficients[,4]

m0 <- glmer(Targetchosen ~ 1 +
              (1|worker.id), data = triplets[nos == 1,],
            family=binomial(link='logit'))


CI.vector_0 <- exp(confint(m0))
OR.vector_0 <- exp(m0@beta)
p.values_0 <- summary(m0)$coefficients[,4]

stargazer(m0,m1,m2, type = "latex",ci=TRUE,single.row = TRUE,
          coef = list(as.numeric(c(OR.vector_0)),as.numeric(c(OR.vector_1)),as.numeric(c(OR.vector_2))),
          ci.custom = list(CI.vector_0,CI.vector_1,CI.vector_2),
          p =list(p.values_0,p.values_1, p.values_2),model.numbers = FALSE,
          covariate.labels = c("Intercept","Seen all", "TC similarity rating", "TD similarity rating",
                               "TD rating difference", "TC rating", "order:CTD", "order:DCT", "order:TCD",
                               "order:TDC", "number of choices", "trial number", "reaction time"),
          dep.var.labels = "Target chosen", column.labels = c("Model 1","Model 2", "Model 3"))



stargazer(m0,m1,m2, type  = "latex", ci = T, single.row = T, coef = list(as.numeric(c(OR.vector_0)),as.numeric(c(OR.vector_1)),as.numeric(c(OR.vector_2))),
          ci.custom = list(CI.vector_0,CI.vector_1,CI.vector_2),
          p =list(p.values_0,p.values_1, p.values_2))


#revision 3 03/10/2020

#to get a sense of the individual variation, bootstrap the number of targets chosen
calcmean <- function(data, index) {
  mean(data[index, Targetchosen])
}


toplot <- data.table(worker.id = unique(triplets$worker.id))


for (i in 1:nrow(toplot)) {
bsTind <- boot(triplets[Which.chosen != "Decoy" & worker.id == toplot[i, worker.id],],
               statistic=calcmean, R=100)
cis <- boot.ci(bsTind, conf=0.95, type=c("basic", "bca"))
toplot[i, prop := bsTind$t0]
toplot[i, lower := cis$bca[,4]]
toplot[i, upper := cis$bca[,5]]
print(i)
}

cis <- data.table(Target_prop = 1, Lower = 1, Upper = 1)
pp <- one.boot(choices_distr[, Target_prop], mean, R=10^4)
cis[,Target_prop := pp$t0]
pp <- boot.ci(pp, type = "perc")
cis[,Lower := pp$percent[,4]]
cis[,Upper := pp$percent[,5]]

range(choices_distr$No.choices)
cis[, No.choices := 50]


