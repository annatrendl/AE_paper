rm(list=ls())
library(simpleboot)
library(data.table)
library(ggplot2)
library(ggthemes)
library(lme4)
library(stargazer)
setwd("C:/Anna/latent_attraction/AE_paper/OSF")
load("AE_results_input.RData")


#Figure 2
########################
ggplot(decoy_pairs, aes(x=mean)) + geom_histogram(color="black", fill="royalblue", binwidth = 0.2) +
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

cis <- data.table(Target_prop = 1, Lower = 1, Upper = 1)
pp <- one.boot(choices_distr[, Target_prop], mean, R=10^4)
cis[,Target_prop := pp$t0]
pp <- boot.ci(pp, type = "perc")
cis[,Lower := pp$percent[,4]]
cis[,Upper := pp$percent[,5]]

table(triplets$Targetchosen)
binom.confint(1027, 1027+1028)


range(choices_distr$No.choices)
cis[, No.choices := 24]

ggplot(choices_distr, aes(No.choices, Target_prop)) + 
  geom_point()+ theme_few() + geom_jitter(width = 1, height = 0.01)+
  geom_hline(aes(yintercept = 0.5), lty = 4) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size = 18), aspect.ratio=4/3) + 
  geom_errorbar(data = cis, aes(ymin = Lower, ymax = Upper),width = 2.5, size = 1.2,colour = "firebrick") +
  labs(y = "Proportion of trials where the target was chosen", x = "Number of choices") + ylim(c(0,1))
ggsave("figure4.pdf")

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


#Regression
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

# Dumb but wrong logistic regression
summary(m0 <- glm(Targetchosen ~ 1, data = triplets, family=binomial(link='logit')))
exp(c(coef(m0), confint(m0)))

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




