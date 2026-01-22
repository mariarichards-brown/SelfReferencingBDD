# remove everything
rm(list=ls())
# call our functions
source("functions_v3.R")

if (!require(ggplot2)) {install.packages("ggplot2")}; library(ggplot2)
if (!require(ggpubr)) {install.packages("ggpubr")}; library(ggpubr)
if (!require(lmerTest)) {install.packages("lmerTest")}; library(lmerTest)
if (!require(effectsize)) {install.packages("effectsize")}; library(effectsize)
if (!require(report)) {install.packages("report")}; library(report)
options(es.use_symbols = TRUE) # get nice symbols when printing! (On Windows, requires R >= 4.2.0)
if (!require(dplyr)) {install.packages("dplyr")}; library(dplyr)


# do you want to print the output?
printFiles <- 1

# read data exp1
e1_wf <- read.csv("exp1_wf.csv")
e1_lf <- read.csv("exp1_lf.csv")

# read data exp2
e2_wf <- read.csv("exp2_wf.csv")
e2_lf <- read.csv("exp2_lf.csv")


# re-grouping blocks into 30 trials per block
e1_lf$newBlock <- ceiling(e1_lf$nTrial / 30)
e2_lf$newBlock <- ceiling(e2_lf$nTrial / 30)
# combine experiments 1 and 3 (not including 2a and 2b) long format
lf <- rbind(e1_lf, e2_lf)

# combine experiments wide format
relCols <- c("exp", "partId","age","sex","bddq","phq","bis","gad","pairing_duration")
wf <- rbind(e1_wf[,relCols],e2_wf[,relCols])

# add questionnaires to long format
lf <- addQuestionnairesToLF(lf, wf)

# word factor in desired order
lf$word <- factor(lf$word, levels = c("You","Friend","Stranger"))

# sample size before cleaning?
table(wf$exp); table(wf$exp,wf$sex)



############################################################################## #
# removing bad participants, criteria:
#  - trial level: remove <200ms and >1400
#  - participant: remove overall correct <65%
lf <- lf[lf$rt > 200 & lf$rt < 1400,]
# lf <- lf[lf$rt > 200,] # sensitivity analysis
# calculate mean of correct
temp <- lf %>% group_by(partId) %>%
  summarise(mCorrect = mean(corr), mRt = mean(rt), n = n())
# detect bad participants (<65%)
temp <- temp$partId[temp$mCorrect < 0.65]
for (i in 1:length(temp)) {
  lf <- lf[lf$partId != temp[i],]
  wf <- wf[wf$partId != temp[i],]
}
# manual remove, this participant has only one good trial after above criterion
lf <- lf[lf$partId != 10446215,] 
wf <- wf[wf$partId != 10446215,] 

# sample size after cleaning?
table(wf$exp); table(wf$exp,wf$sex)

# descriptive statistics
# age
f_descrContinuous(wf$age[wf$exp == "e1"])
f_descrContinuous(wf$age[wf$exp == "e2"])
# sex
f_descrCategorical(wf$sex[wf$exp == "e1"])
f_descrCategorical(wf$sex[wf$exp == "e2"])
# bddq
f_descrContinuous(wf$bddq[wf$exp == "e1"])
f_descrContinuous(wf$bddq[wf$exp == "e2"])
# phq
f_descrContinuous(wf$phq[wf$exp == "e2"])
# bis
f_descrContinuous(wf$bis[wf$exp == "e2"])
# gad
f_descrContinuous(wf$gad[wf$exp == "e2"])





# # # # # Experiment 1 # # # # # # # # # # # # # # # # # # # # # # # # # # #### 

# data for experiment 1
temp <- lf[lf$exp=="e1",]

# # # # accuracy E1 # # # #
(fig2 <- ggplot(temp, aes(x = word, y = corr, shape = factor(match), col = factor(match))) +  
    stat_summary(fun = mean, geom = "point", size = 3) +  # Plot means  
    stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + # Add bootstrapped CIs  
    theme_classic() +  
    theme(
      axis.text.x = element_text(angle = 30, hjust = 1),  
      axis.title.x = element_text(face = "bold"),  
      axis.title.y = element_text(face = "bold"),  
      text = element_text(family = "Times New Roman"),  
      legend.title = element_text(face = "bold"),  
      legend.text = element_text(size = 10),  
      legend.background = element_rect(fill = "white", color = "black"),  
      legend.box = "horizontal"  
    ) +  
    scale_shape_manual(values = c(16, 17)) +  # Ensure shape values align with factor levels  
    scale_colour_manual(values = c("black", "grey")) +  
    labs(
      x = "Word Type", 
      y = "Accuracy", 
      col = "Congruency", 
      shape = "Congruency"
    ) +  
    coord_cartesian(ylim = c(0.75, 1)))  # Set y-axis limit


# # # Main Self-Prioritisation effect E1 # # #
# model
m1 <- glmer(corr ~ word * match + (1|partId), family = binomial, temp)
summary(m1)
# does the results are affected by the you-shape counterbalance?
# fit a model that includes the factor selfShape
m0 <- glmer(corr ~ word * match + selfShape + (1|partId), family = binomial, temp)
# compare the 
anova(m1, m0)
# compute the effect size of the regular model (m)
tm1 <- report_table(m1)


# # # # accuracy E1 bddq # # # #
(fig3 <- ggplot(temp, aes(x = bddq, y = corr, linetype = word)) + 
    stat_smooth(method = "lm", se = TRUE, color = "black") +  # Black smooth lines with SE   
    facet_grid(~newBlock) + # facet by newBlock   
    theme_classic() +   
    labs(x = "BDDQ Score",                      # X-axis label
         y = "Accuracy",              # Y-axis label
         title = "Accuracy for Judging Object-Label Pairings by BDDQ by Block"  # Graph title
    ) +  
    scale_linetype_manual(values = c("solid", "dashed", "twodash"), guide = guide_legend(title = "Word Type")) +  # Customize line types   
    theme(text = element_text(family = "Times New Roman"),
          legend.title = element_text(),
          legend.background = element_rect(fill = "white", color = "black"),  # Add box around legend     
          legend.box.background = element_rect(color = "black"),  # Box around legend     
          strip.text.x = element_text(angle = 0),  # Adjust facet text     
          strip.placement = "outside",     
          strip.text.x.top = element_text(face = "bold")  # Bold the facet label   
    ) + 
    coord_cartesian(ylim = c(0.6, 1)))

# # # BDDQ and blocks E1 # # #
m_e1_co <- glmer(corr ~ bddq * word * newBlock * match + (newBlock|partId), 
                 family = binomial, data = temp, 
                 control= glmerControl(optimizer = "bobyqa")) 
summary(m_e1_co)

# # # BDDQ and block 1 E1 # # #
temp <- lf[lf$exp=="e1" & lf$newBlock == 1,] 
m_e1_b1_co <- glmer(corr ~ bddq * word * match + (1|partId), 
                    family = binomial, data = temp, 
                    control= glmerControl(optimizer = "bobyqa")) 
summary(m_e1_b1_co)
# see here (22/01/2025)
ggplot(temp, aes(x = bddq, y = corr, col = word)) + 
  stat_smooth(method = "lm", se = TRUE) + stat_cor()


# # # # reaction time E1 # # # #
temp <- lf[lf$exp=="e1",]
(fig4 <- ggplot(temp, aes(x = word, y = rt, shape = factor(match), col = factor(match))) +  
    stat_summary(fun = mean, geom = "point", size = 3) +  # Plot means  
    stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +  # Add bootstrapped CIs  
    theme_classic() +  
    theme(
      axis.text.x = element_text(angle = 30, hjust = 1),  
      axis.title.x = element_text(face = "bold"),  
      axis.title.y = element_text(face = "bold"),  
      text = element_text(family = "Times New Roman"),  
      legend.title = element_text(face = "bold"),  
      legend.text = element_text(size = 10),  
      legend.background = element_rect(fill = "white", color = "black"),  
      legend.box = "horizontal"  
    ) +  
    scale_shape_manual(values = c(16, 17)) +  # Ensure shape values align with factor levels  
    scale_colour_manual(values = c("black", "grey")) +  
    labs(
      x = "Word Type", 
      y = "Reaction Time (ms)", 
      col = "Congruency", 
      shape = "Congruency"
    ) +  
    coord_cartesian(ylim = c(725, 875)))  # Set y-axis limit  


# # # Main Self-Prioritisation effect E1 # # #
# model
m1 <- lmer(rt ~ word * match + (1|partId), REML = F, temp)
summary(m1)
m0 <- lmer(rt ~ word * match + selfShape + (1|partId), REML = F, temp)
anova(m1, m0)
# compute the effect size of the regular model (m)
tm1 <- report_table(m1)

# # # # RT E1 bddq # # # #
(fig5 <- ggplot(temp, aes(x = bddq, y = rt, linetype = word)) + 
  stat_smooth(method = "lm", se = TRUE, color = "black") +  # Black smooth lines with SE
  facet_grid(~newBlock) +  # Facet by newBlock, which now has custom labels
  theme_classic() +
  labs(
    x = "BDDQ Score",                      # X-axis label
    y = "Reaction Time (ms)",              # Y-axis label
    title = "Reaction Time for Judging Object-Label Pairings by BDDQ by Block"  # Graph title
  ) +
  scale_linetype_manual(values = c("solid", "dashed", "twodash"), 
                        guide = guide_legend(title = "Word Type")) +  # Customize line types
  theme(
    text = element_text(family = "Times New Roman"),
    legend.title = element_text(),  
    legend.background = element_rect(fill = "white", color = "black"),  # Add box around legend
    legend.box.background = element_rect(color = "black"),  # Box around legend
    strip.text.x = element_text(angle = 0),  # Adjust facet text
    strip.placement = "outside",
    strip.text.x.top = element_text(face = "bold")  # Bold the facet label
  ))

# # # BDDQ and blocks E1 # # #
m_e1_rt <- lmer(rt ~ bddq * word * newBlock * match + (newBlock|partId), 
                REML = F, data = temp, 
                control= lmerControl(optimizer = "bobyqa")) 
summary(m_e1_rt)

# # # BDDQ and block 1 E1 (should not be needed) # # #
temp <- lf[lf$exp=="e1" & lf$newBlock == 1,] 
m_e1_b1_rt <- lmer(rt ~ bddq * word * match + (1|partId), 
                   REML = F, data = temp, 
                   control= lmerControl(optimizer = "bobyqa")) 
summary(m_e1_b1_rt)



# # # # # Experiment 2 # # # # # # # # # # # # # # # # # # # # # # # # # # #### 

# data for experiment 2
temp <- lf[lf$exp=="e2",]

# # # # accuracy E2 # # # #
(fig6 <- ggplot(temp, aes(x = word, y = corr, shape = factor(match), col = factor(match))) +  
  stat_summary(fun = mean, geom = "point", size = 3) +  # Plot means  
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +  # Add bootstrapped CIs  
  theme_classic() +  
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),  
    axis.title.x = element_text(face = "bold"),  
    axis.title.y = element_text(face = "bold"),  
    text = element_text(family = "Times New Roman"),  
    legend.title = element_text(face = "bold"),  
    legend.text = element_text(size = 10),  
    legend.background = element_rect(fill = "white", color = "black"),  
    legend.box = "horizontal"  
  ) +  
  scale_shape_manual(values = c(16, 17)) +  # Ensure shape values align with factor levels  
  scale_colour_manual(values = c("black", "grey")) +  
  labs(
    x = "Word Type", 
    y = "Accuracy", 
    col = "Congruency", 
    shape = "Congruency"
  ) +  
  coord_cartesian(ylim = c(0.75, 1)))  # Set y-axis limit  


# # # Main Self-Prioritisation effect E1 # # #
# model
m1 <- glmer(corr ~ word * match + (1|partId), family = binomial, temp)
summary(m1)
m0 <- glmer(corr ~ word * match + selfShape + (1|partId), family = binomial, temp)
anova(m1, m0)
# compute the effect size of the regular model (m)
tm1 <- report_table(m1)


# # # # accuracy E2 bddq # # # #
(fig7 <- ggplot(temp, aes(x = bddq, y = corr, linetype = word)) + 
    stat_smooth(method = "lm", se = TRUE, color = "black") +  # Black smooth lines with SE   
    facet_grid(~newBlock) + # facet by newBlock   
    theme_classic() +   
    labs(x = "BDDQ Score",                      # X-axis label
         y = "Accuracy",              # Y-axis label
         title = "Accuracy for Judging Object-Label Pairings by BDDQ by Block"  # Graph title
    ) +  
    scale_linetype_manual(values = c("solid", "dashed", "twodash"), guide = guide_legend(title = "Word Type")) +  # Customize line types   
    theme(text = element_text(family = "Times New Roman"),
          legend.title = element_text(),
          legend.background = element_rect(fill = "white", color = "black"),  # Add box around legend     
          legend.box.background = element_rect(color = "black"),  # Box around legend     
          strip.text.x = element_text(angle = 0),  # Adjust facet text     
          strip.placement = "outside",     
          strip.text.x.top = element_text(face = "bold")  # Bold the facet label   
    ) + 
    coord_cartesian(ylim = c(0.6, 1)))

# # # BDDQ and blocks E2 # # #
m_e2_co <- glmer(corr ~ bddq * word * newBlock * match + (newBlock|partId), 
                 family = binomial, data = temp, 
                 control= glmerControl(optimizer = "bobyqa")) 
summary(m_e2_co)

# # # BDDQ and block 1 E2 # # #
temp <- lf[lf$exp=="e2" & lf$newBlock == 1,] 
m_e2_b1_co <- glmer(corr ~ bddq * word * match + (1|partId), 
                    family = binomial, data = temp, 
                    control= glmerControl(optimizer = "bobyqa")) 


# # # # reaction time E2 # # # #
temp <- lf[lf$exp=="e2",]
(fig8 <- ggplot(temp, aes(x = word, y = rt, shape = factor(match), col = factor(match))) +  
  stat_summary(fun = mean, geom = "point", size = 3) +  # Plot means  
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +  # Add bootstrapped CIs  
  theme_classic() +  
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),  
    axis.title.x = element_text(face = "bold"),  
    axis.title.y = element_text(face = "bold"),  
    text = element_text(family = "Times New Roman"),  
    legend.title = element_text(face = "bold"),  
    legend.text = element_text(size = 10),  
    legend.background = element_rect(fill = "white", color = "black"),  
    legend.box = "horizontal"  
  ) +  
  scale_shape_manual(values = c(16, 17)) +  # Ensure shape values align with factor levels  
  scale_colour_manual(values = c("black", "grey")) +  
  labs(
    x = "Word Type", 
    y = "Reaction Time (ms)", 
    col = "Congruency", 
    shape = "Congruency"
  ) +  
  coord_cartesian(ylim = c(725, 875)))  # Set y-axis limit  


# # # Main Self-Prioritisation effect E2 # # #
# model
m1 <- lmer(rt ~ word * match + (1|partId), REML = F, temp)
summary(m1)
m0 <- lmer(rt ~ word * match + selfShape + (1|partId), REML = F, temp)
anova(m1, m0)
# compute the effect size of the regular model (m)
tm1 <- report_table(m1)

# # # # RT E2 bddq # # # #
(fig9 <- ggplot(temp, aes(x = bddq, y = rt, linetype = word)) + 
    stat_smooth(method = "lm", se = TRUE, color = "black") +  # Black smooth lines with SE
    facet_grid(~newBlock) +  # Facet by newBlock, which now has custom labels
    theme_classic() +
    labs(
      x = "BDDQ Score",                      # X-axis label
      y = "Reaction Time (ms)",              # Y-axis label
      title = "Reaction Time for Judging Object-Label Pairings by BDDQ by Block"  # Graph title
    ) +
    scale_linetype_manual(values = c("solid", "dashed", "twodash"), 
                          guide = guide_legend(title = "Word Type")) +  # Customize line types
    theme(
      text = element_text(family = "Times New Roman"),
      legend.title = element_text(),  
      legend.background = element_rect(fill = "white", color = "black"),  # Add box around legend
      legend.box.background = element_rect(color = "black"),  # Box around legend
      strip.text.x = element_text(angle = 0),  # Adjust facet text
      strip.placement = "outside",
      strip.text.x.top = element_text(face = "bold")  # Bold the facet label
    ))

# # # BDDQ and blocks E1 # # #
m_e2_rt <- lmer(rt ~ bddq * word * newBlock * match + (newBlock|partId), 
                REML = F, data = temp, 
                control= lmerControl(optimizer = "bobyqa")) 
summary(m_e2_rt)

# # # BDDQ and block 1 E2 # # #
temp <- lf[lf$exp=="e2" & lf$newBlock == 1,] 
m_e2_b1_rt <- lmer(rt ~ bddq * word * match + (1|partId), 
                   REML = F, data = temp, 
                   control= lmerControl(optimizer = "bobyqa")) 
summary(m_e2_b1_rt)



# # # # # # # # # # # # # #
# # # # # PHQ E2 # # # # #
# # # # # # # # # # # # # #

# # # # Accuracy PHQ E2 # # # #
temp <- lf[lf$exp=="e2",]
m_e2_co_phq <- glmer(corr ~ phq * word * newBlock * match + (newBlock|partId), 
                 family = binomial, data = temp, 
                 control= glmerControl(optimizer = "bobyqa")) 
summary(m_e2_co_phq)

# # # PHQ Accuracy and block 1 E2 (if main effect of block) # # #
temp <- lf[lf$exp=="e2" & lf$newBlock == 1,] 
m_e2_b1_co_phq <- glmer(corr ~ phq * word * match + (1|partId), 
                    family = binomial, data = temp, 
                    control= glmerControl(optimizer = "bobyqa")) 

# # # # RT PHQ E2 # # # #
temp <- lf[lf$exp=="e2",]
m_e2_rt_phq <- lmer(rt ~ phq * word * newBlock * match + (newBlock|partId), 
                REML = F, data = temp, 
                control= lmerControl(optimizer = "bobyqa")) 
summary(m_e2_rt_phq)

# # # PHQ RT and block 1 E2 (if main effect of block) # # #
temp <- lf[lf$exp=="e2" & lf$newBlock == 1,] 
m_e2_b1_rt_phq <- lmer(rt ~ phq * word * match + (1|partId), 
                   REML = F, data = temp, 
                   control= lmerControl(optimizer = "bobyqa")) 
summary(m_e2_b1_rt_phq)

# # # # STEPWISE REGRESSION FOR PHQ AND BDDQ ON RT # # # #
m_e2_rt_phq_step <- lmer(rt ~ (word + bddq + phq)^3 + (word|partId), 
                    REML = F, data = temp, 
                    control= lmerControl(optimizer = "bobyqa")) 
summary(m_e2_rt_phq_step) 
report(m_e2_rt_phq_step) 
step(m_e2_rt_phq_step) 



# # # # Accuracy PHQ E2 # # # #
temp <- lf[lf$exp=="e2",]
m_e2_co_phq <- glmer(corr ~ phq * word * newBlock * match + (newBlock|partId), 
                     family = binomial, data = temp, 
                     control= glmerControl(optimizer = "bobyqa")) 
summary(m_e2_co_phq)


# # # PHQ Accuracy and block 1 E2 (if main effect of block) # # #
temp <- lf[lf$exp=="e2" & lf$newBlock == 1,] 
m_e2_b1_co_phq <- glmer(corr ~ phq * word * match + (1|partId), 
                        family = binomial, data = temp, 
                        control= glmerControl(optimizer = "bobyqa")) 
summary(m_e2_b1_co_phq)



# # # # # # # # # # # # # #
# # # # # SEX E2 # # # # #
# # # # # # # # # # # # # #

# # # # Accuracy Sex E2 # # # #
temp <- lf[lf$exp=="e2",]
m_e2_co_sex <- glmer(corr ~ sex * word * newBlock * match + (newBlock|partId), 
                     family = binomial, data = temp, 
                     control= glmerControl(optimizer = "bobyqa")) 
summary(m_e2_co_sex)

# # # Sex Accuracy and block 1 E2 (if main effect of block) # # #
temp <- lf[lf$exp=="e2" & lf$newBlock == 1,] 
m_e2_b1_co_sex <- glmer(corr ~ sex * word * match + (1|partId), 
                        family = binomial, data = temp, 
                        control= glmerControl(optimizer = "bobyqa")) 

# # # # RT Sex E2 # # # #
temp <- lf[lf$exp=="e2",]
m_e2_rt_sex <- lmer(rt ~ sex * word * newBlock * match + (newBlock|partId), 
                    REML = F, data = temp, 
                    control= lmerControl(optimizer = "bobyqa")) 
summary(m_e2_rt_sex)

# # # Sex RT and block 1 E2 (if main effect of block) # # #
temp <- lf[lf$exp=="e2" & lf$newBlock == 1,] 
m_e2_b1_rt_sex <- lmer(rt ~ sex * word * match + (1|partId), 
                       REML = F, data = temp, 
                       control= lmerControl(optimizer = "bobyqa")) 
summary(m_e2_b1_rt_sex)



# # # # # # # # # # # # # #
# # # # # BIS E2 # # # # #
# # # # # # # # # # # # # #

# # # # Accuracy Bis E2 # # # #
temp <- lf[lf$exp=="e2",]
m_e2_co_bis <- glmer(corr ~ bis * word * newBlock * match + (newBlock|partId), 
                     family = binomial, data = temp, 
                     control= glmerControl(optimizer = "bobyqa")) 
summary(m_e2_co_bis)

# # # BIS Accuracy and block 1 E2 (if main effect of block) # # #
temp <- lf[lf$exp=="e2" & lf$newBlock == 1,] 
m_e2_b1_co_bis <- glmer(corr ~ bis * word * match + (1|partId), 
                        family = binomial, data = temp, 
                        control= glmerControl(optimizer = "bobyqa")) 

# # # # RT BIS E2 # # # #
temp <- lf[lf$exp=="e2",]
m_e2_rt_bis <- lmer(rt ~ bis * word * newBlock * match + (newBlock|partId), 
                    REML = F, data = temp, 
                    control= lmerControl(optimizer = "bobyqa")) 
summary(m_e2_rt_bis)

# # # BIS RT and block 1 E2 (if main effect of block) # # #
temp <- lf[lf$exp=="e2" & lf$newBlock == 1,] 
m_e2_b1_rt_bis <- lmer(rt ~ bis * word * match + (1|partId), 
                       REML = F, data = temp, 
                       control= lmerControl(optimizer = "bobyqa")) 
summary(m_e2_b1_rt_bis)



# # # ## # # ## # # ## # # ## # # ## # # ## # # ##
# # # # DURATION SPENT ON PAIRINGS SCREEN## # # ##
# # # ## # # ## # # ## # # ## # # ## # # ## # # ## 
tmp <- wf[wf$exp == "e1",] 
report::report_table(cor.test(tmp$pairing_duration, tmp$bddq))

tmp <- wf[wf$exp == "e2",]
report::report_table(cor.test(tmp$pairing_duration, tmp$bddq))



# # # ## # # ## # # ## # # ## # # # # # # ## # # ##
# # # # TYPES OF YOU-MISMATCH # # # # # # ## # # ## 
# # # ## # # ## # # ## # # ## # # # # # # ## # # ## 

# different types of mismatches
for (i in 1:length(unique(lf$partId))) {
  tmp <- lf[lf$partId==unique(lf$partId)[i],]
  tmp2 <- tmp[tmp$match == "Match ",]
  tmp2 <- tmp2[tmp2$word != "You",]
  friend_shape <- tmp2$shape[tmp2$word == "Friend"][1]
  stranger_shape <- tmp2$shape[tmp2$word == "Stranger"][1]

  tmp3 <- tmp[tmp$match == "Mismatch ",]
  tmp4 <- tmp3[tmp3$word != "You",]
  tmp3 <- tmp3[tmp3$word == "You",]

  tmp3$mismatchtype <- ifelse(tmp3$shape==friend_shape,"friend",
                              ifelse(tmp3$shape==stranger_shape,"stranger",NA))
  if (i == 1) {
    lf2 <- tmp3
  } else {
    lf2 <- rbind(tmp3,lf2)
  }
}

# Reaction Time (RT)
summary(lmer(rt ~ mismatchtype + (1|exp/partId), REML = F, lf2))
summary(lmer(rt ~ mismatchtype + (1|partId), REML = F, lf2[lf2$exp=="e1",]))
summary(lmer(rt ~ mismatchtype + (1|partId), REML = F, lf2[lf2$exp=="e2",]))
(figS1a <- ggplot2::ggplot(lf2, aes(x=mismatchtype,y=rt)) +
  labs(title = "You trials mismatches", y="Reaction Time (ms)",
       x="Type of Missmatch for You") + #geom_boxplot() +
  stat_summary() + facet_grid(. ~ exp) +
  theme_classic() + theme(axis.text.x = element_text(angle=30,hjust=1)))

# Accuracy (correct)
summary(glmer(corr ~ mismatchtype + (1|exp/partId), family = binomial, lf2))
summary(glmer(corr ~ mismatchtype + (1|partId), family = binomial, lf2[lf2$exp=="e1",]))
summary(glmer(corr ~ mismatchtype + (1|partId), family = binomial, lf2[lf2$exp=="e2",]))
(figS1b <- ggplot2::ggplot(lf2, aes(x=mismatchtype,y=corr*100)) +
  labs(title = "You trials mismatches", y="Accuract (%)",
       x="Type of Missmatch for You") + #geom_boxplot() +
  stat_summary() + facet_grid(. ~ exp) +
  theme_classic() + theme(axis.text.x = element_text(angle=30,hjust=1)))

summary(glmer(corr ~ mismatchtype*bddq + (1|exp/partId), family = binomial, lf2))
summary(glmer(corr ~ mismatchtype*bddq + (1|partId), family = binomial, lf2[lf2$exp=="e1",]))
summary(glmer(corr ~ mismatchtype*bddq + (1|partId), family = binomial, lf2[lf2$exp=="e2",]))
(fig <- ggplot2::ggplot(lf2, aes(x=bddq,y=corr*100,col=mismatchtype)) +
    labs(title = "You trials mismatches", y="Accuract (%)",
         x="BDDQ", col="Type of\n Missmatch\n for You") + #geom_boxplot() +
    geom_smooth(method="lm", se=F) +
    stat_summary() + facet_grid(. ~ exp) +
    theme_classic() + theme(axis.text.x = element_text(angle=30,hjust=1)))

ggarrange(figS1a, figS1b, nrow=2)
