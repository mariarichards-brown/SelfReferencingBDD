# remove everything
rm(list=ls())
# call our functions
source("functions_v3.R")

if (!require(ggplot2)) {install.packages("ggplot2")}; library(ggplot2)
if (!require(lmerTest)) {install.packages("lmerTest")}; library(lmerTest)
if (!require(effectsize)) {install.packages("effectsize")}; library(effectsize)
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
lf <- addQuestionnairesToLF(lf,wf)

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
m <- glmer(corr ~ word * match + (1|partId), family = binomial, temp)
summary(m)
# effect size
tm <- report_table(m)


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



# # # # reaction time E1 # # # #
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
m <- lmer(rt ~ word * match+ (1|partId), REML = F, temp)
# anova
summary(m)
# effect size
report_table(m)


## # # # # ####  ## # # # # #### ## # # # # #### ## # # # # #### 
## # # # # ####  ## # # # # #### ## # # # # #### ## # # # # #### 
## # # # # #### PLEASE CHECK FROM HERe ## # # # # #### AND REMOVE THIS TEXT #####
## # # # # ####  ## # # # # #### ## # # # # #### ## # # # # #### 
## # # # # ####  ## # # # # #### ## # # # # #### ## # # # # #### 



# # # # # Experiment 2 # # # # # # # # # # # # # # # # # # # # # # # # # # #### 

# data for experiment 2
temp <- lf[lf$exp=="e2",]

# # # # accuracy E2 # # # #
ggplot(temp, aes(x = word, y = corr, shape = factor(match), col = factor(match))) +  
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
  coord_cartesian(ylim = c(0.75, 1))  # Set y-axis limit  

# # # Main Self-Prioritisation effect E2 # # #
# model
m <- glmer(corr ~ word * match + (1|partId), family = binomial, temp)
summary(m)
# effect size
report_table(m)



# # # # reaction time E2 # # # #
ggplot(temp, aes(x = word, y = rt, shape = factor(match), col = factor(match))) +  
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
  coord_cartesian(ylim = c(725, 875))  # Set y-axis limit  

# # # Main Self-Prioritisation effect E2 # # #
# model
m <- lmer(rt ~ word * match+ (1|partId), REML = F, temp)
# anova
summary(m)
# effect size
report_table(m)












# graphs for experiment 3
temp <- lf[lf$exp=="e3",]

# reaction time E3
ggplot(temp, aes(x = word, y = rt, shape = factor(match), col = factor(match))) +  
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
  coord_cartesian(ylim = c(725, 875))  # Set y-axis limit  

# accuracy E3
ggplot(temp, aes(x = word, y = corr, shape = factor(match), col = factor(match))) +  
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
  coord_cartesian(ylim = c(0.75, 1))  # Set y-axis limit  







# # # # Analysis Figure 4 # # #


# effect size

# Print ANOVA summary
summary(anova_result)

# Ensure factors
temp$word <- as.factor(temp$word)
temp$match <- as.factor(temp$match)

# Run two-way ANOVA
anova_result <- aov(rt ~ word * match, data = temp)

library(report)
report(anova_result)

# Calculate means and standard deviations for each condition
library(dplyr)

# Group by word type and match, then summarize to get mean and SD
summary_stats <- temp %>%
  group_by(word) %>%
  summarise(
    mean_rt = sprintf("%.2f", mean(rt, na.rm = TRUE)),  # Force rounding to 2 decimal places
    sd_rt = sprintf("%.2f", sd(rt, na.rm = TRUE)),      # Force rounding to 2 decimal places
    .groups = 'drop'                    # Drop grouping after summarizing
  )

# Print the summary statistics
print(summary_stats)


# Calculate means and standard deviations for each pairing (combining match and mismatch)
pairing_stats <- temp %>%
  group_by(word) %>%
  summarise(
    mean_corr = sprintf("%.3f", mean(corr, na.rm = TRUE)),  # Force rounding to 2 decimal places
    sd_corr = sprintf("%.3f", sd(corr, na.rm = TRUE)),      # Force rounding to 2 decimal places
    .groups = 'drop'                                    # Drop grouping after summarizing
  )

# Print the summary statistics for pairings
print(pairing_stats)



posthoc_results <- TukeyHSD(anova_result, "word:match")
print(posthoc_results)

# Extract Q-values from Tukey's HSD output
q_values <- posthoc_results$`word:match`[, "diff"] / posthoc_results$`word:match`[, "lwr"]
q_values

# analysis for Experiment 3 self-referencing
temp <- lf[lf$exp=="e2",]

# Ensure factors
temp$word <- as.factor(temp$word)
temp$match <- as.factor(temp$match)

# Run two-way ANOVA
anova_result <- aov(corr ~ word * match, data = temp)

# Print ANOVA summary
summary(anova_result)
library(report)
report(anova_result)

# Calculate means and standard deviations for each condition
library(dplyr)

# Group by word type and match, then summarize to get mean and SD
summary_stats <- temp %>%
  group_by(word) %>%
  summarise(
    mean_corr = sprintf("%.3f", mean(corr, na.rm = TRUE)),  # Force rounding to 2 decimal places
    sd_corr = sprintf("%.3f", sd(corr, na.rm = TRUE)),      # Force rounding to 2 decimal places
    .groups = 'drop'                    # Drop grouping after summarizing
  )

# Print the summary statistics
print(summary_stats)


# Calculate means and standard deviations for each pairing (combining match and mismatch)
pairing_stats <- temp %>%
  group_by(word) %>%
  summarise(
    mean_corr = sprintf("%.3f", mean(corr, na.rm = TRUE)),  # Force rounding to 2 decimal places
    sd_corr = sprintf("%.3f", sd(corr, na.rm = TRUE)),      # Force rounding to 2 decimal places
    .groups = 'drop'                                    # Drop grouping after summarizing
  )

# Print the summary statistics for pairings
print(pairing_stats)

posthoc_results <- TukeyHSD(anova_result, "word:match")
print(posthoc_results)

# Extract Q-values from Tukey's HSD output
q_values <- posthoc_results$`word:match`[, "diff"] / posthoc_results$`word:match`[, "lwr"]
q_values


# # ## # ## # ## # ## # #
# #   BDD Analysis  # # #
# # ## # ## # ## # ## # #

if (!require(lmerTest)) {install.packages("lmerTest")}; library(lmerTest)

# Get Experiment 1 data
temp <- lf[lf$exp=="e1"]

# Ensure newBlock is an integer
temp$newBlock <- as.integer(temp$newBlock)

# Model for Accuracy
m_e1_co <- glmer(corr ~ bddq * word * newBlock * match + (newBlock|partId),
                 family = binomial, data = temp,
                 control= glmerControl(optimizer = "bobyqa"))
summary(m_e1_co)
step(m_e1_co)
library(report)
report(m_e1_co)

# Create the graph for accuracy across blocks
ggplot(temp, aes(x = bddq, y = corr, linetype = word)) + 
  stat_smooth(method = "lm", se = TRUE, color = "black") +  # Black smooth lines with SE
  facet_grid(~newBlock) +  # facet by newBlock
  theme_classic() +
  labs(
    x = "BDDQ Score",                      # X-axis label
    y = "Accuracy",              # Y-axis label
    title = "Accuracy for Judging Object-Label Pairings by BDDQ by Block"  # Graph title
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
  )+
  coord_cartesian(ylim = c(0.6, 1))

ggplot(temp, aes(x = bddq, y = corr, linetype = word)) +
  stat_smooth(method = "lm", se = TRUE, color = "black") +  # Black smooth lines with SE
  facet_grid(~newBlock) +  # facet by newBlock
  theme_classic() +
  labs(
    x = "BDDQ Score",                      
    y = "Accuracy",              
    title = "Accuracy for Judging Object-Label Pairings by BDDQ by Block"
  ) +
  scale_linetype_manual(
    values = c("solid", "dashed", "dotted"),  # clearer, distinct linetypes
    guide = guide_legend(title = "Word Type")
  ) +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.title = element_text(),  
    legend.background = element_rect(fill = "white", color = "black"),  
    legend.box.background = element_rect(color = "black"),  
    strip.text.x = element_text(angle = 0),  
    strip.placement = "outside",
    strip.text.x.top = element_text(face = "bold")  
  ) +
  coord_cartesian(ylim = c(0.6, 1))

# Explore early learning
temp <- lf[lf$exp=="e1" & lf$newBlock == 1,]
m_e1_b1_co <- glmer(corr ~ bddq * word * match + (1|partId),
                    family = binomial, data = temp,
                    control= glmerControl(optimizer = "bobyqa"))
summary(m_e1_b1_co)

# Model for Reaction Time
m_e1_rt <- lmer(rt ~ bddq * word * newBlock * match + (newBlock|partId),
                REML = F, data = temp,
                control= lmerControl(optimizer = "bobyqa"))
summary(m_e1_rt)
step(m_e1_rt)

# Convert newBlock to a factor with custom labels
temp$newBlock <- factor(temp$newBlock, levels = c(1, 2, 3, 4, 5, 6), labels = c("Block 1", "Block 2", "Block 3", "Block 4", "Block 5", "Block 6"))

# Create the ggplot
ggplot(temp, aes(x = bddq, y = rt, linetype = word)) + 
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
  )

# DEPRESSION
m_e1_co_phq <- glmer(corr ~ phq * word * newBlock * match + (newBlock|partId),
                     family = binomial, data = temp,
                     control= glmerControl(optimizer = "bobyqa"))
summary(m_e1_co_phq)

# Get Experiment 3 data - termed Experiment 2 in the paper
temp <- lf[lf$exp=="e3"]

# Model for Accuracy
m_e3_co <- glmer(corr ~ bddq * word * newBlock * match + (newBlock|partId),
                 family = binomial, data = temp,
                 control= glmerControl(optimizer = "bobyqa"))
summary(m_e3_co)
step(m_e3_co)
library(report)
report(m_e3_co)

# Convert newBlock to a factor with custom labels
temp$newBlock <- factor(temp$newBlock, levels = c(1, 2, 3, 4), labels = c("Block 1", "Block 2", "Block 3", "Block 4"))

# Create the ggplot
ggplot(temp, aes(x = bddq, y = corr, linetype = word)) + 
  stat_smooth(method = "lm", se = TRUE, color = "black") +  # Black smooth lines with SE
  facet_grid(~newBlock) +  # facet by newBlock
  theme_classic() +
  labs(
    x = "BDDQ Score",                      # X-axis label
    y = "Accuracy",              # Y-axis label
    title = "Accuracy for Judging Object-Label Pairings by BDDQ by Block"  # Graph title
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
  )+
  coord_cartesian(ylim = c(0.8, 0.95))

# Explore early learning
temp <- lf[lf$exp=="e3" & lf$newBlock == 1,]
m_e3_b1_co <- glmer(corr ~ bddq * word * match + (1|partId),
                    family = binomial, data = temp,
                    control= glmerControl(optimizer = "bobyqa"))
summary(m_e3_b1_co)

# Model for Reaction Time
m_e3_rt <- lmer(rt ~ bddq * word * newBlock * match + (newBlock|partId),
                REML = F, data = temp,
                control= lmerControl(optimizer = "bobyqa"))
summary(m_e3_rt)
step(m_e3_rt)

# Create the ggplot
ggplot(temp, aes(x = bddq, y = rt, linetype = word)) + 
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
  )+
  coord_cartesian(ylim = c(700, 950))



# DEPRESSION - accuracy model
m_e3_co_phq <- glmer(corr ~ phq * word * newBlock * match + (newBlock|partId),
                     family = binomial, data = temp,
                     control= glmerControl(optimizer = "bobyqa"))
summary(m_e3_co_phq)

# DEPRESSION - reaction time model
m_e3_rt_phq <- lmer(rt ~ phq * word * newBlock * match + (newBlock|partId),
                REML = F, data = temp,
                control= lmerControl(optimizer = "bobyqa"))
summary(m_e3_rt_phq)

ggplot(temp, aes(x = phq, y = rt, linetype = word)) + 
  stat_smooth(method = "lm", se = TRUE, color = "black") +  # Black smooth lines with SE
  theme_classic() +
  labs(
    x = "PHQ Score",                      # X-axis label
    y = "Reaction Time (ms)",             # Y-axis label
    title = "Reaction Time as a function of PHQ and Word Type"  # Graph title
  ) +
  scale_linetype_manual(values = c("solid", "dashed", "twodash"),
                        guide = guide_legend(title = "Word Type"))  +  # Customize line types
  theme(
    text = element_text(family = "Times New Roman"),
    legend.title = element_text(),  
    legend.background = element_rect(fill = "white", color = "black"),  # Add box around legend
    legend.box.background = element_rect(color = "black"),  # Box around legend
    legend.position = c(0.2,0.2)  # Position the legend to the right
  ) +
  coord_cartesian(ylim = c(700, 900))

# look at stepwise regression to see if PHQ and BDDQ are significant predictors
m_e3_rt_phq <- lmer(rt ~ (word + bddq + phq)^3 + (word|partId),
                    REML = F, data = temp,
                    control= lmerControl(optimizer = "bobyqa"))
summary(m_e3_rt_phq)
report(m_e3_rt_phq)
step(m_e3_rt_phq)
# only BDDQ and word are left as predictors of RT

# IMPULSIVITY - reaction time model
m_e3_rt_bis <- lmer(rt ~ bis * word * newBlock * match + (newBlock|partId),
                    REML = F, data = temp,
                    control= lmerControl(optimizer = "bobyqa"))
summary(m_e3_rt_bis)
step(m_e3_rt_bis)

