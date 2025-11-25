# in this script we are going to store all our functions
addQuestionnairesToLF <- function(lf,wf) {
  subjs <- wf$partId
  quests <- c("bddq","phq","bis","gad","sex","age")
  lf$bddq <- lf$phq <- lf$bis <- lf$sex <- lf$age <- NA
  for (i in 1:length(subjs)) {
    for (j in 1:length(quests)) {
      lf[lf$partId == subjs[i],quests[j]] <- wf[i,quests[j]] 
    } 
    # lf$bddq[lf$partId == subjs[i]] <- wf$bddq[i] 
    # lf$phq[lf$partId == subjs[i]] <- wf$phq[i]
    # lf$bis[lf$partId == subjs[i]] <- wf$bis[i]
    # lf$sex[lf$partId == subjs[i]] <- wf$sex[i]
    # lf$age[lf$partId == subjs[i]] <- wf$age[i]
  }
  return(lf)
}


# categorical descriptive variable
f_descrCategorical <- function(vec) {return(matrix(c(levels(as.factor(vec)),table(vec),(table(vec) / length(vec)) * 100),
                                                   nrow=length(levels(as.factor(vec)))))}
# continuous descriptive variable
f_descrContinuous <- function(vec) {return(c(sum(!is.na(vec)),
                                             mean(vec, na.rm = T),
                                             sd(vec, na.rm = T),
                                             range(vec, na.rm = T)))}
