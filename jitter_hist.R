#### Nice histogram code


# to make NA's in excercise time = 0
P1Data$Vig.ex.Time[which(is.na(P1Data$Vig.ex.Time)==TRUE)] <- 0
P1Data$Mod.ex.time[which(is.na(P1Data$Mod.ex.time)==TRUE)] <- 0
P1Data$Walk.ex.Time[which(is.na(P1Data$Walk.ex.Time)==TRUE)] <- 0

# if T_M_M is still `NA`, fix it
P1Data$Total_Met_Min <- 8*P1Data$Vig.ex.Time + 4*P1Data$Mod.ex.time + 3.3*P1Data$Walk.ex.Time

# If weightgain = "No", lbs_gained should be 0
P1Data$lbs_gained[which(P1Data$weightgain=="No")] <- 0

# Identify complete cases
P1Data$complete <- as.numeric(complete.cases(P1Data))
P1Data$complete <- as.factor(P1Data$complete)



# full histogram of complete cases
hist(P1Data_comp$lbs_gained, breaks = c(0, seq(1,71,3)), freq = FALSE);
lines(density(P1Data_comp$lbs_gained[which(P1Data_comp$weightgain=="Yes")]))

hist(log(P1Data_comp$lbs_gained), freq = FALSE);
lines(density(log(P1Data_comp$lbs_gained[which(P1Data_comp$weightgain=="Yes")])))



# to jitter non-zero values of lbs_gained
lbs_jitter <- vector(length = nrow(P1Data_comp))
set.seed(600)
for (i in 1:length(lbs_jitter)) {
  if (P1Data_comp$lbs_gained[i] == 0) {
    lbs_jitter[i] <- P1Data_comp$lbs_gained[i]
  }
  else {
    lbs_jitter[i] <- P1Data_comp$lbs_gained[i] + runif(1,-2.5,2.5)
  }
}
lbs_jitter


# jittered histogram just for those who gained weight
hist(lbs_jitter[which(P1Data_comp$weightgain=="Yes")], breaks = seq(0,72,3), freq = FALSE);
lines(density(lbs_jitter[which(P1Data_comp$weightgain=="Yes")]))


