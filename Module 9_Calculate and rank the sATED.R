#### Module 9: Calculate and rank the sATED for all species with LPI data

# Using data I want to use to create a new df
sATED_rank <- my_data[, c("Binomial","Order","Family","RL.cat","EDGE.median","ED.median","Avg.lambda","Freq"), drop = FALSE]
sATED_rank$sATED <- NA

# Create a loop to calculate sATED using all mammal tree's ED
for (i in 1:length(sATED_rank$Binomial)) {
  I <- 10^sATED_rank$Avg.lambda[i]
  ED <- sATED_rank$ED.median[i]
  sATED_rank$sATED[i] <- (ED*(1-I))/I
}

# Rank the df by sATED
sATED_rank <- sATED_rank[order(-sATED_rank$sATED), ]
head(sATED_rank)

# It seems  low Freq species are easier to get extreme value, let's check
plot(log10(sATED_rank$Freq),sATED_rank$sATED) #indeed, but it's not my focus for now.


##### I want to compare the rank between sATED and EDGE
sATED_rank$aRank <- rank(-sATED_rank$sATED)
sATED_rank$eRank <- rank(-sATED_rank$EDGE.median)

# then plot them
plot(x=sATED_rank$eRank,y=sATED_rank$aRank,
     xlab = "EDGE Scores Ranking", ylab = "sATED Scores Ranking") #OMG

# they don't have any relation, do they?
cor(sATED_rank$sATED,sATED_rank$EDGE.median, method = "spearman") #0.03379474
cor(sATED_rank$aRank,sATED_rank$eRank, method ="kendall") #0.02015627


# OMG, I guess it's because there is no correlation between RLC and Avg.lambda
AL.RLCa <- aov(Avg.lambda~RL.cat, data = my_data)
summary(AL.RLCa) # type it manually in the table of AL.RLCa.T
AL.RLCa.T <- TukeyHSD(AL.RLCa) # paired comparison.
plot(AL.RLCa.T)
write.csv(AL.RLCa.T$RL.cat,"Sup_Tukey_Avg.lambda~RLC.csv")



