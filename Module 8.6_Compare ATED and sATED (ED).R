### Module 8.6: Compare ATED and sATED (by species)

# Create a df with all needed species
# compare_list and my_data2 are from module 8.5
my_data3 <- my_data2[my_data2$Genus %in% compare_list | my_data2$Family %in% compare_list, c("Binomial","Genus","Family","Avg.lambda","RL.cat")]

# Create a loop to compare
compare_result <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(compare_result) <- c("ATED","sATED")
for (i in 1:length(compare_list)){ 
  # Select the target group out
  group <- compare_list[i]
  data <- my_data3
  data <- data[data$Genus == group | data$Family == group, c("Binomial","Avg.lambda")]
  # Create a trend data for the group to suit the function ATED 
  trend <- data.frame(lambda = 10^data$Avg.lambda)
  rownames(trend) <- data$Binomial
  trend <- t(trend)
  # Use function in Module 8.4 to Calculate ATPD and sATPD by 50 diff trees
  group_result <- data.frame(ATED = numeric(length(trend)), 
                             sATED = numeric(length(trend)))
  group_result$ATED <- ATED50(trend = trend)[,1]
  group_result$sATED <- ATED50(trend = trend)[,2]
  # Name the row by the group's name
  rownames(group_result) <- colnames(trend)
  compare_result <- rbind(compare_result,group_result)
}

# I should delete the species in sp_blank (from Module8.5)
compare_result <- compare_result[!(rownames(compare_result) %in% sp_blank$Binomial), ]

# Check their relation
lm<-summary(lm(data = compare_result, ATED~sATED))
write.csv(lm$coefficients,file = "ATED~sATED lm result.csv")
cor(compare_result$ATED, compare_result$sATED)

# Plot the compare result
library(ggplot2)
ggplot(data = compare_result, aes(x = sATED, y = ATED)) +
  geom_point(color = "blue", size = 3, alpha = 0.6) + 
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed", size = 1) +  # add fitting line
  labs(x = "sATED", y = "ATED", title = "Comparison of sATED and ATED") +  # set line and title
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))
