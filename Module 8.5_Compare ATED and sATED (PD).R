### Module 8.5: Compare ATED and sATED (by group)

compare_list <- c("Rhinocerotidae","Otariidae","Hominidae",
                 "Panthera","Oryx","Lynx","Ursus","Capra","Arctocephalus","Kobus",
                 "Elephantidae","Myrmecophagidae","Redunca","Isoodon","Procapra")

# Fill the blank of missing species in some groups manually
sp_blank <- data.frame(Binomial = c("Arctocephalus_galapagoensis","Capra_aegagrus","Capra_cylindricornis","Kobus_megaceros"),
                       Family = c("Otariidae","Bovidae","Bovidae","Bovidae"),
                       Avg.lambda = 0)
sp_blank_na <- data.frame(matrix(nrow = length(sp_blank$Binomial), ncol = length(my_data)))
colnames(sp_blank_na) <- colnames(my_data)
sp_blank_na <- sp_blank_na[,!colnames(sp_blank_na) %in% c("Binomial","Family","Avg.lambda")]
sp_blank <- cbind(sp_blank, sp_blank_na)
my_data2 <- rbind(my_data, sp_blank)
my_data2 <- separate(my_data2, col = "Binomial", into = c("Genus", "Species"), sep = "_", remove = FALSE)

# Create a loop to compare
compare_result <- data.frame(ATPD = numeric(length(compare_list)), 
                             sATPD = numeric(length(compare_list)))
for (i in 1:length(compare_list)){
  # Select the target group out
  group <- compare_list[i]
  data <- my_data2
  data <- data[data$Genus == group | data$Family == group, c("Binomial","Avg.lambda")]
  # Create a trend data for the group to suit the function ATED 
  trend <- data.frame(lambda = 10^data$Avg.lambda)
  rownames(trend) <- data$Binomial
  trend <- t(trend)
  # Use function in Module 8.4 to Calculate ATPD and sATPD by 50 diff trees
  compare_result[i,1] <- sum(ATED50(trend = trend)[,1])
  compare_result[i,2] <- sum(ATED50(trend = trend)[,2])
  # Name the row by the group's name
  rownames(compare_result)[i] <- group
}

# Check their relation
lm<-summary(lm(data = compare_result, ATPD~sATPD))
write.csv(lm$coefficients,file = "ATPD~sATPD lm result.csv")

# Plot the compare result
library(ggplot2)
ggplot(data = compare_result, aes(x = sATPD, y = ATPD)) +
  geom_point(color = "blue", size = 3, alpha = 0.6) + 
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed", size = 1) +  # add fitting line
  labs(x = "sATPD", y = "ATPD", title = "Comparison of sATPD and ATPD") +  # set line and title
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

# out put as 6*8

