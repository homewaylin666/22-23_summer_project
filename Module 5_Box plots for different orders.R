### Module 5: Comparison of box plots for different orders

# Put orders' name to compare in
ord <- c("Rodentia","Artiodactyla","Carnivora","Primates","Chiroptera","Diprotodontia"
         ,"Eulipotyphla","Didelphimorphia","Dasyuromorphia","Perissodactyla"
         ,"Lagomorpha","Proboscidea","Peramelemorphia","Afrosoricida","Pilosa","Pholidota","Cingulata","Scandentia") #,"Peramelemorphia","Afrosoricida","Pilosa","Pholidota","Cingulata","Scandentia"
# Select them out
data <- my_data[my_data$Order %in% ord, ]

# Sort them by orders' mean
library(dplyr)
grouped_mean <- data %>%
  group_by(Order) %>%
  summarize(mean_value = mean(Avg.lambda, na.rm = TRUE))%>%
  arrange(mean_value)
data$Order <- factor(data$Order, levels = grouped_mean$Order)

# Create a color gradient by group mean, over 0 should be green, under 0 should be brown.
color1 <- colorRampPalette(c("#990000", "#FFF5EB"))(14)
color2 <- colorRampPalette(c("#DCF0DC", "#4C9900"))(4)
colors <- c(color1, color2)
# Plotting
ggplot(data, aes(x = Order, y = Avg.lambda, fill = Order)) +
  geom_boxplot(outlier.size = 0.5, show.legend = FALSE) +
  geom_jitter(width = 0.2, size = 0.5, show.legend = FALSE)+
  geom_hline(yintercept = mean(my_data$Avg.lambda), color = "#606060", linetype = "dashed") +
  geom_text(aes(x = 17.7, y = mean(my_data$Avg.lambda)-0.3, label = "Mean of All speices"),  # 设置标签位置和内容
            color = "#606060", size = 3, hjust = 0.5, vjust = -8) +
  labs(y = "Average lambda", 
       title = "Comparison of average lambda for different orders") +
  scale_fill_manual(values = colors)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, size = 25),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 20, hjust = 0.5))
#Then, just take a screen shoot from Plot Zoom


# BTW, let's compare them using ANOVA, only use orders have species >5
ord <- c("Rodentia","Artiodactyla","Carnivora","Primates","Chiroptera","Diprotodontia"
         ,"Eulipotyphla","Didelphimorphia","Dasyuromorphia","Perissodactyla"
         ,"Lagomorpha","Peramelemorphia") 
# Select them out
data <- my_data[my_data$Order %in% ord, ]
AL.OD <- aov(Avg.lambda~Order, data = data)
summary(AL.OD) # type it manually in the table of AL.OD.T
AL.OD.T <- TukeyHSD(AL.OD)
AL.OD.T$Order
write.csv(AL.OD.T$Order, "Sup_Tukey_Avg.lambda~Order.csv")

