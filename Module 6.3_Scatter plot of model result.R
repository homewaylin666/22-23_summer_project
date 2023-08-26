### Module 6.3 Scatter plot of model result

library(ggplot2)
library(ggpmisc)
# For plotting, you should go back to module 6 and prepare a result without scale() the data.
# And then get a new Avg.lambda from Module 6.2.
slope <-as.numeric(round(Avg.lambda[2,2],8))
intercept <-as.numeric(round(Avg.lambda[1,2],8))
Std <-  as.numeric(round(Avg.lambda[2,3],8))

data$ED.median <- log(data$ED.median)

ggplot(data, aes(ED.median, Avg.lambda)) +
  geom_point(alpha = 0.5, size = 1) +
  theme_minimal()+
  ylim(-max(data$Avg.lambda), max(data$Avg.lambda)) +
  xlim(min(data$ED.median), max(data$ED.median)) +
  geom_ribbon(aes(ymin=intercept+ED.median*-1.96*Std, 
                  ymax=intercept+ED.median*1.96*Std), fill = "gray", alpha = 0.6)+
  geom_segment(aes(x = min(ED.median), xend = max(ED.median), 
               y = intercept+slope*min(ED.median), yend = intercept+slope*max(ED.median)), 
               color = "#ad2e28", size = 1) +
  ggtitle("The relation between relative abundance trend and ED")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("ln(ED)")+
  ylab("Average lambda")

# Export pdf as 6*8
# I'll add the legend manually.
