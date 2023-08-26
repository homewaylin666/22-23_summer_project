#各種補充圖
# 1. 頻率的累積圖，用來表示只有一點點數據的物種大概有多少
# my_data來自 module 4.9
hist_data <- hist(log10(my_data$Freq), plot = FALSE)
cum_freq <- cumsum(hist_data$counts) / sum(hist_data$counts)
plot(hist_data$breaks[-1], cum_freq, type = "l", col = "blue",
     xlab = "Values", ylab = "Cumulative Frequency of Data Frequency",
     main = "Log(Data Frequency)")
# 這個結果說明有大約一半的物種的數據頻率小於等於10。

# 2. 