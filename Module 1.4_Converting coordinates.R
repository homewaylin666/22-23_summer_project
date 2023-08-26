### Module 1.4 Converting coordinates

# 示例输入数据
latitude_degrees <- 17  # 纬度度数
latitude_minutes <- 10  # 纬度分钟
latitude_seconds <- 0  # 纬度秒数

longitude_degrees <- 62  # 经度度数
longitude_minutes <- 35  # 经度分钟
longitude_seconds <- 0  # 经度秒数

# 转换为十进制格式
latitude_decimal <- latitude_degrees + latitude_minutes/60 + latitude_seconds/3600
longitude_decimal <- longitude_degrees + longitude_minutes/60 + longitude_seconds/3600

# 打印结果
print(paste("Latitude (Decimal):", latitude_decimal))
print(paste("Longitude (Decimal):", longitude_decimal))
