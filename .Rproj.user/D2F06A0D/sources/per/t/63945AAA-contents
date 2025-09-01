#Create auto_info.csv

set.seed(123)  
n <- 200  

vehicle_types <- c("Sedan", "SUV", "Hatchback", "Truck", "Coupe", "Convertible")
brands <- c("Toyota", "Ford", "BMW", "Honda", "Chevrolet", "Mercedes", "Hyundai", "Kia")

auto_info <- data.frame(
  engine_size = round(runif(n, 1.0, 4.5), 1),
  horsepower = round(runif(n, 70, 300), 0),
  curb_weight = round(runif(n, 2000, 5000), 0),
  fuel_efficiency = round(runif(n, 10, 50), 1),
  age = sample(0:14, n, replace = TRUE),
  vehicle_type = sample(vehicle_types, n, replace = TRUE),
  brand = sample(brands, n, replace = TRUE)
)

auto_info$price <- round(
  5000 +
    auto_info$engine_size * 5000 +
    auto_info$horsepower * 50 -
    auto_info$age * 1000 +
    rnorm(n, 0, 3000), 2
)

auto_info$price <- pmax(pmin(auto_info$price, 100000), 5000)

write.csv(auto_info, "auto_info.csv", row.names = FALSE)



---------------------------------------------------------------------------





data <- read.csv("auto_info.csv")
str(data)  # Check structure
summary(data)  # Summary statistics
head(data)  # View first few rows

colSums(is.na(data))  # Count missing values per column

boxplot(data$price, main="Price Boxplot")  # Repeat for other numeric columns

# Calculate statistics
stats <- data %>%
  summarise(
    Engine_Size_Mean = mean(engine_size, na.rm = TRUE),
    Engine_Size_Median = median(engine_size, na.rm = TRUE),
    Engine_Size_Mode = as.numeric(names(sort(table(engine_size), decreasing=TRUE)[1])),
    Engine_Size_SD = sd(engine_size, na.rm = TRUE),
    Horsepower_Mean = mean(horsepower, na.rm = TRUE),
    Horsepower_Median = median(horsepower, na.rm = TRUE),
    Horsepower_Mode = as.numeric(names(sort(table(horsepower), decreasing=TRUE)[1])),
    Horsepower_SD = sd(horsepower, na.rm = TRUE),
    Curb_Weight_Mean = mean(curb_weight, na.rm = TRUE),
    Curb_Weight_Median = median(curb_weight, na.rm = TRUE),
    Curb_Weight_Mode = as.numeric(names(sort(table(curb_weight), decreasing=TRUE)[1])),
    Curb_Weight_SD = sd(curb_weight, na.rm = TRUE),
    Price_Mean = mean(price, na.rm = TRUE),
    Price_Median = median(price, na.rm = TRUE),
    Price_Mode = as.numeric(names(sort(table(price), decreasing=TRUE)[1])),
    Price_SD = sd(price, na.rm = TRUE)
  )
print(stats)

#plots
ggplot(data, aes(x = engine_size)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "blue", alpha = 0.5) +
  geom_density(color = "red", size = 1) +
  labs(title = "Distribution of Engine Size", x = "Engine Size (L)", y = "Density") +
  theme_minimal()
ggsave("engine_size_bell.png")

# Bell curve for horsepower
ggplot(data, aes(x = horsepower)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "blue", alpha = 0.5) +
  geom_density(color = "red", size = 1) +
  labs(title = "Distribution of Horsepower", x = "Horsepower (hp)", y = "Density") +
  theme_minimal()
ggsave("horsepower_bell.png")

# Bell curve for curb_weight
ggplot(data, aes(x = curb_weight)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "blue", alpha = 0.5) +
  geom_density(color = "red", size = 1) +
  labs(title = "Distribution of Curb Weight", x = "Curb Weight (lbs)", y = "Density") +
  theme_minimal()
ggsave("curb_weight_bell.png")

# Bell curve for price
ggplot(data, aes(x = price)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "blue", alpha = 0.5) +
  geom_density(color = "red", size = 1) +
  labs(title = "Distribution of Price", x = "Price (USD)", y = "Density") +
  theme_minimal()
ggsave("price_bell.png")

anova_result <- aov(price ~ vehicle_type, data = data)
summary(anova_result)

ggplot(data, aes(x = vehicle_type, y = price, fill = vehicle_type)) +
  geom_boxplot() +
  labs(title = "Price Distribution by Vehicle Type", x = "Vehicle Type", y = "Price (USD)") +
  theme_minimal()
ggsave("price_vehicle_type_boxplot.png")

shapiro.test(data$price)
shapiro.test(data$engine_size)
shapiro.test(data$horsepower)
shapiro.test(data$curb_weight)
shapiro.test(data$age)

# Task 5: Correlation tests
cor.test(data$price, data$engine_size, method = "pearson")
cor.test(data$price, data$horsepower, method = "pearson")
cor.test(data$price, data$curb_weight, method = "pearson")
cor.test(data$price, data$age, method = "pearson")

# Task 5: Regression analysis
model <- lm(price ~ engine_size + horsepower + curb_weight + age, data = data)
summary(model)

# Task 5: Scatter plots
library(ggplot2)

ggplot(data, aes(x = engine_size, y = price)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Price vs. Engine Size", x = "Engine Size (L)", y = "Price (USD)") +
  theme_minimal()
ggsave("price_engine_size_scatter.png")

ggplot(data, aes(x = horsepower, y = price)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Price vs. Horsepower", x = "Horsepower (hp)", y = "Price (USD)") +
  theme_minimal()
ggsave("price_horsepower_scatter.png")

ggplot(data, aes(x = curb_weight, y = price)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Price vs. Curb Weight", x = "Curb Weight (lbs)", y = "Price (USD)") +
  theme_minimal()
ggsave("price_curb_weight_scatter.png")

ggplot(data, aes(x = age, y = price)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Price vs. Age", x = "Age (years)", y = "Price (USD)") +
  theme_minimal()
ggsave("price_age_scatter.png")


nrow(data)