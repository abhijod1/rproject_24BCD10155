data<-read.csv(file.choose())
# Load necessary packages
# ggplot2 for data visualization
# dplyr for data manipulation and summarization
# reshape2 for reshaping data
library(ggplot2)  # Data visualization package
library(dplyr)    # Data manipulation package
library(reshape2) # Data reshaping package
# tidyverse 

#Finding mean in this project
# Create the data frame
df <- data.frame(
  Year = c(2020, 2020, 2021, 2021, 2022, 2022, 2023, 2023, 2024, 2024),
  Product = c("Laptops", "Monitors", "Laptops", "Keyboards", "Monitors", "Keyboards", "Laptops", "Monitors", "Keyboards", "Laptops"),
  Quantity = c(50, 70, 65, 90, 80, 100, 85, 75, 95, 100),
  Cost_per_Unit = c(800, 150, 820, 40, 155, 45, 830, 160, 50, 840),
  Total_Cost = c(40000, 10500, 53300, 3600, 12400, 4500, 70550, 12000, 4750, 84000)
)

# Find the mean of Quantity, Cost_per_Unit, and Total_Cost
mean_quantity <- mean(df$Quantity)
mean_cost_per_unit <- mean(df$Cost_per_Unit)
mean_total_cost <- mean(df$Total_Cost)

# Print results
mean_quantity
mean_cost_per_unit
mean_total_cost

# Median
median_quantity <- median(df$Quantity)
median_cost <- median(df$Cost_per_Unit)
median_total <- median(df$Total_Cost)

# Standard Deviation
sd_quantity <- sd(df$Quantity)
sd_cost <- sd(df$Cost_per_Unit)
sd_total <- sd(df$Total_Cost)


cat("MEDIAN:\n")
cat("Quantity:", median_quantity, "\n")
cat("Cost per Unit:", median_cost, "\n")
cat("Total Cost:", median_total, "\n\n")

cat("STANDARD DEVIATION:\n")
cat("Quantity:", sd_quantity, "\n")
cat("Cost per Unit:", sd_cost, "\n")
cat("Total Cost:", sd_total, "\n")


library(ggplot2)

# Data frame
df <- data.frame(
  Year = c(2020, 2020, 2021, 2021, 2022, 2022, 2023, 2023, 2024, 2024),
  Product = c("Laptops", "Monitors", "Laptops", "Keyboards", "Monitors", "Keyboards", "Laptops", "Monitors", "Keyboards", "Laptops"),
  Quantity = c(50, 70, 65, 90, 80, 100, 85, 75, 95, 100),
  Cost_per_Unit = c(800, 150, 820, 40, 155, 45, 830, 160, 50, 840),
  Total_Cost = c(40000, 10500, 53300, 3600, 12400, 4500, 70550, 12000, 4750, 84000)
)

# Bar plot of Quantity by Product
ggplot(df, aes(x = Product, y = Quantity, fill = Product)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  ggtitle("Quantity Purchased by Product")

# Histogram of Cost_per_Unit
ggplot(df, aes(x = Cost_per_Unit)) +
  geom_histogram(binwidth = 50, fill = "skyblue", color = "black") +
  theme_minimal() +
  ggtitle("Distribution of Cost per Unit")

# Box plot of Total_Cost by Year
ggplot(df, aes(x = as.factor(Year), y = Total_Cost, fill = as.factor(Year))) +
  geom_boxplot() +
  theme_minimal() +
  ggtitle("Total Cost Distribution by Year") +
  xlab("Year")


# --- Interpretation (in comments) ---

# INSIGHTS:
# 1. Quantity is typically between 70–100, with laptops and keyboards most frequently bought.
# 2. Cost per unit is right-skewed. Laptops are expensive, keyboards are low-cost.
# 3. Total Cost has high variance; large spikes in years with laptop purchases.
# 4. 2024 had the highest cost due to a big laptop order.
# 5. Consider bulk discounts, especially on laptops.
# 6. Regular purchases of keyboards and monitors suggest need for stock tracking and supplier negotiation.

# Load necessary library
library(ggplot2)

# Data
df <- data.frame(
  Year = c(2020, 2020, 2021, 2021, 2022, 2022, 2023, 2023, 2024, 2024),
  Product = c("Laptops", "Monitors", "Laptops", "Keyboards", "Monitors", "Keyboards", "Laptops", "Monitors", "Keyboards", "Laptops"),
  Quantity = c(50, 70, 65, 90, 80, 100, 85, 75, 95, 100),
  Cost_per_Unit = c(800, 150, 820, 40, 155, 45, 830, 160, 50, 840),
  Total_Cost = c(40000, 10500, 53300, 3600, 12400, 4500, 70550, 12000, 4750, 84000)
)

# Summary calculations
summary_table <- data.frame(
  Metric = c("Mean", "Median", "Standard Deviation"),
  Quantity = c(mean(df$Quantity), median(df$Quantity), sd(df$Quantity)),
  Cost_per_Unit = c(mean(df$Cost_per_Unit), median(df$Cost_per_Unit), sd(df$Cost_per_Unit)),
  Total_Cost = c(mean(df$Total_Cost), median(df$Total_Cost), sd(df$Total_Cost))
)

# Print table
print(summary_table)


# Load necessary library
library(ggplot2)
library(reshape2)

Create the data frame
df <- data.frame(
  Year = c(2020, 2020, 2021, 2021, 2022, 2022, 2023, 2023, 2024, 2024),
  Product = c("Laptops", "Monitors", "Laptops", "Keyboards", "Monitors", "Keyboards", "Laptops", "Monitors", "Keyboards", "Laptops"),
  Quantity = c(50, 70, 65, 90, 80, 100, 85, 75, 95, 100),
  Cost_per_Unit = c(800, 150, 820, 40, 155, 45, 830, 160, 50, 840),
  Total_Cost = c(40000, 10500, 53300, 3600, 12400, 4500, 70550, 12000, 4750, 84000)
)

# --- Descriptive Statistics ---
summary_stats <- data.frame(
  Metric = c("Mean", "Median", "Standard Deviation"),
  Quantity = c(mean(df$Quantity), median(df$Quantity), sd(df$Quantity)),
  Cost_per_Unit = c(mean(df$Cost_per_Unit), median(df$Cost_per_Unit), sd(df$Cost_per_Unit)),
  Total_Cost = c(mean(df$Total_Cost), median(df$Total_Cost), sd(df$Total_Cost))
)

# Reshape the data for plotting
summary_stats_melted <- melt(summary_stats, id.vars = "Metric")

# Plot Bar Graph of Mean, Median, and SD
ggplot(summary_stats_melted, aes(x = Metric, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Summary Statistics: Mean, Median, and SD",
       x = "Metric", y = "Value") +
  theme_minimal() +
  scale_fill_manual(values = c("skyblue", "lightgreen", "salmon")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot Histogram for Cost per Unit Distribution
ggplot(df, aes(x = Cost_per_Unit)) +
  geom_histogram(binwidth = 50, fill = "skyblue", color = "black") +
  theme_minimal() +
  ggtitle("Distribution of Cost per Unit") +
  xlab("Cost per Unit")

# Plot Box Plot of Total Cost by Year
ggplot(df, aes(x = as.factor(Year), y = Total_Cost, fill = as.factor(Year))) +
  geom_boxplot() +
  theme_minimal() +
  ggtitle("Total Cost Distribution by Year") +
  xlab("Year")

Identify highest and lowest values for Quantity
highest_qty <- df[which.max(df$Quantity), ]
lowest_qty <- df[which.min(df$Quantity), ]

# Identify highest and lowest values for Cost per Unit
highest_cost_unit <- df[which.max(df$Cost_per_Unit), ]
lowest_cost_unit <- df[which.min(df$Cost_per_Unit), ]

# Identify highest and lowest values for Total Cost
highest_total_cost <- df[which.max(df$Total_Cost), ]
lowest_total_cost <- df[which.min(df$Total_Cost), ]

# Print results
cat("Highest Quantity:\n")
print(highest_qty)

cat("\nLowest Quantity:\n")
print(lowest_qty)

cat("\nHighest Cost per Unit:\n")
print(highest_cost_unit)

cat("\nLowest Cost per Unit:\n")
print(lowest_cost_unit)

cat("\nHighest Total Cost:\n")
print(highest_total_cost)

cat("\nLowest Total Cost:\n")
print(lowest_total_cost)                 


# Load necessary library
library(ggplot2)

# Data
df <- data.frame(
  Year = c(2020, 2020, 2021, 2021, 2022, 2022, 2023, 2023, 2024, 2024),
  Product = c("Laptops", "Monitors", "Laptops", "Keyboards", "Monitors", "Keyboards", "Laptops", "Monitors", "Keyboards", "Laptops"),
  Quantity = c(50, 70, 65, 90, 80, 100, 85, 75, 95, 100),
  Cost_per_Unit = c(800, 150, 820, 40, 155, 45, 830, 160, 50, 840),
  Total_Cost = c(40000, 10500, 53300, 3600, 12400, 4500, 70550, 12000, 4750, 84000)
)

# --- 1. Performance Trend: Quantity by Year and Product ---
ggplot(df, aes(x = as.factor(Year), y = Quantity, group = Product, color = Product)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(title = "Performance Trend: Quantity by Year and Product",
       x = "Year", y = "Quantity Purchased") +
  theme_minimal()

# --- 2. Performance Trend: Cost per Unit by Year and Product ---
ggplot(df, aes(x = as.factor(Year), y = Cost_per_Unit, group = Product, color = Product)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(title = "Performance Trend: Cost per Unit by Year and Product",
       x = "Year", y = "Cost per Unit") +
  theme_minimal()

# --- 3. Performance Trend: Total Cost by Year and Product ---
ggplot(df, aes(x = as.factor(Year), y = Total_Cost, group = Product, color = Product)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(title = "Performance Trend: Total Cost by Year and Product",
       x = "Year", y = "Total Cost") +
  theme_minimal()

# --- 4. Grouped Summary Statistics by Product ---
summary_stats_by_product <- df %>%
  group_by(Product) %>%
  summarise(
    Mean_Quantity = mean(Quantity),
    Mean_Cost_Per_Unit = mean(Cost_per_Unit),
    Mean_Total_Cost = mean(Total_Cost),
    Median_Quantity = median(Quantity),
    Median_Cost_Per_Unit = median(Cost_per_Unit),
    Median_Total_Cost = median(Total_Cost),
    SD_Quantity = sd(Quantity),
    SD_Cost_Per_Unit = sd(Cost_per_Unit),
    SD_Total_Cost = sd(Total_Cost)
  )

# Print grouped summary statistics
print(summary_stats_by_product)