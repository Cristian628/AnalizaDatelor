library(gapminder)
library(dplyr)
library(ggplot2)
library(gridExtra)

# Assuming "comics" is your dataset
comics <- read.csv("C:\\Users\\capat\\Desktop\\Univer\\AD\\CapațînaCristian\\Lab1\\BankChurners.csv", encoding = "UTF-8")

# Ex 1
gap_data <- comics %>%
  filter(Customer_Age == 44)

continent_life_expectancy_summary <- gap_data %>%
  group_by(Gender) %>%
  summarize(
    Mean_Card_Category = mean(Card_Category),
    Median_Card_Category = median(Card_Category)
  )

ggplot(data = gap_data, aes(x = Gender, y = Card_Category)) +
  geom_boxplot() +
  labs(
    x = "Gender",
    y = "Card_Category",
    title = "Boxplots by Gender"
  )

# Ex 2
spread_summary <- gap_data %>%
  group_by(Income_Category) %>%
  summarize(
    SD_Customer_Age = sd(Customer_Age),
    IQR_Customer_Age = IQR(Customer_Age),
    Count_of_Customers = n()
  )

ggplot(data = gap_data, aes(x = Card_Category, fill = Income_Category)) +
  geom_density(alpha = 0.5) +
  labs(
    x = "Card_Category",
    y = "Density",
    title = "Density Plot by Card Category and Income"
  )

# Ex 3
platinum_data <- gap_data %>%
  filter(Card_Category == "Platinum")

summary_measures <- platinum_data %>%
  summarize(
    Mean_Customer_Age = mean(Customer_Age),
    IQR_Customer_Age = IQR(Customer_Age)
  )

summary_measures

# Ex 4
density_plot_original <- ggplot(data = gap_data, aes(x = Total_Trans_Amt)) +
  geom_density(fill = "blue", color = "black") +
  labs(
    x = "Total_Trans_Amt",
    y = "Density",
    title = "Density Plot of Total Transaction Amount"
  )

# Ex 5
platinum_data <- gap_data %>%
  filter(Card_Category == "Platinum")

platinum_data <- platinum_data %>%
  mutate(is_outlier = if_else(Customer_Age < 30, TRUE, FALSE))

ggplot(data = platinum_data %>% filter(!is_outlier), aes(x = Card_Category, y = Customer_Age)) +
  geom_boxplot(fill = "blue", color = "black") +
  labs(
    x = "Card_Category",
    y = "Customer_Age",
    title = "Boxplot of Customer Age for Platinum Card Category"
  )

