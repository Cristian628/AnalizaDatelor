library(tidyverse)

custom_data <- read.csv("C:\\Users\\capat\\Desktop\\Univer\\AD\\CapațînaCristian\\Lab1\\BankChurners.csv")

# Influența locației asupra salariului
ggplot(data, aes(x = Attrition_Flag, y = Customer_Age, fill = Card_Category)) +
  geom_boxplot() +
  labs(x = "Există clientul sau nu", y = "Vărsta clienților", title = "Influența cardurilor asupra clienților existenți și cei care au părăsit serivicul") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  guides(fill = guide_legend(title = "Cardurile"))

correlation_matrix <- cor(custom_data[, c("Customer_Age", "CLIENTNUM", "Total_Relationship_Count")])
print(correlation_matrix)

summary(custom_data)

glimpse(custom_data)

ggplot(custom_data, aes(x = Attrition_Flag)) +
  geom_bar() +
  labs(title = "Distribution of Attrition Flag",
       x = "Attrition Flag",
       y = "Nr clienți") +
  theme_minimal()


ggplot(custom_data, aes(x = Customer_Age)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Distribution of Customer Age",
       x = "Customer Age",
       y = "Nr clienți") +
  theme_minimal()


ggplot(custom_data, aes(x = Gender)) +
  geom_bar() +
  labs(title = "Distribution of Gender",
       x = "Gender",
       y = "Nr clienți") +
  theme_minimal()

glimpse(custom_data)

