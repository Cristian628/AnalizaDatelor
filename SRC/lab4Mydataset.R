library(ggplot2)
library(openintro)

comics <- read.csv("C:\\Users\\capat\\Desktop\\Univer\\AD\\CapațînaCristian\\Lab1\\BankChurners.csv", encoding = "UTF-8")

# Exemplu 1: Scatterplot Customer Age vs. Credit Limit
ggplot(data = comics, aes(x = Customer_Age, y = Credit_Limit)) +
  geom_point() +
  labs(
    x = "Customer Age",
    y = "Credit Limit",
    title = "Scatterplot: Customer Age vs. Credit Limit"
  )

# Exemplu 2: Boxplot Total Transactions Count by Card Category
ggplot(data = comics, aes(x = Card_Category, y = Total_Trans_Ct, fill = Card_Category)) +
  geom_boxplot() +
  labs(
    x = "Card Category",
    y = "Total Transactions Count",
    title = "Boxplot: Total Transactions Count by Card Category"
  )

# Exemplu 3: Scatterplot Total Transaction Amount vs. Total Transactions Count
ggplot(data = comics, aes(x = Total_Trans_Amt, y = Total_Trans_Ct)) +
  geom_point() +
  labs(
    x = "Total Transaction Amount",
    y = "Total Transactions Count",
    title = "Scatterplot: Total Transaction Amount vs. Total Transactions Count"
  )

# Exemplu 4: Scatterplot Total Revolving Balance vs. Avg Open To Buy
ggplot(data = comics, aes(x = Total_Revolving_Bal, y = Avg_Open_To_Buy)) +
  geom_point() +
  labs(
    x = "Total Revolving Balance",
    y = "Avg Open To Buy",
    title = "Scatterplot: Total Revolving Balance vs. Avg Open To Buy"
  )

# Exemplu 5: Scatterplot Monthly Inactive Months vs. Contacts Count 12 Months
ggplot(data = comics, aes(x = Months_Inactive_12_mon, y = Contacts_Count_12_mon)) +
  geom_point() +
  labs(
    x = "Monthly Inactive Months",
    y = "Contacts Count 12 Months",
    title = "Scatterplot: Monthly Inactive Months vs. Contacts Count 12 Months"
  )

