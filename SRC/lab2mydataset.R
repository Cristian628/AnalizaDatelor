 #Ex 1
library(openintro)
library(dplyr)
library(ggplot2)

comics <- read.csv("C:\\Users\\capat\\Desktop\\Univer\\AD\\CapațînaCristian\\Lab1\\BankChurners.csv", encoding = "UTF-8")

head(comics)

comics$Customer_age <- factor(comics$Customer_Age)
levels(comics$Customer_age)

comics$Gender <- factor(comics$Gender)
levels(comics$Gender)

tab <- table(comics$Customer_age, comics$Gender)
tab

# Ex 2
comics_filtered <- comics %>%
  filter(Customer_age != 'Gender') %>% 
  droplevels()

levels(comics_filtered$Customer_age)

# Ex 3
ggplot(comics, aes(x = Customer_age, fill = Gender)) + geom_bar(position = "dodge")
ggplot(comics, aes(x = Gender, fill = Card_Category)) + geom_bar(position = "dodge") + theme(axis.text.x = element_text(angle = 90))

# Ex 4
tab_proportions <- table(comics$Customer_age, comics$Gender)

conditional_proportions <- prop.table(tab_proportions, 2)

ggplot(data = as.data.frame(conditional_proportions), aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity") +
  labs(x = "Anii", y = "Proportion", fill = "Gen", title = "Ani clienților și genul")

# Ex 5
histogram1 <- ggplot(comics, aes(x = Customer_Age)) + 
  geom_histogram(binwidth = 3, fill = "blue", color = "black", alpha = 0.7) + 
  labs(x = "Anii", y = "frequency", title = "Histogram of Customer Age with a binwidth of 3")
histogram1

histogram2 <- ggplot(comics, aes(x = Customer_Age)) + 
  geom_histogram(binwidth = 10, fill = "green", color = "black", alpha = 0.7) + 
  labs(x = "Customer Age", y = "Frequency", title = "A second histogram of Customer Age with a binwidth of 10")
histogram2

histogram3 <- ggplot(comics, aes(x = Customer_Age)) + 
  geom_histogram(binwidth = 20, fill = "purple", color = "black", alpha = 0.7) + 
  labs(x = "Customer Age", y = "Frequency", title = "A third histogram of Customer Age with a binwidth of 60")
histogram3

 #Ex 6
boxplot1 <- ggplot(comics, aes(y = Customer_Age)) +
  geom_boxplot() +
  labs(y = "Customer income", title = "Boxplot of Customer age")
boxplot1

comics_no_out <- subset(comics, Customer_Age < 40000)

 #Ex 7
ggplot(comics, aes(x = Customer_Age)) +
  geom_histogram(binwidth = 2, fill = "green", color = "black", alpha = 0.7) +
  labs(x = "Anii", y = "Clienți") +
  facet_wrap(~ Gender, scales = "free_x") +
  ggtitle("Distribution of Gender by Age")

# Ex 8
raw_data <- comics

filtered_data <- raw_data %>%
  filter(Customer_Age < 30)

ggplot(filtered_data, aes(x = round(Customer_Age), fill = Income_Category)) +
  geom_histogram(binwidth = 1) +
  labs(x = "Vârstă", y = "Număr de Clienți", title = "Distribuția Vârstei Clienților în funcție de Categoria de Venit") +
  facet_wrap(~ Card_Category)

