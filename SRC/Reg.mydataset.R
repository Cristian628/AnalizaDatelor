library(dplyr)
library(ggplot2)
library(caret)
library(rsample)
library(vip)
library(tidyr)
library(glmnet)

default_dataset <- read.csv("C:\\Users\\capat\\Desktop\\Univer\\AD\\CapațînaCristian\\Lab1\\BankChurners.csv")

dataset <- default_dataset

dataset$Gender <- case_when(
  dataset$Gender %in% "M" ~ "yes",
  dataset$Gender == "F" ~ "no",
  TRUE ~ "no"
)

#Partea 1
set.seed(123)
split <- initial_split(dataset, prop = 0.7, strata = 'Months_Inactive_12_mon')
dataset_train <- training(split)
dataset_test <- training(split)

model1 <- lm(Months_Inactive_12_mon ~ Credit_Limit, data = dataset_train)
model1

(model2 <- lm(Months_Inactive_12_mon ~ Credit_Limit + Total_Revolving_Bal, data = dataset_train))

model3 <- lm(Months_Inactive_12_mon ~ ., data = dataset_train)
model3

sigma(model1)

set.seed(123)
(cv_model1 <- train(
  form = Months_Inactive_12_mon ~ Credit_Limit,
  data = dataset_train,
  method = 'lm',
  trControl = trainControl(method = 'cv', number = 10)
))
set.seed(123)
cv_model2 <- train(
  Months_Inactive_12_mon ~ Credit_Limit + Total_Revolving_Bal,
  data = dataset_train,
  method = 'lm',
  trControl = trainControl(method = 'cv', number = 10)
)

set.seed(123)
cv_model3 <- train(
  Months_Inactive_12_mon ~ .,
  data = dataset_train,
  method = 'lm',
  trControl = trainControl(method = 'cv', number = 10)
)

summary(resamples(list(
  model1 = cv_model1,
  model2 = cv_model2,
  model3 = cv_model3)))

p1 <- ggplot(dataset_train, aes(Credit_Limit, Months_Inactive_12_mon)) +
  geom_point(size = 1, alpha = .4) +
  geom_smooth(se = F) +
  scale_y_continuous('Salaries per year in USD', labels = scales::dollar) +
  xlab('Company Country') +
  ggtitle(paste('Non-transformed variables with a\n',
                'non-linear relationshop'))
p1


p2 <- ggplot(dataset_train, aes(Credit_Limit, Months_Inactive_12_mon)) +
  geom_point(size = 1, alpha = .4) +
  geom_smooth(method = 'lm', se = F) +
  scale_y_log10('Salaries per year in USD', labels = scales::dollar,
                breaks = seq(0, 400000, by = 100000)) +
  xlab('Company Country') +
  ggtitle(paste('Transforming variables can privide a\n',
                'near-linear relationship'))
p2
gridExtra::grid.arrange(p1, p2, nrow = 1)

df1 <- broom::augment(cv_model1$finalModel, data = dataset_train)
glimpse(df1)
p1 <- ggplot(df1, aes(.fitted, .std.resid)) +
  geom_point(size = 1, alpha = .4) +
  xlab('Predicted values') +
  ylab('Residuals') +
  ggtitle('Model 1', subtitle = 'Months_Inactive_12_mon ~ Credit_Limit')
p1

df2 <- broom::augment(cv_model2$finalModel, data = dataset_train)
glimpse(df2)
p2 <- ggplot(df2, aes(.fitted, .std.resid)) +
  geom_point(size = 1, alpha = .4) +
  xlab('Predicted values') +
  ylab('Residuals') +
  ggtitle('Model 2', subtitle = 'Months_Inactive_12_mon ~ Credit_Limit + Total_Revolving_Bal')
p2


df3 <- broom::augment(cv_model3$finalModel, data = dataset_train)
glimpse(df3)
p3 <- ggplot(df3, aes(.fitted, .std.resid)) +
  geom_point(size = 1, alpha = .4) +
  xlab('Predicted values') +
  ylab('Residuals') +
  ggtitle('Model 3', subtitle = 'Months_Inactive_12_mon ~ ,')
p3

df1 <- mutate(df1, id = row_number())
glimpse(df1)
p1 <- ggplot(df1, aes(id, .std.resid)) + 
  geom_point(size = 1, alpha = .4) + 
  xlab('Row ID') +
  ylab('Residuals') +
  ggtitle('Model 1', subtitle = 'Correlated residuals.')
p1


df3 <- mutate(df3, id = row_number())
glimpse(df3)
p3 <- ggplot(df3, aes(id, .std.resid)) + 
  geom_point(size = 1, alpha = .4) + 
  xlab('Row ID') +
  ylab('Residuals') +
  ggtitle('Model 3', subtitle = 'Uncorrelated residuals.')
p3


vip(cv_model3, num_features = 10)


#PARTEA 2
# Logistic Regression


churn_split <- initial_split(dataset, prop = .7, strata = 'Gender')
churn_train <- training(churn_split)
churn_test <- testing(churn_split)

churn_train$Gender <- factor(churn_train$Gender, levels = c("no", "yes"))

model1 <- glm(Gender ~ Months_Inactive_12_mon, family = 'binomial', data = churn_train)
model2 <- glm(Gender ~ Total_Revolving_Bal, family = 'binomial', data = churn_train)

tidy(model1)
tidy(model2)

exp(coef(model1))
exp(coef(model2))


model3 <- glm(
  Gender ~ Months_Inactive_12_mon + Total_Revolving_Bal, family = 'binomial',
  data = churn_train
)
tidy(model3)

set.seed(123)
cv_model1 <- train(
  Gender ~ Months_Inactive_12_mon,
  data = churn_train,
  method = 'glm',
  family = 'binomial',
  trControl = trainControl(method = 'cv', number = 10)
)

set.seed(123) 
cv_model2 <- train(
  Gender ~ Months_Inactive_12_mon + Total_Revolving_Bal, 
  data = churn_train,
  method = 'glm',
  family = 'binomial',
  trControl = trainControl(method = 'cv', number = 10)
)

set.seed(123) 
cv_model3 <- train(
  Gender ~ .,
  data = churn_train,
  method = 'glm',
  family = 'binomial',
  trControl = trainControl(method = 'cv', number = 10)
)

summary( 
  resamples(
    list(
      model1 = cv_model1, model2 = cv_model2, model3 = cv_model3
    ))
)$statistics$Accuracy

pred_class <- predict(cv_model3, churn_train)
levels(pred_class)


confusionMatrix(
  data = relevel(pred_class, ref = 'yes'),
  reference = relevel(churn_train$Gender, ref = 'yes')
)

levels(churn_train$Gender)

library(ROCR)

m1_prob <- predict(cv_model1, churn_train, type = 'prob')$yes
m3_prob <- predict(cv_model3, churn_train, type = 'prob')$yes

perf1 <- prediction(m1_prob, churn_train$Gender) %>% 
  performance(measure = 'tpr', x.measure = 'fpr')

perf2 <- prediction(m3_prob, churn_train$Gender) %>% 
  performance(measure = 'tpr', x.measure = 'fpr')


plot(perf1, col = 'black', lty = 2)
plot(perf2, add = TRUE, col = 'blue')
legend(0.8, 0.2, legend = c('cv_model1', 'cv_model3'),
       col = c('black', 'blue'), lty = 2:1, cex = 0.6)


vip(cv_model3, num_features = 20)

