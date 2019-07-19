# Loading librarues -------------------------------------------------------
library(tidyverse)
library(vroom)
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
library(purrr)

set.seed(999)

# Import Dataset ----------------------------------------------------------
train <- vroom("C:/Users/LEI00023/Downloads/pml-training.csv", delim = ",", 
               na=c("NA","#DIV/0!", ""))

test <- vroom("C:/Users/LEI00023/Downloads/pml-testing.csv", delim = ",",
              na=c("NA","#DIV/0!", ""))


train <- train[,-c(1:7)] %>% 
         keep(~sum(is.na(.x))==0) %>% 
         mutate(id=row_number(),
                classe = as.factor(classe))


test <- test[,-c(1:7)] %>% keep(~sum(is.na(.x))==0)

# Exploratory Data Analysis -----------------------------------------------

train_train <- train %>% group_by(classe) %>% sample_frac(size = .80) %>% ungroup()

train_cv <- train %>% anti_join(train_train, by = "id") %>% select(-id)

train_train <- train_train %>% select(-id)

count(train,classe) %>% mutate(prop = n/sum(n))

count(train_train,classe) %>% mutate(prop = n/sum(n))

count(train_cv,classe) %>% mutate(prop = n/sum(n))


count(train, classe) %>% 
  ggplot()+
  geom_bar(aes(x=classe, y=n, fill=classe), stat = "identity")


# Decision Tree -----------------------------------------------------------

decision_model <- rpart(classe ~ ., data=train_train, method="class") # class perche classificazione

prediction_dt <- predict(decision_model, train_cv, type = "class") # test_preliminare

rpart.plot(decision_model, main="Classification Tree", extra=102, under=TRUE, faclen=0)

summary_pred_dt <- data.frame(obs = as.factor(train_cv$classe), predicted = prediction_dt)

acc_dt <-sum(summary_pred_dt$obs==summary_pred_dt$predicted)/nrow(summary_pred_dt)#accuracy

conf_mat_dt<-confusionMatrix(prediction_dt, summary_pred_dt$obs)
conf_mat_dt$table

# Random Forest CV --------------------------------------------------------

control.parms <- trainControl(method="cv", 5)

model_cv <- train(classe ~ ., data=train_train, method="rf",
                  trControl=control.parms)

prediction_cv <- predict(model_cv, train_cv, type = "raw")

summary_pred_cv <- data.frame(obs = as.factor(train_cv$classe), predicted = prediction_cv)

acc_cv <- sum(summary_pred_cv$obs==summary_pred_cv$predicted)/nrow(summary_pred_cv)#accuracy

conf_mat_cv<-confusionMatrix(prediction_cv, summary_pred_cv$obs)
conf_mat_cv$table

# Accuracy Comparison -----------------------------------------------------

acc_dt
acc_cv

# Out Of Sample Error -----------------------------------------------------

1 - acc_dt
1 - acc_cv


# Final Sumbission --------------------------------------------------------

prediction_final <- predict(model_cv, test, type = "raw")
