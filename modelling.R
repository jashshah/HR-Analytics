source('fe_test.R')

#### Logistic Regression ####

glm.model <- glm(formula = DOL ~ ., 
                 data = data.frame(x_train, y_train), 
                 family = binomial(link = 'logit'))

summary(glm.model)

pred_prob_test_glm <- predict(glm.model, newdata = x_test, type = 'response')

pred_vals_test_glm <- as.factor(ifelse(pred_prob_test_glm >= 0.5, 1, 0))

caret::confusionMatrix(pred_vals_test_glm, y_test$DOL, mode = 'prec_recall', positive = '1')

roc(y_test$DOL, pred_prob_test_glm)$auc

#### Decision Trees ####

dt.model <- rpart(formula = DOL ~ ., 
                  data = data.frame(x_train, y_train))

plot(dt.model)
text(dt.model, pretty = TRUE)

pred_prob_test_dt <- predict(dt.model, newdata = x_test)[,2]

pred_vals_test_dt <- predict(dt.model, newdata = x_test, type = 'class')

caret::confusionMatrix(pred_vals_test_dt, y_test$DOL, mode = 'prec_recall', positive = '1')

roc(y_test$DOL, pred_prob_test_dt)$auc
