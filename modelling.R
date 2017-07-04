source('fe_test.R')

#### Logistic Regression ####

### Model with Variance and Rating ###

glm.model <- glm(formula = quit ~ ., 
                 data = data.frame(x_train, y_train), 
                 family = binomial(link = 'logit'))

summary(glm.model) # AIC: 668.58

write.csv(summary(glm.model)$coefficients, 'model_with_variance_and_rating.csv')

pred_prob_test_glm <- predict(glm.model, newdata = x_test, type = 'response')

pred_vals_test_glm <- as.factor(ifelse(pred_prob_test_glm >= 0.5, 1, 0))

caret::confusionMatrix(pred_vals_test_glm, y_test$quit, mode = 'prec_recall', positive = '1')

# Accuracy: 0.9733
# Precision: 0.9752
# Recall: 0.9979

roc(y_test$quit, pred_prob_test_glm)$auc # 0.8724

write.csv(summary(glm.model)$coefficients, 'model_with_variance_and_rating.csv')

### Model without Variance and Rating ###

glm.model <- glm(formula = quit ~ .-Rating -hike, 
                 data = data.frame(x_train, y_train), 
                 family = binomial(link = 'logit'))

summary(glm.model) # AIC: 716.85

pred_prob_test_glm <- predict(glm.model, newdata = x_test, type = 'response')

pred_vals_test_glm <- as.factor(ifelse(pred_prob_test_glm >= 0.5, 1, 0))

caret::confusionMatrix(pred_vals_test_glm, y_test$quit, mode = 'prec_recall', positive = '1')

# Accuracy: 0.9733
# Precision: 0.9752
# Recall: 0.9979

roc(y_test$quit, pred_prob_test_glm)$auc # 0.799

write.csv(summary(glm.model)$coefficients, 'model_without_variance_and_rating.csv')
