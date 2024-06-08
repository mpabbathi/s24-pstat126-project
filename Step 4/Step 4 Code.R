# load dataset
heart_disease_data <- read.csv("~/Documents/GitHub/s24-pstat126-project/dataset/heart_disease_data_1.csv")
heart_disease_data$sex<-as.factor(heart_disease_data$sex)
heart_disease_data$cp<-as.factor(heart_disease_data$cp)
heart_disease_data$fbs<-as.factor(heart_disease_data$fbs)
heart_disease_data$restecg<-as.factor(heart_disease_data$restecg)
heart_disease_data$exang<-as.factor(heart_disease_data$exang)
heart_disease_data$slope<-as.factor(heart_disease_data$slope)
heart_disease_data$target<-as.factor(heart_disease_data$target)

# split data into training and testing
set.seed(7)
train_idx <- sample(1:nrow(heart_disease_data), 0.8 * nrow(heart_disease_data)) 
train_data <- heart_disease_data[train_idx, ] 
test_data <- heart_disease_data[-train_idx, ]

# lasso regression
require(glmnet)
y <- train_data$thalach
x <- scale(data.matrix(train_data[ , -8]))
lasso_model <- cv.glmnet(x, y, alpha = 1)
lambda_lasso <- lasso_model$lambda.min

# plot mse vs. lambda
par(mar = c(7, 4, 2.2, 0.5))
plot(lasso_model, cex = 0.8)

# choose best model
best_lasso_model <- glmnet(x, y, alpha = 1, lambda = lambda_lasso)
coef(best_lasso_model)

# making predictions
new_x <- scale(data.matrix(test_data[ , -8]))
y_pred_lasso <- predict(best_lasso_model, s = lambda_lasso, newx = new_x)
plot(y_pred_lasso, test_data$thalach, xlab = "Predicted Values", ylab = "Observed Values")
abline(1, 1)

# ridge regression
ridge_model <- cv.glmnet(x, y, alpha = 0)
lambda_ridge <- ridge_model$lambda.min

# plot mse vs. lambda
par(mar = c(7, 4, 2.2, 0.5))
plot(ridge_model, cex = 0.8)

# choose best model
best_ridge_model <- glmnet(x, y, alpha = 0, lambda = lambda_ridge)
coef(best_ridge_model)

# making predictions
y_pred_ridge <- predict(best_ridge_model, s = lambda_ridge, newx = new_x)
plot(y_pred_ridge, test_data$thalach, xlab = "Predicted Values", ylab = "Observed Values")
abline(1, 1)

# fitting ols model
fit <- lm(thalach ~ slope + age + cp + exang + target + trestbps + restecg, data = train_data)
y_pred_mlr <- predict(fit, newdata = test_data)

# combining data frames
y_pred_lasso <- as.data.frame(y_pred_lasso)
y_pred_ridge <- as.data.frame(y_pred_ridge)
y_pred_mlr <- as.data.frame(y_pred_mlr)
y_observed_og <- test_data$thalach

colnames(y_pred_lasso) <- c("values")
colnames(y_pred_ridge) <- c("values")
colnames(y_pred_mlr) <- c("values")

y_pred_lasso$Source <- rep("Lasso", length(y_pred_lasso))
y_pred_ridge$Source <- rep("Ridge", length(y_pred_ridge))
y_pred_mlr$Source <- rep("MLR", length(y_pred_mlr))

combined_preds_og <- rbind(y_pred_lasso, y_pred_ridge, y_pred_mlr)
y_observed <-rep(y_observed_og, 3)
combined_preds <- cbind(y_observed, combined_preds_og)

# making combined plot
library(ggplot2)

combined_plot <- ggplot(combined_preds, aes(x = y_observed, y = values, color = Source)) +
  geom_point() + 
  labs(x = "Observed Values", y = "Predicted Values")