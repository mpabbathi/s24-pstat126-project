library(ggplot2)
dataset <- read.csv("~/Documents/GitHub/s24-pstat126-project/dataset/heart_disease_data_1.csv")

# plotting data
plot(dataset$age, dataset$thalach)

# fitting model
fit <- lm(formula = thalach ~ age, data = dataset)
summary(fit)

# plotting model
p <- ggplot(dataset, aes(x = age, y = thalach)) + geom_point() +
  labs(x = 'Age', y = 'Max Heart Rate')
p + geom_smooth(method = 'lm', 
                formula = 'y ~ x',
                se = F)

# checking normality of variables
qqplot(dataset$age, dataset$thalach)

#finding residuals
res <- resid(fit)

# produce residual vs. fitted plot 
plot(fitted(fit), res) 

# add a horizontal line at 0 
abline(0,0) 

# checking normality of residuals
qqnorm(res)
qqline(res)

# transforming x variable
fit2 <- lm(formula = thalach ~ log(age), data = dataset)
summary(fit2)

res2 <- resid(fit2)
qqnorm(res2)

