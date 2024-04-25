library(tidyverse)
library(readr)
heart_disease_data_1 <- read_csv("heart_disease_data_1.csv")
colors <- c("#FDAE61", 
            "#D9EF8B") 

sex = heart_disease_data_1$sex

plot(x = heart_disease_data_1$age, y = heart_disease_data_1$thalach,
     pch = 19,col = colors[factor(sex)],
     xlab = "Age", ylab = "Maximum Heart Rate Achieve",
     main = "Relationship between Age and Maximum Heart Rate ")

legend("topright",c("female","male"), pch = 19,col = colors)
