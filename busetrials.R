getwd()
#
library(readxl)
data_final <- read.csv("data_final.csv")
data<-read_xlsx("projectdata.xlsx")

head(data)

str(data)

missing_counts <- colSums(is.na(data))
missing_counts


total_obs <- nrow(data)

missing_percentage <- (missing_counts / total_obs) * 100
missing_percentage

missing_summary <- data.frame(
  Variable = names(missing_counts),
  Missing_Count = missing_counts,
  Missing_Percentage = missing_percentage
)

missing_summary

# In the pre-processing part, the structure of the data,the number of NA's 
# and the column-based ratio of NA values were examined.

# In the 'msa' and 'pmsa' columns containing statistical area information, 
# NA values were observed at rates of 73.575% and 99.11%, respectively.

# We excluded these two columns from our data set due to their high rates of NA values and 
# their lack of critical importance for our research questions.

# After omitting the NA values observed at a rate of 0.86% in the 'democrat', 'republican', and 'Perot' columns, 
# which represent citizens' voting preferences proportionally, we decided to keep these columns in our data set 
# as we found them meaningful.


cleaned_data <- data[, -c(3,4)]
cleaned_data

data_final<- na.omit(cleaned_data)

data_final

write.csv(data_final, "data_final.csv", row.names = FALSE)

colSums(is.na(data_final))

# After the cleaning part, we have 17 variables and 3114 observations. Data is ready to analyze.

data_final<-read.csv("data_final.csv")

head(data_final)
summary(data_final)

library(sqldf)

sqldf("SELECT COUNT(crime) FROM data_final WHERE crime==0")


# We can see some frequency tables of the varibles to check whether there is a normality or not.

hist(data_final$pop.density, main = "Population Density Histogram", xlab = "Population Density")

hist(data_final$pop, main = "Population Histogram", xlab = "Population")

hist(data_final$pop.change, main = "Population Change Histogram", xlab = "Population Change")

hist(data_final$age6574, main = " Percentage of Age between 65-74 Histogram", xlab = "Percentage of Age between 65-74") #approximately normal

# Q-Q plots

qqnorm(data_final$pop.density)
qqline(data_final$pop.density)

qqnorm(data_final$age6574)
qqline(data_final$age6574)

shapiro.test(data_final$age6574)
shapiro.test(data_final$pop.density)






# In general, variables are not normally distributed. However, in this project, we assume that they are normal and conduct
# our hypothesis tests

# Does population density vary significantly across different states?
# Take the p-value as 0.05.
# H0: Variances of population densities are equal.
# H1: Variances of population densities are not equal.

anova_model <- aov(pop.density ~ state, data = data)

summary(anova_model)

?aov

# F value is relatively great (?) and p-value is significantly smaller than 0.05, then we can reject the null hypothesis. 
# Population density vary significantly across different states.



class(data_final)
colnames(data_final)
numeric_data<-data_final[,c(3:17)]
cor_matrix<-cor(numeric_data)
print(cor_matrix)


install.packages("corrplot")
library(corrplot)

corrplot(cor_matrix, type="upper",
         title="Correlation Matrix of Voting Elements")

# We saw the direction and strength of the linear relationship between variables.



#anova
#1
#there is a significant difference in average income among counties in different states.
result_anova1 <- aov(income ~ state, data = data_final)
summary(result_anova1)

#2
result_anova2 <- aov(college ~ state, data = data_final)
summary(result_anova2)

#3
result_anova3 <- aov(pop.density ~ state, data = data_final)
summary(result_anova3)

#multiple regression, trying to find best model

model1 <- lm(democrat ~ pop.density + college + income, data = data_final)
summary(model1) #0.09 not good

model2 <- lm(democrat ~ age6574 + college + income, data = data_final)
summary(model2) #0.06

model3 <- lm(democrat ~ pop + college + income, data = data_final)
summary(model3) #0.2319

model4 <- lm(democrat ~ white + college + sqrt(income), data = data_final)
summary(model4) #0.2374


data_final$income_ratio <- data_final$income/1000
summary(data_final)

model5 <- lm(democrat ~ white + college + income_ratio, data = data_final)
summary(model5) #0.2319

model6 <- lm(Perot ~ white + income + college, data = data_final)
summary(model6) #0.2989

<<<<<<< HEAD
model7<-lm(republican~white+income+college,data=data_final)
summary(model7) #0.03458

model8<-lm(republican~age6574+age75+farm,data=data_final)
summary(model8) #0.348
=======
prop.test(x = c(sum(data_final$republican), sum(data_final$democrat)), 
          n = c(sum(data_final$pop), sum(data_final$pop)),
          alternative = "two.sided")


#güncelemem
>>>>>>> 284bb78e4e2ebd8d181230c7e67bb63c37bdab05


