getwd()
#guncelleme
library(readxl)

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



