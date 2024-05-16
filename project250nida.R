library(readxl)
library(dplyr)
project <- read.csv("data_final.csv")
head(project)
str(project)
summary(project)


#multiple linear regression
#1
model <- lm(turnout ~ pop.density + income + college + white + black, data = project)
summary(model)
summary(model)$r.squared
#the model suggests that population density, income, percentage of college-educated individuals, 
#and racial demographics (white and black population percentages) are significant predictors of voter turnout in counties.
#the model explains only about 31.8% of the variation in voter turnout

#anova
#1
#there is a significant difference in average income among counties in different states.
result_anova <- aov(income ~ state, data = project)
summary(result_anova)


model <- lm(crime ~ pop.density + college + income, data = project)
summary(model)
# the model suggests that population density, percentage of college-educated individuals, 
# and income are significant predictors of the crime rate in counties.
# the model explains only about 18.71% of the variation in the crime rate

#most significant relation between crime rate is education level
cor(project$crime, project$college) #0.42 its weird btw??
cor(project$crime, project$income) #0.33
cor(project$crime, project$pop.density) #0.15

#what about other relationships
cor(project$college, project$income) #0.696 good relationship
cor(project$crime, project$black) #0.19
cor(project$crime, project$white) #-0.25 we can test it, it can be racist???

cor(project$income, project$farm)
# Assuming your data frame is named 'county_data' and it contains the variables: democrat and republican

# Load necessary library for plotting
library(ggplot2)

# Create a density plot
ggplot(project, aes(x = democrat, fill = "Democratic")) +
  geom_density(alpha = 0.5) +
  geom_density(aes(x = republican, fill = "Republican"), alpha = 0.5) +
  labs(title = "Distribution of Democratic and Republican Votes Across Counties",
       x = "Percentage of Votes",
       y = "Density") +
  scale_fill_manual(values = c("Democratic" = "blue", "Republican" = "red")) +
  theme_minimal()