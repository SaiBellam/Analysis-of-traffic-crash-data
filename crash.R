# Load the required libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Read the CSV file and store it in a data frame
crash_data <- read.csv("crash_data.csv")

# Check the structure of the data frame
str(crash_data)

# checking NA values
sum(is.na(crash_data))
for(i in 1:ncol(crash_data)){
  crash_data[is.na(crash_data[,i]), i] <- mean(crash_data[,i], na.rm = TRUE)
}

# Create a bar chart of the number of crashes by county
crash_count_by_county <- crash_data %>%
  group_by(County) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(10)

ggplot(crash_count_by_county, aes(x = County, y = count, fill = County)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Counties by Number of Crashes", x = "County", y = "Number of Crashes") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Create a scatter plot of speed limit vs number of vehicles involved
ggplot(crash_data, aes(x = Speed_Limit_at_Crash_Site, y = Total_Motor_Vehicles)) +
  geom_point(alpha = 0.5) +
  labs(title = "Speed Limit vs Number of Vehicles Involved in Crashes", x = "Speed Limit at Crash Site", y = "Number of Vehicles Involved")


# Create a stacked bar chart of the number of crashes by crash type and drug use
crash_count_by_type_and_drug_use <- crash_data %>%
  group_by(Crash_Type, Crash_Drug_Use) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

ggplot(crash_count_by_type_and_drug_use, aes(x = Crash_Type, y = count, fill = Crash_Drug_Use)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Crashes by Crash Type and Drug Use", x = "Crash Type", y = "Number of Crashes") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Heatmap of crashes by month and county
crash_count_by_month_and_county <- crash_data %>%
  group_by(Crash_Month, County) %>%
  summarise(count = n()) %>%
  arrange(Crash_Month, County)

ggplot(crash_count_by_month_and_county, aes(x = County, y = Crash_Month, fill = count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Heatmap of Crashes by Month and County", x = "County", y = "Month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Box plot of speed limit by crash type:
ggplot(crash_data, aes(x = Crash_Type, y = Speed_Limit_at_Crash_Site)) +
  geom_boxplot() +
  labs(title = "Box Plot of Speed Limit by Crash Type", x = "Crash Type", y = "Speed Limit at Crash Site") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Stacked bar chart of crashes by crash type and drinking involvement
crash_count_by_type_and_drinking <- crash_data %>%
  group_by(Crash_Type, Crash_Drinking) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

ggplot(crash_count_by_type_and_drinking, aes(x = Crash_Type, y = count, fill = Crash_Drinking)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Crashes by Crash Type and Drinking Involvement", x = "Crash Type", y = "Number of Crashes") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


##################################### Statistical Analysis

# Hypothesis test for the difference in means of speed limit between injury and fatal crashes:
# Subset data for injury and fatal crashes
injury_data <- subset(crash_data, Crash == 0)
fatal_data <- subset(crash_data, Crash == 1)

# Conduct t-test for difference in means
t_test_result <- t.test(injury_data$Speed_Limit_at_Crash_Site, fatal_data$Speed_Limit_at_Crash_Site)

# Print results
cat("t-test for difference in means of speed limit between injury and fatal crashes:\n")
cat("t-value:", t_test_result$statistic, "\n")
cat("p-value:", t_test_result$p.value, "\n")


# Logistic regression to predict the likelihood of a fatal crash based on crash type and drinking involvement:
# Create binary variables for crash type and drinking involvement
crash_data$Crash_Type_Binary <- ifelse(crash_data$Crash_Type %in% c("Head-On", "Rear-End"), 0, 1)
crash_data$Drinking_Binary <- ifelse(crash_data$Crash_Drinking == "Drinking Involved", 1, 0)

# Fit logistic regression model
log_reg_model <- glm(Crash ~ Crash_Type_Binary + Drinking_Binary, data = crash_data, family = "binomial")

# Print model summary
summary(log_reg_model)


# ANOVA for the effect of county on the number of vehicles involved in a crash:
# Fit ANOVA model
anova_result <- aov(Total_Motor_Vehicles ~ County, data = crash_data)

# Print results
cat("ANOVA for effect of county on number of vehicles involved in a crash:\n")
summary(anova_result)

