#four main tasks:Data Exploration, Random Sampling, Summary Statistics, and Plotting with ggplot2 # nolint

#setting working directory
#PART1: Data Exploration
setwd("C:/MSU Courses/Spring2025/mth_345") #working directory
#Load Required Packages
library(readxl)
library(ggplot2)

HousePrice <- read_excel("HousePrice.xlsx") #Import Dataset

dim(HousePrice)  # Number of rows and columns
str(HousePrice)  # Structure of the dataset
#The dataset has 2,919 rows and 13 columns.
#includes SalePrice, BldgType, LotArea, OverallCond, TotalBsmtSF, and MSZoning.

#Checking for Missing Values
missing_percent <- colSums(is.na(HousePrice)) / nrow(HousePrice) * 100 #na.omit(), so missing rows were removed.
print(missing_percent)  # Print missing value percentages
#yes, Most columns have 0% missing values (0.00000000) and Some columns (Exterior1st, BsmtFinSF2, TotalBsmtSF) have more missing values
dim(HousePrice)  # Checking new dimensions
#PART2: Random Sampling
set.seed((03474684)) #setting seed with my student ID
HousePrice_Sample <- HousePrice[sample(nrow(HousePrice), 1000), ]  # Random sample of 1000 

#PART3: Summary Statistics of
#1. HouseData_Sample
summary(HousePrice_Sample[, c("MSSubClass", "LotArea", "OverallCond", "BsmtFinSF2", "TotalBsmtSF", "SalePrice")])
#Summarizing my  findings
#LotArea varies from 8,450 to 14,260 square feet
#SalePrice has a right-skewed distribution (higher values pulling the mean up)


#no.of properties (in thousands) in HouseData_Sample for each dwelling type (BldgType) 
table(HousePrice_Sample$BldgType) / 1000
table(HousePrice_Sample$MSZoning)
table(HousePrice_Sample$BldgType, HousePrice_Sample$MSZoning)
table(HousePrice_Sample$OverallCond, HousePrice_Sample$LotConfig)
#Summary Statistics for Basement Area by Dwelling Type
aggregate(TotalBsmtSF ~ BldgType, data = HousePrice_Sample, summary)
#the 20th, 40th, 60th and 80th percentiles of sale prices (SalePrice) for eachzoning category(MSZoning).  # nolint: line_length_linter.
aggregate(SalePrice ~ MSZoning, data = HousePrice_Sample, FUN = function(x) quantile(x, probs = c(0.2, 0.4, 0.6, 0.8)))  
#Contingency Table for Dwelling Type vs Condition
table(HousePrice_Sample$BldgType, HousePrice_Sample$OverallCond)
# Independence Check
prop_table <- prop.table(table(HousePrice_Sample$BldgType, HousePrice_Sample$OverallCond), margin = 2) 
print(prop_table)

# Store the Chi-Square test result in a variable
chi_test1 <- chisq.test(table(HousePrice_Sample$BldgType, HousePrice_Sample$OverallCond), simulate.p.value = TRUE, B = 10000)
print(chi_test1)

# Check significance
if (chi_test1$p.value < 0.05) {
  print("There is a significant association (dependent variables).")
} else {
  print("There is no significant association (independent variables).")
}


#To Note:If Exterior1st and MSZoning are independent, the proportions of exterior 
#materials should be approximately the same across all zoning classifications.
table(HousePrice_Sample$Exterior1st, HousePrice_Sample$MSZoning)
# To Note: If Exterior1st and MSZoning are independent, 
# the proportions of exterior materials should be approximately the same across all zoning classifications.
table(HousePrice_Sample$Exterior1st, HousePrice_Sample$MSZoning)

# Are they independent?
prop_table2 <- prop.table(table(HousePrice_Sample$Exterior1st, HousePrice_Sample$MSZoning), margin = 2) 
print(prop_table2)

# Store the Chi-Square test result in a variable
chi_test2 <- chisq.test(table(HousePrice_Sample$Exterior1st, HousePrice_Sample$MSZoning), simulate.p.value = TRUE, B = 10000)
print(chi_test2)

# Check significance
if (chi_test2$p.value < 0.05) {
  print("There is a significant association (dependent variables).")
} else {
  print("There is no significant association (independent variables).")
}


#factor

#PART4:Plotting with ggplot2
#1.distribution of the dwelling type (BldgType) across HouseData_Sample
ggplot(HousePrice_Sample, aes(x = BldgType)) + geom_bar(fill = "blue") + labs(title = "Distribution of Dwelling Types", x = "Dwelling Type", y = "Count") + theme_minimal() 
ggsave("Distribution of Dwelling Types in HousePrice Sample.png", width = 8, height = 6) 

#2: overall condition (OverallCond) distribute among each dwelling type (BldgType)group
ggplot(HousePrice_Sample, aes(x = BldgType, fill = as.factor(OverallCond))) + geom_bar(position = "dodge") + labs(title = "Overall Condition Distribution by Dwelling Type", x = "Dwelling Type", y = "Count", fill = "Overall Condition") + theme_minimal() 
ggsave("Overall Condition Distribution by Dwelling Type.png", width = 8, height = 6) 
#difference between OverallCond as a number vs. a factor.
# Without factor conversion
ggplot(HousePrice_Sample, aes(x = BldgType, fill = as.factor(OverallCond))) + geom_bar(position = "dodge") + labs(title = "Overall Condition Distribution by Dwelling Type (No Factor)", x = "Dwelling Type", y = "Count") + theme_minimal() 
ggsave("dwelling type Without factor conversion.png", width = 8, height = 6)
# With factor conversion
ggplot(HousePrice_Sample, aes(x = BldgType, fill = as.factor(OverallCond))) + geom_bar(position = "dodge") + labs(title = "Overall Condition Distribution by Dwelling Type (Factor)", x = "Dwelling Type", y = "Count", fill = "Overall Condition") + theme_minimal() # nolint
ggsave("dwelling type With factor conversion.png", width = 8, height = 6)
#3. distribution of the sales price (SalePrice) across HouseData_Sample?
ggplot(HousePrice_Sample, aes(x = SalePrice)) + geom_histogram(binwidth = 10000, fill = "green", color = "black") + labs(title = "Distribution of Sale Prices", x = "Sale Price ($)", y = "Count") + theme_minimal() 
ggsave("distribution of the sales price across HouseData_Sample.png", width = 8, height = 6) 
summary(HousePrice_Sample$SalePrice)
library(moments)  # If not installed, install first: install.packages("moments")
skewness(HousePrice_Sample$SalePrice)
#If skewness > 1 or < -1, it's highly skewed.
#If skewness is close to 0, it's normally distributed.
#Comment on distribution:  This histogram shows how house sale prices are distributed. Most houses seem to be within a specific price range, with a few outliers on the higher end. 
#whether SalePrice follows a normal distribution?
#chi_test

#4. sale prices (SalePrice) differ by dwelling type (BldgType)?
ggplot(HousePrice_Sample, aes(x = BldgType, y = SalePrice, fill = BldgType)) + geom_boxplot() + labs(title = "Sale Price Distribution by Dwelling Type", x = "Dwelling Type", y = "Sale Price ($)") + theme_minimal() 
ggsave("sale prices differ by dwelling type.png", width = 8, height = 6)

#5. Are there differences in sale price (SalePrice) based on overall condition (OverallCond)?
ggplot(HousePrice_Sample, aes(x = as.factor(OverallCond), y = SalePrice, fill = as.factor(OverallCond))) + geom_boxplot() + labs(title = "Sale Price vs. Overall Condition", x = "Overall Condition", y = "Sale Price ($)") + theme_minimal()
ggsave("overall differences in sale price.png", width = 8, height = 6)

#6. does the overall condition (OverallCond) affect the sale price (SalePrice) across different types of dwelling 
ggplot(HousePrice_Sample, aes(x = as.factor(OverallCond), y = SalePrice, fill = BldgType)) + geom_boxplot() + facet_wrap(~ BldgType) + labs(title = "Sale Price vs. Overall Condition Across Dwelling Types", x = "Overall Condition", y = "Sale Price ($)") + theme_minimal()
ggsave("overall condition affect the sale price.png", width = 8, height = 6)

#7. relationship between the total square footage of the basement (TotalBsmtSF) and the sale price (SalePrice)?
ggplot(HousePrice_Sample, aes(x = TotalBsmtSF, y = SalePrice)) + geom_point(alpha = 0.5) + geom_smooth(method = "lm", formula = y ~ x, color = "red") + labs(title = "Sale Price vs. Total Basement Square Footage", x = "Total Basement SF", y = "Sale Price ($)") + theme_minimal() 
ggsave("SalePrice_vs_BasementSF.png.png", width = 8, height = 6)
#Comment: A positive trend is expected—houses with larger basements generally have higher prices. 

#8. relationship between the total square footage of the basement (TotalBsmtSF) and the sale price (SalePrice) depends on if the property is in corner lot (LotConfig)? 
ggplot(HousePrice_Sample, aes(x = TotalBsmtSF, y = SalePrice, color = LotConfig)) + geom_point(alpha = 0.5) + geom_smooth(method = "lm", formula = y ~ x) + labs(title = "Sale Price vs. Basement SF (by Lot Configuration)", x = "Total Basement SF", y = "Sale Price ($)") + theme_minimal() 
ggsave("SalePrice_vs_BasementSF_by_LotConfig.png.png", width = 8, height = 6) 
#Comment: The relationship between basement square footage and sale price seems to vary based on lot configuration. Corner lots have a wider range of prices compared to other configurations. 

#9.1does the lot size (LotArea) influence the sale price (SalePrice) before moving outliers? 
ggplot(HousePrice_Sample, aes(x = LotArea, y = SalePrice)) + geom_point(alpha = 0.5) + geom_smooth(method = "lm", formula = y ~ x, color = "blue") + labs(title = "Sale Price vs. Lot Area (Before Removing Outliers)", x = "Lot Area (sq ft)", y = "Sale Price ($)") + theme_minimal() 
ggsave("Sale Price vs. Lot Area (Before Removing Outliers).png", width = 8, height = 6) 
#after moving outliers?
# Identifying and removing extreme outliers
Q1 <- quantile(HousePrice_Sample$LotArea, 0.25)
Q3 <- quantile(HousePrice_Sample$LotArea, 0.75)
IQR <- Q3 - Q1
upper_bound <- Q3 + 1.5 * IQR
lower_bound <- Q1 - 1.5 * IQR
HousePrice_Sample_Clean <- HousePrice_Sample[HousePrice_Sample$LotArea > lower_bound & HousePrice_Sample$LotArea < upper_bound, ] 

#9.2 plotting after removing outliers
ggplot(HousePrice_Sample_Clean, aes(x = LotArea, y = SalePrice)) + geom_point(alpha = 0.5) + geom_smooth(method = "lm", formula = y ~ x, color = "red") + labs(title = "Sale Price vs. Lot Area (After Removing Outliers)", x = "Lot Area (sq ft)", y = "Sale Price ($)") + theme_minimal() # nolint
ggsave("Sale Price vs. Lot Area (After Removing Outliers).png", width = 8, height = 6)
# after removing the outlier, the relationship became more apparent
