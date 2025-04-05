# Load necessary libraries
library(rcompanion)
library(ppcor)        # For partial correlations
library(corrplot)     # For correlation matrix visualization
library(MASS)
library(factoextra)
library(ggplot2)
library(car)        # For VIF calculation
library(relaimpo)   # For relative importance
library(RcmdrMisc)  # For stepwise selection

#------------------------------------------------
# Set the working directory (replace with your directory path)
setwd(dirname(file.choose()))
getwd()
#------------------------------------------------
# Import the dataset
Covid_Data <- read.csv("u2657350_DS7006_CW2_data.csv", stringsAsFactors = FALSE)
str(Covid_Data)



# Display first few rows
head(Covid_Data)
# Summary statistics
summary(Covid_Data)

#------------------------------------------------

# Check for missing values
missing_values <- colSums(is.na(Covid_Data))
print(missing_values)

#------------------------------------------------
#Converted per into per thousand data through Excel
Covid_Per_Thousand <- read.csv(file.choose(), stringsAsFactors = FALSE)

str(Covid_Per_Thousand)
Covid_Per_Thousand$Old

# Display first few rows
head(Covid_Per_Thousand)

# Summary statistics
summary(Covid_Per_Thousand)

missing_values1 <- colSums(is.na(Covid_Per_Thousand))
print(missing_values1)

#------------------------------------------------
# Boxplot for the variable "Covid_Death"
# Label outliers and calculate the number of outliers
boxdata <- boxplot(Covid_Per_Thousand$Covid_Death, 
                   xlab = "Covid Death", 
                   ylab = "Count", 
                   col = "skyblue", 
                   outcol = "red", 
                   main = "Boxplot of Covid Death")

# Extract outliers
outliers <- boxdata$out

#for each outlier in boxdata
for(i in 1:length(outliers)){
  #add text to the boxplot
  text(boxdata$group[i], outliers[i],
       labels =  which(Covid_Per_Thousand$Covid_Death==outliers[i]),pos=4, cex=1,col = "darkred")}

# Print the number of outliers and the list of outliers
cat("Number of outliers detected:", length(outliers), "\n")
cat("Outlier values:\n", outliers, "\n")

#------------------------------------------------

# Perform normality test for dependent variable "Covid_Death"
shapiro_test_result <- shapiro.test(Covid_Per_Thousand$Covid_Death)

# Print Shapiro-Wilk normality test result
cat("Shapiro-Wilk Normality Test for Covid Death:\n")
print(shapiro_test_result)

# If p-value < 0.05, the distribution is not normal.
if (shapiro_test_result$p.value < 0.05) {
  cat("The data is not normally distributed (p < 0.05).\n")
} else {
  cat("The data is normally distributed (p >= 0.05).\n")
}

#------------------------------------------------
# Select dependent and independent variables
dependent_variable <- "Covid_Death"
independent_variables <- names(Covid_Per_Thousand)[sapply(Covid_Per_Thousand, is.numeric) & names(Covid_Per_Thousand) != dependent_variable]

# Print results
cat("Dependent variable:", dependent_variable, "\n")
cat("Independent variables:", paste(independent_variables, collapse = ", "), "\n")

#------------------------------------------------
# Perform normality tests for all independent variables
normality_results <- data.frame(
  Variable = character(), 
  P_Value = numeric(), 
  Normality = character(), 
  stringsAsFactors = FALSE
)
#------------------------------------------------
par(mfrow = c(3, 3))

# Boxplot for each variable
# Boxplot for dependent and independent variables
for (var in c(independent_variables)) {
  # Create a boxplot for each variable
  boxplot(Covid_Per_Thousand[[var]], main = paste("Boxplot of", var), 
          ylab = var, col = "lightblue")
}


#------------------------------------------------

# QQplot for dependent and independent variables
par(mfrow = c(3, 3))  # Adjust layout for multiple plots
for (var in c(independent_variables)) {
  # Create a QQ plot for each variable
  qqnorm(Covid_Per_Thousand[[var]], main = paste("QQ Plot of", var))
  qqline(Covid_Per_Thousand[[var]], col = "red")
}

#------------------------------------------------

# Histogram with density and standardization line for dependent variable
hist(Covid_Per_Thousand$Covid_Death, 
     main = paste("Histogram of", dependent_variable),
     xlab = dependent_variable, col ="lightblue", 
     border = "black", freq = FALSE)  # freq = FALSE makes it a density plot
rug (Covid_Per_Thousand[[dependent_variable]])

# Add density curve
lines(density(Covid_Per_Thousand[[dependent_variable]]), col = "blue", lwd = 2)

# Add standard normal distribution curve
x_vals <- seq(min(Covid_Per_Thousand[[dependent_variable]], na.rm = TRUE), 
              max(Covid_Per_Thousand[[dependent_variable]], na.rm = TRUE), length = 100)
curve_vals <- dnorm(x_vals, 
                    mean = mean(Covid_Per_Thousand[[dependent_variable]], na.rm = TRUE), 
                    sd = sd(Covid_Per_Thousand[[dependent_variable]], na.rm = TRUE))
lines(x_vals, curve_vals, col = "red", lwd = 2, lty = 2)  # Standardization curve (red, dashed)


# Add a legend
legend("topright", legend = c("Density curve", "Normal curve"),
       lty = c("solid", "dotted"), cex = 0.7)

#------------------------------------------------

plotNormalHistogram(Covid_Per_Thousand$Covid_Death, main = "Histogram", xlab = "Covid_death",col = "light blue")

#------------------------------------------------

# Repeat for each independent variable
for (var in independent_variables) {
  hist(Covid_Per_Thousand[[var]], 
       main = paste("Histogram of", var),
       xlab = var, col = "light blue", 
       border = "black", freq = FALSE)  # freq = FALSE makes it a density plot
  
  # Add density curve
  lines(density(Covid_Per_Thousand[[var]], na.rm = TRUE), col = "blue", lwd = 2)
  
  # Add standard normal distribution curve
  x_vals <- seq(min(Covid_Per_Thousand[[var]], na.rm = TRUE), 
                max(Covid_Per_Thousand[[var]], na.rm = TRUE), length = 100)
  curve_vals <- dnorm(x_vals, 
                      mean = mean(Covid_Per_Thousand[[var]], na.rm = TRUE), 
                      sd = sd(Covid_Per_Thousand[[var]], na.rm = TRUE))
  lines(x_vals, curve_vals, col = "red", lwd = 2, lty = 2)  # Standardization curve (red, dashed)
}

#------------------------------------------------
# Initialize an empty dataframe to store results
normality_results <- data.frame(Variable = character(), 
                                P_Value = numeric(), 
                                Normality = character(), 
                                stringsAsFactors = FALSE)

# Loop through all independent variables
for (var in independent_variables) {
  test_result <- shapiro.test(Covid_Per_Thousand[[var]])
  p_value <- test_result$p.value
  
  # Determine if the variable is normally distributed
  normality <- ifelse(p_value < 0.05, "Not Normal", "Normal")
  
  # Append the results for this variable
  normality_results <- rbind(normality_results, data.frame(
    Variable = var,
    P_Value = p_value,
    Normality = normality
  ))
}

# Remove duplicate entries (if necessary)
normality_results <- normality_results[!duplicated(normality_results), ]

# Print and view the results
print(normality_results)

#------------------------------------------------
# Subset Independent Variables
independent_vars <- Covid_Per_Thousand[independent_variables]

#------------------------------------------------
# Pearson Correlation Matrix
pearson_matrix <- cor(independent_vars, method = "pearson", use = "complete.obs")

# Display the Pearson correlation matrix
cat("Pearson Correlation Matrix:\n")
print(round(pearson_matrix, 2))

# Visualize the Pearson correlation matrix
windows(width = 10, height = 8)  # Set width and height in inches
corrplot(pearson_matrix, method = "color", type = "full", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black",  # Add correlation numbers in black
         number.cex = 0.7,  
         cl.pos = "r",   
         title = "Pearson Correlation Between Independent Variables",
         mar = c(0, 0, 1, 0))

#------------------------------------------------
# Spearman Correlation Matrix
spearman_matrix <- cor(independent_vars, method = "spearman", use = "complete.obs")

# Display the Spearman correlation matrix
cat("Spearman Correlation Matrix:\n")
print(round(spearman_matrix, 2))
# Full Heatmap with Spearman Correlation Values
windows(width = 10, height = 8)  # Set width and height in inches
corrplot(spearman_matrix, method = "color", type = "full", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black",  # Add correlation numbers in black
         number.cex = 0.7,       # Adjust size of the numbers
         cl.pos = "r",           # Add color legend to the right
         title = "Spearman Correlation Between Independent Variables",
         mar = c(0, 0, 1, 0))

# Print High Correlations
# Define a threshold for high correlations
threshold <- 0.7

cat("Highly Correlated Independent Variables (Pearson):\n")
high_corr_pearson <- which(abs(pearson_matrix) > threshold & abs(pearson_matrix) < 1, arr.ind = TRUE)
if (nrow(high_corr_pearson) > 0) {
  for (i in 1:nrow(high_corr_pearson)) {
    cat(rownames(pearson_matrix)[high_corr_pearson[i, "row"]], 
        "-", 
        colnames(pearson_matrix)[high_corr_pearson[i, "col"]],
        ": ", 
        pearson_matrix[high_corr_pearson[i, "row"], high_corr_pearson[i, "col"]], "\n")
  }
} else {
  cat("No high correlations found for Pearson method.\n")
}

cat("Highly Correlated Independent Variables (Spearman):\n")
high_corr_spearman <- which(abs(spearman_matrix) > threshold & abs(spearman_matrix) < 1, arr.ind = TRUE)
if (nrow(high_corr_spearman) > 0) {
  for (i in 1:nrow(high_corr_spearman)) {
    cat(rownames(spearman_matrix)[high_corr_spearman[i, "row"]], 
        "-", 
        colnames(spearman_matrix)[high_corr_spearman[i, "col"]],
        ": ", 
        spearman_matrix[high_corr_spearman[i, "row"], high_corr_spearman[i, "col"]], "\n")
  }
} else {
  cat("No high correlations found for Spearman method.\n")
}

#------------------------------------------------

# Correlation Matrices
Covid_Subset <- data.frame(Covid_Per_Thousand[[dependent_variable]], 
                           Covid_Per_Thousand[independent_variables])
colnames(Covid_Subset) <- c(dependent_variable, independent_variables)
# Compute the correlation matrix pearson
cor_matrix <- cor(Covid_Subset, use = "pairwise.complete.obs",method = "pearson")

# Display the correlation matrix in the console
print(round(cor_matrix,2))

#------------------------------------------------

#Multivariate Scatterplot Matrix
pairs(cor_matrix, 
      main = "Multivariate Scatterplot Matrix")

#------------------------------------------------
# windows(width = 10, height = 8)  # Set width and height in inches
# corrplot(cor_matrix,method = "color", type="full",t1.col="black",t1.srt=45)

windows(width = 10, height = 8)  # Set width and height in inches
corrplot(cor_matrix,method = "color", type="full",addCoef.col = "black",  # Add correlation numbers in black
         number.cex = 0.7,       # Adjust size of the numbers
         cl.pos = "r",           # Add color legend to the right
         title = "Pearson Correlation Between Dependent and Independent Variables",
         mar = c(0, 0, 1, 0))
#------------------------------------------------
library(corrgram)

# corrgram works best with Pearson correlation
corrgram(Covid_Subset, order=FALSE, cor.method = "pearson", lower.panel=panel.conf,
         upper.panel=panel.pie, text.panel=panel.txt, main="COVID Data Correlation Analysis")

#------------------------------------------------
# Print header
cat("Spearman Correlation Results of dependent variable with all independent variables:\n")

# Loop over variables and calculate Spearman correlation
for (var in independent_variables) {
  rho <- cor.test(Covid_Per_Thousand$Covid_Death, Covid_Per_Thousand[[var]], method = "spearman")$estimate
  cat(var, ":", rho, "\n")
}

#------------------------------------------------
#from the correlation diagram, select only those independent variables with high correlation
##Kids, Good_health,Fair_health,Bad_health,Low_Qualification,Work_from_Home,Long_distance,High_Qualification
library(corrgram)
Covid_High_correlated <- data.frame(
  Kids = Covid_Per_Thousand$Kids,
  Good_Health = Covid_Per_Thousand$Good_health,
  Fair_health = Covid_Per_Thousand$Fair_health,
  Bad_Health = Covid_Per_Thousand$Bad_health,
  Low_Qualification = Covid_Per_Thousand$Low_Qualification,
  Work_from_Home = Covid_Per_Thousand$Work_from_Home,
  Long_distance = Covid_Per_Thousand$Long_distance,
  High_Qualification = Covid_Per_Thousand$High_Qualification
)

# corrgram works best with Pearson correlation
corrgram(Covid_High_correlated, order=FALSE, cor.method = "pearson", lower.panel=panel.conf,
         upper.panel=panel.pie, text.panel=panel.txt, main="Selected independent variables (retaining only high correaltion)")

# Display the correlation matrix in the console
cor_matrix_High<- cor(Covid_High_correlated, use = "pairwise.complete.obs",method = "pearson")
print(round(cor_matrix_High,2))
#------------------------------------------------
# # Assuming `normality_results` dataframe contains the normality test results for independent variables
# normal_vars <- normality_results$Variable[normality_results$Normality == "Normal"]
# 
# # Visualization: Scatterplots with correlation labels
# par(mfrow = c(3, 3))  
# 
# for (var in independent_variables) {
#   plot(Covid_Per_Thousand[[var]], Covid_Per_Thousand[[dependent_variable]], 
#        xlab = var, ylab = dependent_variable, 
#        main = paste("Scatterplot of", dependent_variable, "vs", var), 
#        col = "blue", pch = 19)
#   abline(lm(Covid_Per_Thousand[[dependent_variable]] ~ Covid_Per_Thousand[[var]]), col = "red", lwd = 2)
#   corr_method <- ifelse(var %in% normal_vars, "pearson", "spearman")
#   corr_value <- cor(Covid_Per_Thousand[[dependent_variable]], Covid_Per_Thousand[[var]], 
#                     method = corr_method, use = "complete.obs")
#   legend("topleft", legend = paste("Corr:", round(corr_value, 2)), bty = "n")
# }
#------------------------------------------------
##Hypothesis testing
# Set up the plotting area for a 3x3 grid (3 rows, 3 columns)
par(mfrow = c(3, 3))
# 1. Kids
# Null Hypothesis (H0): There is no correlation between 'Covid_Death' and 'Kids'.
# Alternative Hypothesis (H1): There is a correlation between 'Covid_Death' and 'Kids'.
cor_test_result_kids <- cor.test(Covid_Per_Thousand$Covid_Death, Covid_Per_Thousand$Kids)
print(cor_test_result_kids)
plot(Covid_Per_Thousand$Kids, Covid_Per_Thousand$Covid_Death, 
     xlab = "Kids", ylab = "Death Number", 
     main = "Scatter Plot: Death Number vs Kids")
abline(lm(Covid_Death ~ Kids, data = Covid_Per_Thousand), col = "red")

# 2. Good Health
# Null Hypothesis (H0): There is no correlation between 'Covid_Death' and 'Good_Health'.
# Alternative Hypothesis (H1): There is a correlation between 'Covid_Death' and 'Good_Health'.
cor_test_result_good_health <- cor.test(Covid_Per_Thousand$Covid_Death, Covid_Per_Thousand$Good_health)
print(cor_test_result_good_health)
plot(Covid_Per_Thousand$Good_health, Covid_Per_Thousand$Covid_Death, 
     xlab = "Good Health", ylab = "Death Number", 
     main = "Scatter Plot: Death Number vs Good Health")
abline(lm(Covid_Death ~ Good_health, data = Covid_Per_Thousand), col = "red")

# 3. Fair Health
# Null Hypothesis (H0): There is no correlation between 'Covid_Death' and 'Fair_Health'.
# Alternative Hypothesis (H1): There is a correlation between 'Covid_Death' and 'Fair_Health'.
cor_test_result_fair_health <- cor.test(Covid_Per_Thousand$Covid_Death, Covid_Per_Thousand$Fair_health)
print(cor_test_result_fair_health)
plot(Covid_Per_Thousand$Fair_health, Covid_Per_Thousand$Covid_Death, 
     xlab = "Fair Health", ylab = "Death Number", 
     main = "Scatter Plot: Death Number vs Fair Health")
abline(lm(Covid_Death ~ Fair_health, data = Covid_Per_Thousand), col = "red")

# 4. Bad Health
# Null Hypothesis (H0): There is no correlation between 'Covid_Death' and 'Bad_Health'.
# Alternative Hypothesis (H1): There is a correlation between 'Covid_Death' and 'Bad_Health'.
cor_test_result_bad_health <- cor.test(Covid_Per_Thousand$Covid_Death, Covid_Per_Thousand$Bad_health)
print(cor_test_result_bad_health)
plot(Covid_Per_Thousand$Bad_health, Covid_Per_Thousand$Covid_Death, 
     xlab = "Bad Health", ylab = "Death Number", 
     main = "Scatter Plot: Death Number vs Bad Health")
abline(lm(Covid_Death ~ Bad_health, data = Covid_Per_Thousand), col = "red")

# 5. Low Qualification
# Null Hypothesis (H0): There is no correlation between 'Covid_Death' and 'Low_Qualification'.
# Alternative Hypothesis (H1): There is a correlation between 'Covid_Death' and 'Low_Qualification'.
cor_test_result_low_qualification <- cor.test(Covid_Per_Thousand$Covid_Death, Covid_Per_Thousand$Low_Qualification)
print(cor_test_result_low_qualification)
plot(Covid_Per_Thousand$Low_Qualification, Covid_Per_Thousand$Covid_Death, 
     xlab = "Low Qualification", ylab = "Death Number", 
     main = "Scatter Plot: Death Number vs Low Qualification")
abline(lm(Covid_Death ~ Low_Qualification, data = Covid_Per_Thousand), col = "red")

# 6. Work from Home
# Null Hypothesis (H0): There is no correlation between 'Covid_Death' and 'Work_from_Home'.
# Alternative Hypothesis (H1): There is a correlation between 'Covid_Death' and 'Work_from_Home'.
cor_test_result_work_from_home <- cor.test(Covid_Per_Thousand$Covid_Death, Covid_Per_Thousand$Work_from_Home)
print(cor_test_result_work_from_home)
plot(Covid_Per_Thousand$Work_from_Home, Covid_Per_Thousand$Covid_Death, 
     xlab = "Work from Home", ylab = "Death Number", 
     main = "Scatter Plot: Death Number vs Work from Home")
abline(lm(Covid_Death ~ Work_from_Home, data = Covid_Per_Thousand), col = "red")

# 7. Long Distance
# Null Hypothesis (H0): There is no correlation between 'Covid_Death' and 'Long_Distance'.
# Alternative Hypothesis (H1): There is a correlation between 'Covid_Death' and 'Long_Distance'.
cor_test_result_long_distance <- cor.test(Covid_Per_Thousand$Covid_Death, Covid_Per_Thousand$Long_distance)
print(cor_test_result_long_distance)
plot(Covid_Per_Thousand$Long_distance, Covid_Per_Thousand$Covid_Death, 
     xlab = "Long Distance", ylab = "Death Number", 
     main = "Scatter Plot: Death Number vs Long Distance")
abline(lm(Covid_Death ~ Long_distance, data = Covid_Per_Thousand), col = "red")

# 8. High Qualification
# Null Hypothesis (H0): There is no correlation between 'Covid_Death' and 'High_Qualification'.
# Alternative Hypothesis (H1): There is a correlation between 'Covid_Death' and 'High_Qualification'.
cor_test_result_high_qualification <- cor.test(Covid_Per_Thousand$Covid_Death, Covid_Per_Thousand$High_Qualification)
print(cor_test_result_high_qualification)
plot(Covid_Per_Thousand$High_Qualification, Covid_Per_Thousand$Covid_Death, 
     xlab = "High Qualification", ylab = "Death Number", 
     main = "Scatter Plot: Death Number vs High Qualification")
abline(lm(Covid_Death ~ High_Qualification, data = Covid_Per_Thousand), col = "red")


# Reset the plotting area back to default single plot
par(mfrow = c(1, 1))

#------------------------------------------------
# #partial correlation 
library(ppcor)
# Perform partial correlation for Covid_Death vs Adults, controlling for Kids
pcor.test(Covid_Per_Thousand$Covid_Death, Covid_Per_Thousand$Adults, Covid_Per_Thousand$Kids, method = "pearson")
# Perform partial correlation for Covid_Death vs Kids, controlling for adult
pcor.test(Covid_Per_Thousand$Covid_Death, Covid_Per_Thousand$Kids, Covid_Per_Thousand$Adults, method = "pearson")
## Retain Kids 

#Perform partial correlation for Covid_Death vs High_Qualification, controlling for Bad_health:
pcor.test(Covid_Per_Thousand$Covid_Death, Covid_Per_Thousand$High_Qualification, Covid_Per_Thousand$Bad_health, method = "pearson")
#Perform partial correlation for Covid_Death vs Bad_health, controlling for High_Qualification:
pcor.test(Covid_Per_Thousand$Covid_Death, Covid_Per_Thousand$Bad_health, Covid_Per_Thousand$High_Qualification, method = "pearson")
##Retain Bad_health

#Perform partial correlation for Covid_Death vs Bad_health, controlling for Work from home :
pcor.test(Covid_Per_Thousand$Covid_Death, Covid_Per_Thousand$Bad_health, Covid_Per_Thousand$Work_from_Home, method = "pearson")
#Perform partial correlation for Covid_Death vs Work from home , controlling for Bad_health :
pcor.test(Covid_Per_Thousand$Covid_Death, Covid_Per_Thousand$Work_from_Home, Covid_Per_Thousand$Bad_health, method = "pearson")
##retain Bad_health 

#Perform partial correlation for Covid_Death vs Fair_health, controlling for Good_health :
pcor.test(Covid_Per_Thousand$Covid_Death, Covid_Per_Thousand$Fair_health, Covid_Per_Thousand$Good_health, method = "pearson")
#Perform partial correlation for Covid_Death vs Good_health , controlling for Fair_health :
pcor.test(Covid_Per_Thousand$Covid_Death, Covid_Per_Thousand$Good_health, Covid_Per_Thousand$Fair_health, method = "pearson")
##retain both due to its statistical significance.

#Perform partial correlation for Covid_Death vs Low_Qualification, controlling for Long_distance :
pcor.test(Covid_Per_Thousand$Covid_Death, Covid_Per_Thousand$Low_Qualification, Covid_Per_Thousand$Long_distance, method = "pearson")
#Perform partial correlation for Covid_Death vs Long_distance , controlling for Low_Qualification :
pcor.test(Covid_Per_Thousand$Covid_Death, Covid_Per_Thousand$Long_distance, Covid_Per_Thousand$Low_Qualification, method = "pearson")
##retain Low_Qualification due to its statistical significance.

#------------------------------------------------
#kids, Bad_health, Fair_health,Good_health,Low_Qualification
#------------------------------------------------

# Select variables by excluding those not required; the %in% operator means 'matching'
# Update the variable list to exclude specific columns as needed
myvars <- names(Covid_Per_Thousand) %in% c("LA_name", "LA_Code", "Male", "Female","Adults","Old","Medium_Qualification","Long_distance","Unshared_dwell","Covid_Death","Shared_dwell","Short_distance","Medium_distance","Work_from_Home","Other_Location")
# The ! operator means NOT
Covid_selected <- Covid_Per_Thousand[!myvars]  # Exclude unwanted columns
str(Covid_selected)  # Check structure of the new dataset
rm(myvars)  # Clean up the variable used for filtering

#-----Section: Kaiser-Meyer-Olkin (KMO) Test-----------------------------

# Install and load the psych package if not already installed
library(psych)

# Compute the KMO statistics: Use correlation matrix of the selected variables
kmo_result <- KMO(cor(Covid_selected))  # Compute KMO test on correlation matrix
cat("KMO Test Result:\n")
print(kmo_result)  # Print the overall KMO and individual MSA values

# Interpretation:
# If overall KMO > 0.6, proceed to factor analysis.

#------------------------------------------------
#------------------- FACTOR ANALYSIS-------------------
# Load required libraries
library(nFactors)

# Compute correlation matrix of your dataset
cor_matrix <- cor(Covid_selected, use = "complete.obs")

# Eigenvalues from the correlation matrix
eigen_values <- eigen(cor_matrix)$values
print(eigen_values)  # Display eigenvalues

# Scree Plot of Eigenvalues
plot(eigen_values, type = "b", col = "blue", xlab = "Factors",
     ylab = "Eigenvalues", main = "Scree Plot")

# Calculate and Plot Cumulative Proportion of Variance Explained
# Step 1: Normalize eigenvalues (proportion of variance)
proportion_variance <- eigen_values / sum(eigen_values)

# Step 2: Compute cumulative proportion of variance explained
cumulative_variance <- cumsum(proportion_variance)

# Step 3: Plot cumulative proportion
plot(cumulative_variance, type = "b", col = "red", xlab = "Number of Components",
     ylab = "Cumulative Proportion", main = "Cumulative Variance Explained")

# Add a threshold line (optional, e.g., 80% variance explained)
abline(h = 0.8, col = "green", lty = 2)  # Dashed line at 80% variance

#------------------------------------------------
# Varimax Rotated Principal Components
# retaining 'nFactors' components
library(GPArotation)

fit <- principal(Covid_selected, nfactors = 2, rotate = "varimax")
cat("Factor Analysis Results:\n")
fit

# RC1 - Bad_health, Low_Qualification-> Bad_health
# RC2 - Kids

# create four variables to represent the rorated components
fit$scores
fit.data <- data.frame(fit$scores)

# check new variables are uncorrelated
cor.matrix2 <-cor(fit.data, method = "spearman")
cor.df2 <- as.data.frame(cor.matrix2)
round(cor.df2, 2)

# Extract eigenvalues
eig_values <- fit$values  # Eigenvalues from `principal` function

# Create Scree Plot
scree_data <- data.frame(Component = 1:length(eig_values), Eigenvalue = eig_values)

library(ggplot2)
ggplot(scree_data, aes(x = Component, y = Eigenvalue)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = round(Eigenvalue, 2)), vjust = -0.5) +
  labs(title = "Scree Plot for PCA", x = "Component", y = "Eigenvalue") +
  theme_minimal()


#-----------------------------------------------------------------

#------ LINEAR REGRESSION-----------------------------------------
library(car)
# Model 1: Start with all variables
# model1 <- lm(Covid_Death ~ Old+Fair_health+Low_Qualification+Medium_distance+Other_Location, data = Covid_Per_Thousand)
model1 <- lm(Covid_Death ~ Male+Female+Kids+Adults+Good_health+Fair_health+Bad_health+Low_Qualification+High_Qualification+Shared_dwell+Unshared_dwell+Short_distance+Medium_distance+Long_distance+Work_from_Home+Other_Location, data = Covid_Per_Thousand)
summary(model1)

hist(model1$residuals)
rug(model1$residuals)

# consider normality of residuals
plot(model1$residuals ~ model1$fitted.values, xlab = "Model1", ylab = "residuals", main = "Actual vs Predicted")
ks.test(model1$residuals, "pnorm", mean(model1$residuals), sd(model1$residuals))


plot(model1,which=2, xlab = "Model 1")


sqrt(vif(model1)) > 2

#-----------------------------------------------------------------
########### VARIABLES AFTER CORRELATION (BEFORE PCOR)
############## AFTER RETAINING THE HIGH CORRELATIONS 
#Kids, Good_health,Fair_health,Bad_health,Low_Qualification,Work_from_Home,Long_distance,High_Qualification

# Model 2: Reduced model
# Simplified regression model
model2 <- lm(Covid_Death ~ Kids + Good_health +
               Fair_health + Bad_health + Low_Qualification + Work_from_Home+Long_distance+High_Qualification,
                       data = Covid_Per_Thousand)

summary(model2)


hist(model2$residuals)
rug(model2$residuals)
# consider normality of residuals
plot(model2$residuals ~ model2$fitted.values, xlab = "Model2", ylab = "residuals", main = "Actual vs Predicted")
ks.test(model2$residuals, "pnorm", mean(model2$residuals), sd(model2$residuals))



plot(model2,which=2, xlab = "Model 2")

sqrt(vif(model2)) > 2

#------------------------------------------------

#-----AFTER PCOR(kids, Bad_health,Good_health,Low_Qualification)--------------------------

model3 <- lm(Covid_Death ~ Kids + Bad_health +
                 Low_Qualification + Good_health,
             data = Covid_Per_Thousand)

summary(model3)


hist(model3$residuals)
rug(model3$residuals)
# consider normality of residuals
plot(model3$residuals ~ model3$fitted.values, xlab = "Model3", ylab = "residuals", main = "Actual vs Predicted")
ks.test(model3$residuals, "pnorm", mean(model3$residuals), sd(model3$residuals))



plot(model3,which=2, xlab = "Model 3")

sqrt(vif(model3)) > 2

#------------------------------------------------

# Model using factors derived from PCA
# Assuming RC1, RC2are derived from PCA
fit.data$Covid_Death <- Covid_Per_Thousand$Covid_Death
model4 <- lm(Covid_Death ~ RC1 + RC2 , data = fit.data)
summary(model4)

hist(model4$residuals)
rug(model4$residuals)
plotNormalHistogram(model4$residuals)
# consider normality of residuals
plot(model4$residuals ~ model4$fitted.values, xlab = "fitted values", ylab = "residuals")
ks.test(model4$residuals, "pnorm", mean(model4$residuals), sd(model4$residuals))

plot(model4,which=2)


sqrt(vif(model4)) > 2

#-------------------ANOVA TESTS-----------------

anova(model1, model2, model3,model4, test = "F")

