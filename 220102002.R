#####
# Author: Layla Kayi
# Student Num: 220102002
# Assignment: ECON 105 RESEARCH METHOD WITH COMPUTER APPLICATIONS
# Title: In-depth Statistical Analysis for Investigating the Socio-Economic Determinants of Child Mortality.
# 2024
#####

####
# This code can be mirrored from GitHub at:
# github.com/xumiqun/OstimEcon105
# using any parts of this code in your project is NOT allowed
####



########################################################################
# We must first set the options for our output / formatting

# We start by including the necessary libraries
# if you don't have them you can install it by un-commenting the next line
# install.packages(c("openxlsx", "readr", "modeest", "psych", "e1071", "xtable", "stargazer"))
library(openxlsx)
library(readr)
library(modeest)
library(psych)
library(e1071)
library(xtable)
library(stargazer)

# I will be reading the dataset with read.csv
# Because my "utils" package only contains this function
# As opposed to introductions provided by tutor at "R_1-9_revised.docx"

# The original CSV file was renamed to this
file_path <- "veri_seti.csv"

# Delimeter is encoded with ; on our CSV
data <- read.csv(file_path, header = TRUE, sep = ";")

cat("Read the CSV file, now proceeding with the descriptive statistics")
cat("\n--------------------------\n")

########################################################################

###### Descriptive Statistics ######


# Handling Missing Values
# The initial approach involves imputing missing values with the mean of each column.
# I will use median for imputation
# Consideration: Alternative imputation methods like median or regression could be explored.
missing_values <- sum(is.na(data))
print(paste("Total number of missing values is:", missing_values))

# Technique for filling the missing values
data_no_na <- data
for (col in colnames(data)) {
  if (any(is.na(data[[col]]))) {
    mean_value <- mean(as.numeric(gsub(",", ".", data[[col]]), na.rm = TRUE), na.rm = TRUE)
    data_no_na[[col]][is.na(data_no_na[[col]])] <- mean_value
  }
}

# Outlier Analysis
# The IQR method is used to identify outliers. Consideration: More advanced techniques
# like Z-score or clustering could enhance outlier detection robustness.
# Because when dataset is downloaded some problems occur with decimal points...
# ... we use gsub()
outliers <- sapply(data_no_na, function(x) {
  IQR <- quantile(as.numeric(gsub(",", ".", x), na.rm = TRUE), 0.75) - quantile(as.numeric(gsub(",", ".", x), na.rm = TRUE), 0.25)
  lower_limit <- quantile(as.numeric(gsub(",", ".", x), na.rm = TRUE), 0.25) - 1.5 * IQR
  upper_limit <- quantile(as.numeric(gsub(",", ".", x), na.rm = TRUE), 0.75) + 1.5 * IQR
  outliers <- x < lower_limit | x > upper_limit
  sum(outliers)
})

print("Number of outliers is:")
print(outliers)

cat("Overview of the data:\n")
head(data)       # Print the first rows of the data
cat("Info regarding the columns:\n")
summary(data)    # Get the summary of data (encoding, quarterly values, max and min values)

# Now get the mean and the SD for each column
# Assuming your data is a data frame named 'data'
# Function to calculate mode
get_mode <- function(x) {
  tbl <- table(x)
  modes <- as.numeric(names(tbl[tbl == max(tbl)]))
  if (length(modes) > 1) {
    return("No unique mode")
  } else {
    return(modes)
  }
}

# Function to calculate geometric mean with log transformation
get_geometric_mean <- function(x) {
  non_na_values <- as.numeric(x[!is.na(x)])
  
  # If there are no non-na values, return NA
  if (length(non_na_values) == 0) {
    return(NA)
  }
  
  # Use abs() to prevent NaNs and add a small value to avoid log(0)
  exp(mean(log(abs(non_na_values) + 1e-10)))
}

# Function to calculate harmonic mean
get_harmonic_mean <- function(x) {
  non_zero_values <- as.numeric(x[x != 0 & !is.na(x)])
  1 / mean(1 / non_zero_values)
}

# Now calculate descriptive stats
# IDK why, but the CSV decimals are kinda weird
# Someone mistaked a comma with a dot
# That's probably a mistake because of the local language changes
# I could fix it with code but decided to use gsub for flexibility
# Later in the code, it became apparent that there is a decimal formatting issue in the data,
# where commas are used instead of periods. This issue is addressed to ensure consistency.
# For now this works
descriptive_stats <- sapply(data, function(x) c(
  mean = mean(as.numeric(gsub(",", ".", x), na.rm = TRUE)),
  sd = sd(as.numeric(gsub(",", ".", x), na.rm = TRUE)),
  median = median(as.numeric(gsub(",", ".", x), na.rm = TRUE)),
  mode = get_mode(as.numeric(gsub(",", ".", x), na.rm = TRUE)),
  geometric_mean = get_geometric_mean(as.numeric(gsub(",", ".", x))),
  harmonic_mean = get_harmonic_mean(as.numeric(gsub(",", ".", x)))
))

# And... print descriptive statistics in a table format
stats_table <- as.data.frame(t(descriptive_stats))
colnames(stats_table) <- c("Mean", "SD", "Median", "Mode", "Geometric Mean", "Harmonic Mean")
print(stats_table)

# Function to calculate the Skewness
# Well, I could calculate this without a dependency with the formula:
# ∑^N(i= (Xi – X)^3 / (N-1) * σ^3
# But this package handles the numerics for me
# It is opensource and it seems like it checks the number of arguments
# And then calculate the numerics in O(1) and then return
# By default skewness uses the following formula:
# y <- sqrt(n) * sum(x ^ 3) / (sum(x ^ 2) ^ (3/2))
# y <- y * ((1 - 1 / n)) ^ (3/2)
# And then returned
get_skew <- function(x) {
  e1071::skewness(as.numeric(gsub(",", ".", x), na.rm = TRUE))
}

# And the Kurtosis from e1071
get_kurtosis <- function(x) {
  e1071::kurtosis(as.numeric(gsub(",", ".", x), na.rm = TRUE))
}

# Calculate the skewness and kurtosis
skewness_values <- sapply(data, get_skew)
kurtosis_values <- sapply(data, get_kurtosis)

# Output them
skewness_table <- data.frame(Skewness = skewness_values)
kurtosis_table <- data.frame(Kurtosis = kurtosis_values)

print("Skewness table:")
print(skewness_table)

print("Kurtosis table:")
print(kurtosis_table)


# At this point I have decided that it's better to just fix the decimal thing
# Replace commas with periods in the entire dataset
# And then print the correlation matrix
data_numeric <- lapply(data, function(x) as.numeric(gsub(",", ".", x)))
data_numeric <- as.data.frame(data_numeric)
numerical_data <- data_numeric[sapply(data_numeric, is.numeric)]
correlation_matrix <- cor(numerical_data)
print("Correlation Matrix:")
print(correlation_matrix)


# Show the distribution of the data
data_numeric <- data
for (col in colnames(data_numeric)) {
  data_numeric[[col]] <- as.numeric(gsub(",", ".", data[[col]]))
}

# Here I slice the data
# And then print them to terminal
for (col in colnames(data_numeric)) {
  if (is.numeric(data_numeric[[col]])) {
    breaks <- seq(min(data_numeric[[col]], na.rm = TRUE), max(data_numeric[[col]], na.rm = TRUE), length.out = 16)
    
    # Calculate histogram
    hist_data <- hist(data_numeric[[col]], breaks = breaks, plot = FALSE)
    
    # Calculate each slice
    percentage_freq <- prop.table(hist_data$counts) * 100
    
    # Print percentiles in each range and the ranges of values it covers
    cat("Variable:", col, "\n")
    for (i in 1:(length(percentage_freq) - 1)) {
      interval_values <- sprintf("%.2f - %.2f", breaks[i], breaks[i + 1])
      cat(sprintf("Interval %d (%s): %.2f%%\n", i, interval_values, percentage_freq[i]))
    }
    cat(sprintf("Interval %d (%.2f - %.2f): %.2f%%\n", length(percentage_freq), breaks[length(breaks) - 1], breaks[length(breaks)], percentage_freq[length(percentage_freq)]))
    cat("\n--------------------------\n")
  }
}

# Output them as JPG
output_folder <- "histogram_output"
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}

# Translate to numeric data once again
data_numeric <- data
for (col in colnames(data_numeric)) {
  data_numeric[[col]] <- as.numeric(gsub(",", ".", data[[col]]))
}

# Save them as JPG
# I have already printed them but I will also get image outputs
for (col in colnames(data_numeric)) {
  if (is.numeric(data_numeric[[col]])) {
    breaks <- seq(min(data_numeric[[col]], na.rm = TRUE), max(data_numeric[[col]], na.rm = TRUE), length.out = 16)
    
    hist_data <- hist(data_numeric[[col]], breaks = breaks, plot = FALSE)
    
    # Calculate the percentiles for each slice I have made
    percentage_freq <- prop.table(hist_data$counts) * 100
    
    jpeg(paste0(output_folder, "/", col, "_histogram.jpg"), width = 800, height = 600, quality = 100, bg = "white")
    par(mar = c(5, 4, 4, 4))
    barplot(height = percentage_freq, names.arg = seq(1, length(percentage_freq)), col = "red", border = "brown", main = paste("Histogram of", col), xlab = "Interval", ylab = "Percentage", ylim = c(0, max(percentage_freq) + 5))
    dev.off()
  }
}
##########################################################################################################
# Now I'll be starting the section for the ANOVA


# We start by creatin our ANOVA model
anova_model <- aov(child_mort ~ exports + health + imports + income + inflation + life_expec + birth_rate + gdpp, data = data_numeric)
anova_results <- summary(anova_model)

# Save them to dataframe
anova_df <- data.frame(anova_results[[1]])

# Create HTML table
html_table <- xtable::xtable(anova_df, caption = "ANOVA Results")

# Save the HTML table
cat(print(html_table, type = "html"), file = "anova_table.html")

# I now wanna create a two variable graph to visualize correlation
# We need this to not make plot values scientific
options(scipen = 999)

# A custom function, nothing to worry about
p0 <- function(x) {
  paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}

# I create plots between child mortality and other variables so that we can observe better
# I save them to a dir called "plots"
folder_path <- "plots"

# Function to create scatterplot and calculate correlation
create_scatterplot <- function(x_variable, y_variable, xlab, ylab, main) {
  png(file.path(folder_path, paste("scatterplot_", x_variable, "_child_mort.png", sep = "")), width = 800, height = 600)
  plot(data_numeric[[x_variable]], data_numeric$child_mort, xlab = xlab, ylab = ylab, main = main)
  dev.off()
  
  # Correlation calculation
  cor_value <- cor(data_numeric[[x_variable]], data_numeric$child_mort)
  cat("Correlation between", x_variable, "and Child Mortality:", cor_value, "\n")
}

# Iterate over each variable
variables <- c("exports", "health", "imports", "income", "inflation", "life_expec", "birth_rate", "gdpp")

# Create scatterplots and calculate correlations for each variable
for (variable in variables) {
  create_scatterplot(variable, "child_mort", paste(p0(variable), "Value"), "Child Mortality", 
                     paste("Scatterplot:", p0(variable), "vs Child Mortality"))
}

# HTML correlation matrix table
html_table_cor <- print(xtable(correlation_matrix), type = "html", print.results = FALSE)
cat(html_table_cor, file = "cor_matrix.html")

# Regression model
reg_model <- lm(child_mort ~ exports + health + imports + income + inflation + life_expec + birth_rate + gdpp, data = data_numeric)

# Hypothesis test
p_values <- summary(reg_model)$coefficients[, "Pr(>|t|)"]
hypothesis_test <- ifelse(p_values < 0.05, "Reject H0", "Fail to reject H0")

coefficients <- coef(reg_model)[-1]  # Intercept hariç katsayılar
variable_names <- names(coefficients)

# R^2 and regression eq
r_squared <- summary(reg_model)$r.squared
regression_equation <- paste("Child Mortality =", paste(coefficients, variable_names, collapse = " + "), "\n")

# Call stargazer
cleaned_covariate_labels <- c('Intercept', 'Exports', 'Health', 'Imports', 'Income', 'Inflation', 'Life Expectancy', 'Birth Rate', 'GDP')
stargazer(reg_model, type = 'html', out = 'regression_table.html', covariate.labels = cleaned_covariate_labels, dep.var.labels = 'Child Mortality', na.rm = TRUE)

# Just for testing I call the ANOVA model here once again
# Because when stargazer is used some values are rounded
# Therefore this can play as a second check
summary(anova_model)

# Now let's print the regression equation here
cat("Regression Equation:\n")
cat(regression_equation, "\n\n")


################################  END OF CODE ################################

