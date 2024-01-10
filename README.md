# ECON 105 RESEARCH METHOD WITH COMPUTER APPLICATIONS

Presented by Layla Kayi in paper titled **"In-depth Statistical Analysis for Investigating the Socio-Economic Determinants of Child Mortality."**

## Overview
This R script provides an extensive suite of data analysis tools, encompassing the following key functionalities:

1. **Data Cleaning and Imputation:**
   - Handles missing values by imputing them with the median of each column.
   - Ensures data consistency by replacing commas with periods to address potential decimal formatting issues.

2. **Outlier Analysis:**
   - Identifies outliers in the dataset using the Interquartile Range (IQR) method.
   - Offers flexibility for exploring alternative outlier detection methods, such as Z-score or clustering.

3. **Descriptive Statistics:**
   - Calculates mean, standard deviation, median, mode, geometric mean, and harmonic mean for each column.
   - Addresses potential decimal formatting issues in the dataset to ensure consistency.

4. **Skewness and Kurtosis:**
   - Utilizes the `e1071` package to compute skewness and kurtosis for each variable.

5. **Correlation Matrix:**
   - Generates a correlation matrix to examine the relationships between numeric variables.
   - Presents the matrix in a tabular format for easy interpretation.

6. **Histogram Visualization:**
   - Creates histograms for each numeric variable to visualize the distribution of data.
   - Saves the histogram images in the `histogram_output` folder.

7. **ANOVA Analysis:**
   - Performs Analysis of Variance (ANOVA) to investigate the impact of various variables on child mortality.
   - Presents ANOVA results in an HTML table (`anova_table.html`).

8. **Scatterplots and Correlation Visualization:**
   - Generates scatterplots between child mortality and other variables.
   - Calculates and displays correlation coefficients for better insights.

9. **Linear Regression Modeling:**
   - Constructs a linear regression model to explore relationships between predictor variables and child mortality.
   - Outputs regression coefficients, hypothesis test results, and a regression equation.

## Features
- **Modular Code:** The script is organized into functions and sections for clarity and maintainability.
- **Visualization:** Histograms and scatterplots are generated for a visual representation of the data.
- **ANOVA Analysis:** The script performs ANOVA analysis to examine the impact of various variables on child mortality.
- **Linear Regression:** A linear regression model is built to explore relationships between variables.

## Dependencies
The script relies on the following R packages. Make sure to install them before running the script.
- `openxlsx`
- `readr`
- `modeest`
- `psych`
- `e1071`
- `xtable`
- `stargazer`

You can install them using the following:
```R
install.packages(c("openxlsx", "readr", "modeest", "psych", "e1071", "xtable", "stargazer"))
```
## Output
Our findings are reported on our paper in detail. PDF will be added to this repo when approved.

## License
This project is licensed under the MIT License - the authors do NOT give permission for any part of this code to be copied and presented under this or related proejcts.
