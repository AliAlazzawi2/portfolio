setwd("C:\\Users\\ali_a\\OneDrive\\Desktop\\TEST\\Eamon Express")
list.files()
# Step 1: Load Required Libraries
library(readr)
library(dplyr)

# Step 2: Define the file path
# (Make sure to use forward slashes or escape backslashes)
file_path <- "C:/Users/ali_a/OneDrive/Desktop/TEST/Eamon Express/closed-invoices-11-05-2025.CSV"
getwd()
# Step 3: Read the CSV
invoices <- read_csv(file_path)

# Step 4: View structure and basic info
glimpse(invoices)

# Step 5: Check first few rows
head(invoices)

# Step 6: Basic summary (similar to Python's describe)
summary(invoices)
library(readr)

invoices <- read_csv("C:/Users/ali_a/OneDrive/Desktop/TEST/Eamon Express/closed-invoices-11-05-2025.CSV")
# Step 1: Install the tidyverse (only once)
install.packages("tidyverse")

# Step 2: Load the library
library(readr)

# Step 3: Now read your file
invoices <- read_csv("C:/Users/ali_a/OneDrive/Desktop/TEST/Eamon Express/closed-invoices-11-05-2025.CSV")

# Step 4: Check first few rows
head(invoices)
invoices <- read.csv("C:/Users/ali_a/OneDrive/Desktop/TEST/Eamon Express/closed-invoices-11-05-2025.CSV")
head(invoices)
# Load libraries
library(dplyr)
library(lubridate)

# Step 1: Select the main columns we'll use for EDA
eda_df <- invoices %>%
  select(
    `Invoice Number`,
    `Invoice Date`,
    `Original Amount Due`,
    `Current Balance`,
    Payor,
    `Express or Ground Tracking ID`
  )

# Step 2: Convert the Invoice Date to proper Date format
# (Your dataset uses YYYYMMDD style dates, e.g. 20250424)
eda_df <- eda_df %>%
  mutate(`Invoice Date` = ymd(`Invoice Date`))

# Step 3: Check for missing values
colSums(is.na(eda_df))

# Step 4: Check for duplicates (based on Invoice Number)
sum(duplicated(eda_df$`Invoice Number`))

# Step 5: Quick summary to verify cleaning
summary(eda_df)

# Step 6: View the first few rows
head(eda_df)
names(invoices)
library(dplyr)
library(lubridate)

# Step 1: Select the main columns (matching your exact names)
eda_df <- invoices %>%
  select(
    Invoice.Number,
    Invoice.Date,
    Original.Amount.Due,
    Current.Balance,
    Payor,
    Express.or.Ground.Tracking.ID
  )

# Step 2: Convert the Invoice.Date to a proper Date format (YYYYMMDD → Date)
eda_df <- eda_df %>%
  mutate(Invoice.Date = ymd(Invoice.Date))

# Step 3: Check for missing values
colSums(is.na(eda_df))

# Step 4: Check for duplicate invoice numbers
sum(duplicated(eda_df$Invoice.Number))

# Step 5: Summarize numeric + categorical variables
summary(eda_df)

# Step 6: Preview the cleaned data
head(eda_df)
# Load visualization library
library(ggplot2)

# 1️⃣ Histogram of invoice amounts
ggplot(eda_df, aes(x = Original.Amount.Due)) +
  geom_histogram(binwidth = 25, fill = "steelblue", color = "black") +
  labs(
    title = "Distribution of Invoice Amounts (Eamon Express Closed Invoices)",
    x = "Invoice Amount (USD)",
    y = "Number of Invoices"
  ) +
  theme_minimal()

# 2️⃣ Boxplot – to detect outliers
ggplot(eda_df, aes(y = Original.Amount.Due)) +
  geom_boxplot(fill = "orange", color = "black", width = 0.3) +
  labs(
    title = "Invoice Amount Spread & Outliers",
    y = "Invoice Amount (USD)"
  ) +
  theme_minimal()

# 3️⃣ Time-series trend of invoice amounts
ggplot(eda_df, aes(x = Invoice.Date, y = Original.Amount.Due)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  geom_point(color = "darkred", size = 2) +
  labs(
    title = "Invoice Amounts Over Time",
    x = "Invoice Date",
    y = "Invoice Amount (USD)"
  ) +
  theme_minimal()

# 4️⃣ Bar plot of total amount by Payor (if you have more than one)
ggplot(eda_df, aes(x = Payor, y = Original.Amount.Due, fill = Payor)) +
  geom_col() +
  labs(
    title = "Total Invoice Amount by Payor",
    x = "Payor",
    y = "Total Amount (USD)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# 5️⃣ Optional – basic correlation plot (Amount vs Balance)
ggplot(eda_df, aes(x = Original.Amount.Due, y = Current.Balance)) +
  geom_point(color = "purple", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 1) +
  labs(
    title = "Correlation Between Invoice Amount and Balance",
    x = "Invoice Amount (USD)",
    y = "Current Balance (USD)"
  ) +
  theme_minimal()
# Load tidyverse helper packages
library(dplyr)
library(psych)     # for describe()
library(corrplot)  # for correlation visualization

# 1️⃣ Summary statistics table for numeric variables
describe(eda_df %>% select(Original.Amount.Due, Current.Balance))

# 2️⃣ Basic summary using dplyr
eda_df %>%
  summarise(
    Total_Invoices = n(),
    Total_Billed   = sum(Original.Amount.Due, na.rm = TRUE),
    Avg_Invoice    = mean(Original.Amount.Due, na.rm = TRUE),
    Median_Invoice = median(Original.Amount.Due, na.rm = TRUE),
    Min_Invoice    = min(Original.Amount.Due, na.rm = TRUE),
    Max_Invoice    = max(Original.Amount.Due, na.rm = TRUE),
    SD_Invoice     = sd(Original.Amount.Due, na.rm = TRUE)
  )

# 3️⃣ Correlation matrix (numerical columns only)
num_vars <- eda_df %>%
  select(Original.Amount.Due, Current.Balance)

corr_matrix <- cor(num_vars, use = "complete.obs")

# Display numeric correlation values
corr_matrix

# 4️⃣ Visualize correlation matrix
corrplot(corr_matrix, method = "number", type = "upper",
         title = "Correlation Matrix – Eamon Express Closed Invoices",
         tl.col = "black", tl.srt = 45)
install.packages("psych")
install.packages("corrplot")
# Drop constant (zero-variance) columns automatically
num_vars <- eda_df %>%
  select(where(is.numeric)) %>%
  select_if(~ sd(., na.rm = TRUE) != 0)

corr_matrix <- cor(num_vars, use = "complete.obs")
corrplot(corr_matrix, method = "number", type = "upper")
# Load required libraries
library(dplyr)
library(ggplot2)
library(broom)  # for tidy model outputs

# Select key numeric and categorical predictors
model_df <- invoices %>%
  select(
    Original.Amount.Due,
    Actual.Weight.Amount,
    Rated.Weight.Amount,
    Number.of.Pieces,
    Service.Type
  ) %>%
  filter(!is.na(Original.Amount.Due))  # ensure complete cases

# Check structure
glimpse(model_df)
# Fit model
model <- lm(Original.Amount.Due ~ Actual.Weight.Amount + Rated.Weight.Amount + 
              Number.of.Pieces + Service.Type, data = model_df)

# Summarize regression
summary(model)
# Model diagnostics
par(mfrow = c(2, 2))
plot(model)

# R-squared and Adjusted R-squared
summary(model)$r.squared
summary(model)$adj.r.squared

# Clean summary table
tidy(model)
# Example: predict for a hypothetical shipment
new_invoice <- data.frame(
  Actual.Weight.Amount = 50,
  Rated.Weight.Amount = 55,
  Number.of.Pieces = 2,
  Service.Type = "GROUND"
)

predict(model, newdata = new_invoice)
levels(model_df$Service.Type)
# Ensure Service.Type is a factor
model_df <- model_df %>%
  mutate(Service.Type = as.factor(Service.Type))

# Refit the model
model <- lm(Original.Amount.Due ~ Actual.Weight.Amount + Rated.Weight.Amount +
              Number.of.Pieces + Service.Type, data = model_df)

# Confirm it’s now a factor
levels(model_df$Service.Type)
model <- lm(Original.Amount.Due ~ Actual.Weight.Amount + Rated.Weight.Amount +
              Number.of.Pieces + Service.Type, data = model_df)
# Predict for a sample shipment (using same factor levels as before)
new_invoice <- data.frame(
  Actual.Weight.Amount = 50,
  Rated.Weight.Amount = 55,
  Number.of.Pieces = 2,
  Service.Type = factor("FEDEX_GROUND", levels = levels(model_df$Service.Type))
)

# Prediction with 95% confidence & prediction intervals
predict(model, newdata = new_invoice, interval = "prediction", level = 0.95)
levels(model_df$Service.Type)
new_invoice <- data.frame(
  Actual.Weight.Amount = 50,
  Rated.Weight.Amount = 55,
  Number.of.Pieces = 2,
  Service.Type = factor("GROUND", levels = levels(model_df$Service.Type))
)
model <- lm(
  Original.Amount.Due ~ Actual.Weight.Amount + Rated.Weight.Amount +
    Number.of.Pieces + Service.Type,
  data = model_df,
  na.action = na.exclude
)
predict(model, newdata = new_invoice, interval = "prediction", level = 0.95)
levels(model_df$Service.Type)
model_df <- model_df %>%
  mutate(Service.Type = as.factor(Service.Type))
# Create new invoice observation with valid Service.Type level
new_invoice <- data.frame(
  Actual.Weight.Amount = 50,
  Rated.Weight.Amount = 55,
  Number.of.Pieces = 2,
  Service.Type = factor("Ground", levels = levels(model_df$Service.Type))
)

# Predict with 95% confidence & prediction intervals
predict(model, newdata = new_invoice, interval = "prediction", level = 0.95)
model <- lm(Original.Amount.Due ~ Actual.Weight.Amount + Service.Type, data = model_df)
summary(model)
library(car)
vif(model)  # Variance Inflation Factor
install.packages("car")
library(glmnet)

# Prepare data
x <- model.matrix(Original.Amount.Due ~ Actual.Weight.Amount + Rated.Weight.Amount +
                    Number.of.Pieces + Service.Type, data = model_df)[, -1]
y <- model_df$Original.Amount.Due

# Fit a ridge regression
ridge <- glmnet(x, y, alpha = 0)
plot(ridge)
install.packages("glmnet")
model_complete <- model_df %>%
  select(Original.Amount.Due, Actual.Weight.Amount, Rated.Weight.Amount,
         Number.of.Pieces, Service.Type) %>%
  drop_na()  # removes rows with any missing values
library(tidyverse)
# 1️⃣ Clean dataset (remove rows with any missing values)
model_complete <- model_df %>%
  select(Original.Amount.Due, Actual.Weight.Amount, Rated.Weight.Amount,
         Number.of.Pieces, Service.Type) %>%
  drop_na()

# 2️⃣ Create model matrix and response variable
x <- model.matrix(Original.Amount.Due ~ Actual.Weight.Amount + Rated.Weight.Amount +
                    Number.of.Pieces + Service.Type, data = model_complete)[, -1]

y <- model_complete$Original.Amount.Due

# 3️⃣ Confirm both have same number of observations
nrow(x); length(y)

# 4️⃣ Fit ridge regression
library(glmnet)
ridge <- glmnet(x, y, alpha = 0)

# 5️⃣ Cross-validation to find best lambda
cv_ridge <- cv.glmnet(x, y, alpha = 0)
plot(cv_ridge)
best_lambda <- cv_ridge$lambda.min
best_lambda

# 6️⃣ Coefficients at best lambda
coef(cv_ridge, s = "lambda.min")
# Performance metrics for the OLS model
library(Metrics)

# Predicted values (on training data)
ols_pred <- predict(model, newdata = model_df)

# Actual values
actual <- model_df$Original.Amount.Due

# Compute R² manually (to match ridge format)
ols_r2 <- cor(ols_pred, actual, use = "complete.obs")^2

# RMSE
ols_rmse <- rmse(actual, ols_pred)

cat("OLS Model — R²:", round(ols_r2, 3), "| RMSE:", round(ols_rmse, 3))
install.packages("metrics")
install.packages("Metrics")   # Capital M
library(Metrics)
# --- OLS Performance ---
ols_pred <- predict(model, newdata = model_df)
actual <- model_df$Original.Amount.Due
ols_r2 <- cor(ols_pred, actual, use = "complete.obs")^2
ols_rmse <- rmse(actual, ols_pred)

cat("OLS Model — R²:", round(ols_r2, 3), "| RMSE:", round(ols_rmse, 3), "\n")

# --- Ridge Performance ---
ridge_pred <- predict(cv_ridge, s = "lambda.min", newx = x)
ridge_r2 <- cor(ridge_pred, y)^2
ridge_rmse <- rmse(y, ridge_pred)

cat("Ridge Model — R²:", round(ridge_r2, 3), "| RMSE:", round(ridge_rmse, 3))
levels(model_df$Service.Type)
ols_pred <- predict(model, newdata = model_df)
model_df$Service.Type <- factor(model_df$Service.Type,
                                levels = levels(model_complete$Service.Type))
model_df$Service.Type <- factor(
  model_df$Service.Type,
  levels = levels(model_complete$Service.Type)
)
ols_pred <- predict(model, newdata = model_df)
levels(model_complete$Service.Type)
levels(model_df$Service.Type)
ols_pred <- predict(model, newdata = model_df)
best_lambda <- cv_ridge$lambda.min
coef(cv_ridge, s = "lambda.min")
# Predict invoice costs for all rows in your cleaned dataset
ridge_pred <- predict(cv_ridge, s = "lambda.min", newx = x)

# Convert sparse matrix to numeric vector
ridge_pred <- as.numeric(ridge_pred)
# Add predictions to your dataset
model_complete$Predicted.Amount <- ridge_pred

# Compare actual vs predicted
head(model_complete %>%
       select(Original.Amount.Due, Predicted.Amount))
library(ggplot2)

ggplot(model_complete, aes(x = Original.Amount.Due, y = Predicted.Amount)) +
  geom_point(color = "steelblue", size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkred") +
  labs(
    title = "Actual vs Predicted Invoice Costs (Ridge Regression)",
    x = "Actual Invoice Amount ($)",
    y = "Predicted Invoice Amount ($)"
  ) +
  theme_minimal()
write.csv(
  model_complete,
  "C:/Users/ali_a/OneDrive/Desktop/TEST/Eamon Express/ridge_predictions.csv",
  row.names = FALSE
)
write.csv(
  model_complete,
  "C:/Users/ali_a/OneDrive/Desktop/TEST/Eamon Express/ridge_predictions.csv",
  row.names = FALSE
)
# Calculate residuals (error = actual - predicted)
model_complete <- model_complete %>%
  mutate(Residual = Original.Amount.Due - Predicted.Amount)

# Quick summary
summary(model_complete$Residual)
ggplot(model_complete, aes(x = Service.Type, y = Residual, fill = Service.Type)) +
  geom_boxplot(alpha = 0.6) +
  labs(
    title = "Prediction Residuals by Service Type",
    x = "Service Type",
    y = "Residual (Actual - Predicted)"
  ) +
  theme_minimal()
ggplot(model_complete, aes(x = Service.Type, y = Residual, fill = Service.Type)) +
  geom_boxplot(alpha = 0.6) +
  labs(
    title = "Prediction Residuals by Service Type",
    x = "Service Type",
    y = "Residual (Actual - Predicted)"
  ) +
  theme_minimal()
# Correlation heatmap
corrplot(corr_matrix, method = "number", type = "upper")

# Ridge cross-validation
plot(cv_ridge)

# Actual vs Predicted scatter
ggplot(model_complete, aes(x = Original.Amount.Due, y = Predicted.Amount)) +
  geom_point(color = "steelblue", size = 3) +
  geom_abline(slope = 1, intercept = 0, color = "darkred", linetype = "dashed") +
  labs(title = "Actual vs Predicted Invoice Costs", x = "Actual ($)", y = "Predicted ($)") +
  theme_minimal()

# Residual boxplots by Service Type
ggplot(model_complete, aes(x = Service.Type, y = Residual, fill = Service.Type)) +
  geom_boxplot(alpha = 0.6) +
  labs(title = "Residuals by Service Type", x = "Service Type", y = "Residual (Actual - Predicted)") +
  theme_minimal()
pdf("C:/Users/ali_a/OneDrive/Desktop/TEST/Eamon Express/All_Plots_Report.pdf", width = 8, height = 6)

# Each plot command below will be added as a page
corrplot(corr_matrix, method = "number", type = "upper")
plot(cv_ridge)
print(p1)    # your actual-vs-predicted ggplot
print(p2)    # residual boxplot, if you saved it as p2

dev.off()   # closes the PDF file
library(ggplot2)
library(corrplot)

# 1. Correlation plot
corrplot(corr_matrix, method = "number", type = "upper")

# 2. Ridge CV plot
plot(cv_ridge)

# 3. Actual vs Predicted plot
p1 <- ggplot(model_complete, aes(x = Original.Amount.Due, y = Predicted.Amount)) +
  geom_point(color = "steelblue", size = 3) +
  geom_abline(slope = 1, intercept = 0, color = "darkred", linetype = "dashed") +
  labs(
    title = "Actual vs Predicted Invoice Costs (Ridge Model)",
    x = "Actual Invoice Amount ($)",
    y = "Predicted Invoice Amount ($)"
  ) +
  theme_minimal()

# 4. Residuals by Service Type
p2 <- ggplot(model_complete, aes(x = Service.Type, y = Residual, fill = Service.Type)) +
  geom_boxplot(alpha = 0.6) +
  labs(
    title = "Prediction Residuals by Service Type",
    x = "Service Type",
    y = "Residual (Actual - Predicted)"
  ) +
  theme_minimal()
pdf("C:/Users/ali_a/OneDrive/Desktop/TEST/Eamon Express/All_Plots_Report.pdf", 
    width = 8, height = 6)

# Correlation
corrplot(corr_matrix, method = "number", type = "upper")

# Ridge CV plot
plot(cv_ridge)

# Actual vs Predicted
print(p1)

# Residuals by Service Type
print(p2)

dev.off()
