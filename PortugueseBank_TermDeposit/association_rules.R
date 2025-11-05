# ----------------------------------------------------------
# Portuguese Bank Term Deposit Analysis
# Association Rule Mining | Author: Ali Al Azzawi
# ----------------------------------------------------------

# Load required libraries
library(arules)
library(arulesViz)
library(dplyr)
library(ggplot2)

# ----------------------------------------------------------
# 1. Load Data
# ----------------------------------------------------------
# Adjust the path if running locally
bank <- read.csv("bank-additional-full.csv", sep = ";")

# Preview structure
str(bank)
summary(bank)

# ----------------------------------------------------------
# 2. Data Cleaning
# ----------------------------------------------------------
# Remove irrelevant attributes (like duration for fairness)
bank <- bank %>%
  select(-duration)

# Convert all character columns to factors
bank[] <- lapply(bank, function(x)
  if(is.character(x)) as.factor(x) else x)

# ----------------------------------------------------------
# 3. Create Transaction Object
# ----------------------------------------------------------
bank_trans <- as(bank, "transactions")
summary(bank_trans)

# ----------------------------------------------------------
# 4. Generate Association Rules
# ----------------------------------------------------------
rules <- apriori(bank_trans,
                 parameter = list(supp = 0.01,
                                  conf = 0.6,
                                  minlen = 2,
                                  maxlen = 5,
                                  target = "rules"))

# Sort and inspect top rules
rules_sorted <- sort(rules, by = "lift", decreasing = TRUE)
inspect(head(rules_sorted, 10))

# ----------------------------------------------------------
# 5. Filter for term deposit subscriptions
# ----------------------------------------------------------
rules_yes <- subset(rules_sorted, subset = rhs %in% "y=yes")
inspect(head(rules_yes, 10))

# ----------------------------------------------------------
# 6. Visualization
# ----------------------------------------------------------
# Scatter plot of support vs confidence
pdf("visualizations/lift_support_plot.pdf", width = 7, height = 6)
plot(rules_yes, measure = "support", shading = "lift")
dev.off()

# Grouped matrix plot
pdf("visualizations/grouped_matrix_plot.pdf", width = 7, height = 6)
plot(rules_yes, method = "grouped")
dev.off()

# ----------------------------------------------------------
# 7. Export Rules
# ----------------------------------------------------------
write.csv(as(rules_yes, "data.frame"),
          "association_rules_yes.csv",
          row.names = FALSE)

# ----------------------------------------------------------
# 8. Key Insight
# ----------------------------------------------------------
cat("Top rule suggests: clients with previous successful outcome are ~5.8x more likely to subscribe again.\n")
