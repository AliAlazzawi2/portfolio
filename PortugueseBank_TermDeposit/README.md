# 🏦 Portuguese Bank Term Deposit Prediction

### 📘 Overview
Predictive modeling of term-deposit subscriptions using the UCI Bank Marketing dataset.  
This project applied **association-rule mining** and **EDA** to uncover customer patterns that influence marketing success.

### ⚙️ Tools & Methods
- **R packages:** arules, arulesViz, dplyr, ggplot2  
- **Techniques:** Association Rules, Confidence/Lift analysis, Exploratory Data Analysis  

### 📈 Key Results
| Rule | Support | Confidence | Lift |
|------|----------|-------------|------|
| poutcome=success ⇒ y=yes | 0.021 | 0.659 | **5.85** |
| contact=cellular + poutcome=success ⇒ y=yes | 0.020 | 0.652 | **5.79** |

Clients with previous successful campaigns are ~5.8× more likely to subscribe again.

### 📊 Files
- [`BankCD_Report.pdf`](BankCD_Report.pdf)
- [`EDA-half-way-report.docx`](EDA-half-way-report.docx)
- [`Rplot-hslf-way-report.pdf`](Rplot-hslf-way-report.pdf)
- [`bank-additional-full.csv`](bank-additional-full.csv)
- [`association_rules.R`](association_rules.R)
