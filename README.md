# R-kunskapskontroll

# 🚗 Volvo Used Car Price Analysis (Blocket Data)

This project analyzes used Volvo car listings from Blocket using R. The goal is to understand what factors influence price and how well statistical models can predict it.

---

## 📊 Project Summary

We use statistical modeling and visualization to explore how factors such as mileage and horsepower affect used car prices.

Key components include:
- Data cleaning and preprocessing
- Exploratory Data Analysis (EDA)
- Regression modeling (OLS and Lasso)
- Model evaluation using R², RMSE, and cross-validation
- Prediction intervals for new observations
- Export of results and potential report generation

---

## 📁 Dataset
 
**Rows:** ~900  
**Columns:** 15  
 

**Variables include:**
- `price`: Selling price (SEK)
- `miles`: Mileage driven (numeric)
- `hpower`: Horsepower (numeric)
- `gear`: Gearbox type
- `brand`: Car brand (Volvo)
- `model`: Specific car model
- `fuel`, `engine`, `color`, etc.

---

## 🧪 Models

### OLS Regression
```r
price ~ miles + hpower
- **R²** ≈ 0.677  
- **RMSE** ≈ 92,532 SEK  
- **BIC** ≈ 17,971

### Lasso Regression
```r
price ~ miles + hpower  (automatic variable shrinkage)
```
- **R²** ≈ 0.649  
- **RMSE** ≈ 96,391 SEK  
- **Lambda (λ)** ≈ 22,257

---

## 🔁 Cross-validation

5-fold CV for OLS:
- **CV R²** ≈ 0.672  
- **CV RMSE** ≈ 104,769 SEK  

---

## 📈 Visualizations

- Histogram: `miles`, `hpower`, and `price`
- Boxplot: Price by gear type
- Bar chart: Number of cars by brand
- Correlation matrix using `GGally::ggpairs`

---

## 📦 Output

- Model summaries printed in console
- Confidence and prediction intervals for a new car:
  - `miles = 15000`, `hpower = 140`
  - Example prediction: ~155,339 SEK
- Exported results to CSV: `model_comparison.csv`

---

## 📌 Key Insights

- **Mileage** negatively impacts price.
- **Horsepower** positively impacts price.
- OLS and Lasso models both perform well.
- Models explain ~65–68% of the price variation.
- Remaining variation likely due to car condition, features, or seasonality.

