# Dataset: World Bank / IMF Public Data
# Countries: India, USA, China, UK, Germany (2004-2023)
# ============================================================

# DATASET (World Bank / IMF sourced) 
# GDP Growth (%), Inflation (CPI %), Policy/Repo Rate (%)

year - rep(2004:2023, 5)
country <- c(rep("India",20), rep("USA",20), rep("China",20),
               rep("UK",20), rep("Germany",20))

gdp <- c(
  # India
  7.9, 9.3, 9.3, 9.8, 3.9, 8.4, 10.3, 6.6, 5.5, 6.4,
  7.4, 8.0, 8.3, 6.8,-5.8, 9.1, 7.2, 7.0, 8.2, 6.5,
  # USA
  3.9, 3.5, 2.9,-0.1,-2.5, 2.5, 1.6, 2.2, 1.8, 2.5,
  2.9, 2.3, 2.9, 2.3,-2.2, 5.9, 2.1, 1.9, 2.5, 2.8,
  # China
  10.1,11.4,12.7,14.2, 9.7,10.3,10.6, 9.5, 7.9, 7.8,
   7.4, 7.0, 6.9, 6.9,-2.0, 8.1, 3.0, 5.2, 4.6, 5.0,
  # UK
  2.5, 2.8, 2.7, 2.9,-4.3, 2.0, 1.6, 1.4, 2.1, 2.2,
  3.0, 2.4, 2.2, 1.7,-9.3, 7.6, 4.3, 0.1, 0.3, 0.9,
  # Germany
  1.2, 0.7, 3.7, 3.0,-5.6, 4.2, 3.9, 1.0, 0.4, 0.5,
  2.2, 1.7, 2.2, 1.1,-3.8, 2.6, 1.8,-0.3,-0.2, 0.2
)

inflation <- c(
  # India
  3.8, 4.2, 6.1, 6.4, 8.4, 9.5, 9.5, 8.9, 9.9,10.9,
  6.4, 5.9, 4.9, 2.5, 6.6, 5.1, 6.7, 5.5, 4.9, 5.4,
  # USA
  2.7, 3.4, 3.2, 2.9, 3.8,-0.4, 1.6, 3.2, 2.1, 1.5,
  1.6, 0.1, 1.3, 2.1, 1.2, 4.7, 8.0, 4.1, 2.9, 2.5,
  # China
  3.9, 1.8, 1.5, 4.8, 5.9,-0.7, 3.3, 5.5, 2.6, 2.6,
  2.0, 1.4, 2.0, 1.6, 2.4, 0.9, 2.0, 0.2, 0.5, 0.2,
  # UK
  1.3, 2.0, 2.3, 2.3, 2.2, 3.3, 4.5, 2.8, 2.8, 2.6,
  1.5, 0.0, 0.7, 2.7, 0.9, 2.6,  9.1, 7.3, 2.5, 2.2,
  # Germany
  1.8, 1.9, 1.8, 2.3, 0.3, 1.1, 2.1, 2.1, 2.0, 1.5,
  0.9, 0.2, 0.4, 1.7,-0.1, 3.1, 6.9, 5.9, 2.2, 2.3
)

repo_rate <- c(
  # India (RBI Repo Rate)
  6.0, 6.5, 7.25,7.75,5.0, 4.75,5.75,8.5, 8.5, 7.75,
  8.0, 7.75,6.5, 6.0, 4.0, 4.0, 4.0, 6.5, 6.5, 6.5,
  # USA (Fed Funds Rate)
  1.35,3.22,5.24,5.02,0.16,0.18,0.18,0.10,0.14,0.11,
  0.09,0.13,0.40,1.00,0.36,0.08,1.68,5.02,5.33,4.83,
  # China (1Y Loan Prime Rate proxy)
  5.31,5.58,6.12,6.71,5.31,5.56,5.81,6.56,6.31,6.15,
  5.77,5.35,4.85,4.35,3.85,3.85,3.70,3.45,3.45,3.10,
  # UK (Bank Rate)
  4.38,4.75,5.0, 5.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
  0.5, 0.5, 0.25,0.5, 0.1, 0.1, 1.75,5.25,5.25,4.75,
  # Germany (ECB Rate proxy)
  2.0, 2.5, 3.5, 4.0, 1.5, 1.0, 1.0, 1.25,0.75,0.5,
  0.25,0.05,0.05,0.0, 0.0, 0.0, 2.0, 4.5, 4.0, 3.15
)

panel <- data.frame(
  Country   = country,
  Year      = year,
  GDP       = gdp,
  Inflation = inflation,
  RepoRate  = repo_rate
)


# SIMPLE & MULTIPLE REGRESSION

# --- Correlation ---
cor_matrix <- cor(panel[, c("GDP","Inflation","RepoRate")])
print(round(cor_matrix, 4))


# --- Simple Regression: GDP ~ Inflation ---
model1 <- lm(GDP ~ Inflation, data = panel)

print(summary(model1))

# --- Multiple Regression: GDP ~ Inflation + RepoRate ---
model2 <- lm(GDP ~ Inflation + RepoRate, data = panel)

print(summary(model2))

predicted_GDP <- predict(model2, panel)


#  GRANGER CAUSALITY, WALD TEST, VIF
library(lmtest)
library(car)

# Granger Causality

print(grangertest(GDP ~ Inflation, data = panel))

print(grangertest(GDP ~ RepoRate, data = panel))

# Wald Test
print(waldtest(model1))

print(waldtest(model2))

# VIF
print(vif(model2))


# POOLED OLS (PANEL DATA)
# ============================================================


library(plm)

pdata <- pdata.frame(panel, index = c("Country","Year"))

model_pool <- plm(GDP ~ Inflation + RepoRate, data = pdata, model = "pooling")
model_fe   <- plm(GDP ~ Inflation + RepoRate, data = pdata, model = "within")
model_re   <- plm(GDP ~ Inflation + RepoRate, data = pdata, model = "random")


print(summary(model_pool))

print(summary(model_fe))


print(summary(model_re))

# Hausman Test
print(phtest(model_fe, model_re))

# FORECASTING (India only for time series)
library(forecast)

india_gdp <- panel$GDP[panel$Country == "India"]
gdp_ts    <- ts(india_gdp, start = 2004, frequency = 1)

# Holt-Winters
gdp_hw       <- HoltWinters(gdp_ts, gamma = FALSE)
forecast_hw  <- forecast(gdp_hw, h = 3)


print(summary(forecast_hw))

# ARIMA
gdp_arima     <- auto.arima(gdp_ts)
forecast_arima <- forecast(gdp_arima, h = 3)


print(summary(forecast_arima))


