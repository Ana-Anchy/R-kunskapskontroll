# ============================
# Kunskapskontroll - Volvo Bilanalys från Blocket
# ============================


# ============================
# 📦 Paket och bibliotek
# ============================

packages <- c("tidyverse", "readxl", "GGally", "glmnet", "broom", "caret", "rmarkdown", "scales")

installed <- packages %in% installed.packages()
if (any(!installed)) {
  install.packages(packages[!installed])
}

# ============================
# Ladda packages
# ============================

library(readr)
library(tibble)
library(dplyr)
library(ggplot2)
library(GGally)
library(glmnet)
library(broom)
library(caret)
library(rmarkdown)
library(scales)


# ============================
#  1. Läs in och förbered data
# ============================

#Läs in datan från en CSV-fil.
df <- read_csv2("C:/Users/anaba/OneDrive/Desktop/R/data_blocket_volvo.csv")

#Spara originaldatan som referens.
df_raw <- df  

#Konvertera viktiga kolumner (miltal, hästkrafter, pris, årsmodell) till numeriskt format
df$miles      <- as.numeric(df$miles)
df$hpower     <- as.numeric(df$hpower)
df$price      <- as.numeric(df$price)
df$year_model <- as.numeric(df$year_model)

numeric_cols <- c("price", "miles", "hpower", "year_model")
df[numeric_cols] <- lapply(df[numeric_cols], function(x) as.numeric(gsub(" ", "", x)))

#Identifiera rader som misslyckas vid konverteringen till numeriskt format
bad_rows <- df %>%
filter(is.na(price) | is.na(miles) | is.na(hpower) | is.na(year_model))

#Spara och visa problematiska rader i en separat CSV
bad_rows <- df %>% filter(if_any(all_of(numeric_cols), is.na))
write_csv(bad_rows, "problemrader.csv")

#visa bad rows
View(bad_rows)
write_csv(bad_rows, "problemrader.csv")

#antal bad rows
nrow(bad_rows)

#Rensa bort rader med saknade (NA) värden i centrala numeriska variabler
df <- df %>% filter(!is.na(price), !is.na(miles), !is.na(hpower), !is.na(year_model))

#Visa en sammanfattning och kontrollera antalet NA-värden
summary(df)
colSums(is.na(df))

library(dplyr)

# Separera numeriska och kategoriska variabler
num_vars <- df %>% select(where(is.numeric))
cat_vars <- df %>% select(where(is.factor))

# Sammanfattning av numeriska variabler
summary(num_vars)

# Sammanfattning av kategoriska variabler
summary(cat_vars)


#Rensa bort rader med saknade värden
df <- df %>% filter(if_all(all_of(numeric_cols), ~ !is.na(.x)))

#Skapa nya variabler
df <- df %>% 
  mutate(
    age = 2024 - year_model,
    hk_div_age = hpower / (age + 1)
  )


# ============================
# 2. Boxplot & stapeldiagram för kategoriska variabler
# ============================

#============================
# Boxplot över pris per modell

ggplot(df, aes(x = model, y = price)) +
  geom_boxplot(fill = "gold", outlier.color = "darkred") +
  labs(title = "Prisfördelning per Volvomodell",
       x = "Modell",
       y = "Pris (kr)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # rotera x-etiketter

#Här ser vi hur priset varierar beroende på modelltyp. Exempelvis tenderar XC-modeller att ligga högre i pris, medan äldre modeller som V70 visar lägre medianvärden. 
#Det ger en bra inblick i hur modellen påverkar värdet."

# ============================
# Boxplot över pris per växellådetyp som visar prisskillnader mellan automat och manuell
print(ggplot(df, aes(x = gear, y = price)) +
        geom_boxplot(fill = "skyblue") +
        labs(title = "Price by Gear Type", x = "Gear", y = "Price (SEK)") +
        theme_minimal())

# ============================
# Boxplot över pris per årsmodell
ggplot(df, aes(x = as.factor(year_model), y = price)) +
  geom_boxplot(fill = "lightgreen", outlier.color = "red") +
  labs(title = "Prisfördelning per årsmodell",
       x = "Årsmodell",
       y = "Pris (kr)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # rotera etiketter

#I denna boxplot ser vi hur priset generellt minskar för äldre årsmodeller, vilket är väntat då bilar tappar i värde med åldern.
#Nyare bilar, t.ex. från 2020 och framåt, visar högre medianpriser och större spridning.

# Load stringr
library(stringr)
# ============================
# (e.g. "volvo" -> "Volvo")
df$brand <- str_to_title(df$brand)

# Stapeldiagram över bilmärke (förväntat endast Volvo i datan)
print(ggplot(df, aes(x = brand)) +
        geom_bar(fill = "coral") +
        labs(title = "Number of Cars per Brand", x = "Brand", y = "Count") +
        theme_minimal())


# ============================
# 3. Exploratory Data Analysis (EDA)
# ============================

# åldervariabel (baserat på max år i datan)
df$age <- max(df$year_model, na.rm = TRUE) - df$year_model

# Transformation: Hästkrafter / √(ålder + 1)
df$hk_div_age <- df$hpower / sqrt(df$age + 1)

library(ggplot2)
library(scales)

# ============================

#Prisfördelning per bränsletyp
ggplot(df, aes(x = fuel, y = price)) +
  geom_boxplot(fill = "lightblue", outlier.color = "red") +
  labs(
    title = "Prisfördelning per bränsletyp",
    x = "Bränsle",
    y = "Pris (kr)"
  ) +
  theme_minimal()

# ============================
#Standardisera karosstyp 
df$type <- stringr::str_to_title(tolower(df$type))

#Prisvariation beroende på karosstyp
ggplot(df, aes(x = type, y = price)) +
  geom_boxplot(fill = "yellow", color = "black") +
  labs(
    title = "Prisfördelning per karosstyp",
    x = "Karosstyp",
    y = "Pris"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # rotera x-etiketter

# ============================
#Visualisera fördelningen av miltal med ett histogram
ggplot(df, aes(x = miles)) +
  geom_histogram(binwidth = 5000, fill = "darkgreen", color = "black") +
  labs(title = "Distribution of Mileage", x = "Mileage (miles)", y = "Count") +
  scale_x_continuous(labels = comma) +
  theme_minimal()

# ============================
#Visualisera fördelningen av hästkrafter (hp) med ett histogram
print(ggplot(df, aes(x = hpower)) +
        geom_histogram(binwidth = 10, fill = "purple", color = "white") +
        labs(title = "Distribution of Horsepower", x = "Horsepower", y = "Count") +
        theme_minimal())

# ============================
#Visualisera fördelningen av priser med ett histogram.
print(ggplot(df, aes(x = price)) +
        geom_histogram(binwidth = 10000, fill = "pink", color = "white") +
        labs(title = "Distribution of Price", x = "Price (SEK)", y = "Count") +
        theme_minimal())



library(GGally)
library(scales)

#Anpassad funktion för att formatera y-axlar

# ============================
# 4. Correlation Matrix
# ============================

custom_axis <- function(data, mapping, ...) {
  ggplot(data = data, mapping = mapping) +
    geom_point(...) +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(labels = comma)
}

custom_density <- function(data, mapping, ...) {
  ggplot(data = data, mapping = mapping) +
    stat_density(aes(y = ..density..), geom = "line", position = "identity", ...) +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(labels = comma)
}


ggpairs(df %>% select(price, miles, hpower),
        title = "Correlation Matrix",
        upper = list(continuous = wrap("cor", size = 4)),
        lower = list(continuous = custom_axis),
        diag = list(continuous = custom_density))


# ============================
# 5. Train/Test Split med Caret
# ============================

index <- createDataPartition(df$price, p = 0.7, list = FALSE)
train_df <- df[index, ]
test_df <- df[-index, ]

# ============================
# . Alternative models (numeric and categorical)
# ============================

# Kontrollera hur många variationer varje faktor har
sapply(train_df[, c("gear", "brand", "model")], function(x) length(unique(x)))

table(train_df$gear)
table(train_df$brand)
table(train_df$model)


#Skapa en numerisk regressionsmodell baserat på miltal och hästkrafter
model_num <- lm(price ~ miles + hpower, data = train_df)

#Omvandla växellåda, bilmärke och modell till faktorer för kategorisk analys

train_df <- train_df %>%
  group_by(gear) %>% filter(n() > 1) %>%
  group_by(brand) %>% filter(n() > 1) %>%
  group_by(model) %>% filter(n() > 1) %>%
  ungroup()

train_df$gear <- as.factor(train_df$gear)
train_df$brand <- as.factor(train_df$brand)
train_df$model <- as.factor(train_df$model)

if (
  length(unique(train_df$brand)) > 1 &
  length(unique(train_df$gear)) > 1 &
  length(unique(train_df$model)) > 1
) {
  model_cat <- lm(price ~ gear + brand + model, data = train_df)
  summary(model_cat)
} else {
  print("Hoppar över kategorisk modell: en eller flera faktorer har bara en nivå.")
}



summary(model_num)


#tabell över antal Volvo-modeller i träningdata
model_counts <- as.data.frame(table(train_df$model))

# Sortera modellerna efter antal annonser (från flest till minst)
model_counts <- model_counts[order(-model_counts$Freq), ]

# Visa själva tabellen i konsolen 
print(model_counts)

# Skapa stapeldiagram över antal annonser per Volvo-modell
ggplot(model_counts, aes(x = reorder(Var1, -Freq), y = Freq)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Antal annonser per volvo-modell",
       x = "Modell",
       y = "Antal annonser") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # rotera x-etiketter

# ============================
# 6. OLS Regression på hela datan
# ============================


ols_model <- lm(price ~ miles + hpower, data = df)
summary(ols_model)

ols_preds <- predict(ols_model)
ols_r2 <- summary(ols_model)$r.squared
ols_rmse <- sqrt(mean((df$price - ols_preds)^2))
ols_bic <- BIC(ols_model)

cat("OLS R²: ", round(ols_r2, 3), "\n")
cat("OLS RMSE: ", round(ols_rmse), "\n")
cat("OLS BIC: ", round(ols_bic), "\n")


# ============================
# 6.1 Enkel modell med transformerad variabel
# ============================

df <- df %>%
  mutate(
    age = 2024 - year_model + 1,
    age = ifelse(age <= 0 | is.na(age), NA, age),  
    hk_div_age = hpower / age
  )


simple_model <- lm(price ~ hk_div_age, data = df)
summary(simple_model)

ggplot(df, aes(x = hk_div_age, y = price)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", color = "darkred", se = FALSE) +
  labs(
    title = "Pris som funktion av hästkrafter / √(ålder + 1)",
    x = "Hästkrafter / √(ålder + 1)",
    y = "Pris (SEK)"
  ) +
  theme_minimal()

# ================================
# 6.3 OLS Regression på train/test
# ================================

ols_model_test <- lm(price ~ miles + hpower, data = train_df)
summary(ols_model_test)

ols_preds <- predict(ols_model, newdata = test_df)
ols_r2 <- 1 - sum((test_df$price - ols_preds)^2) / sum((test_df$price - mean(test_df$price))^2)
ols_rmse <- sqrt(mean((test_df$price - ols_preds)^2))
ols_bic <- BIC(ols_model)

cat("Test R²: ", round(ols_r2, 3), "\n")
cat("Test RMSE: ", round(ols_rmse), "\n")
cat("OLS BIC: ", round(ols_bic), "\n")


# ============================
# 7. Lasso Regression
# ============================
set.seed(123)

x <- as.matrix(df[, c("miles", "hpower")])
y <- df$price

lasso_cv <- cv.glmnet(x, y, alpha = 1)
best_lambda <- lasso_cv$lambda.min

lasso_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
lasso_preds <- predict(lasso_model, s = best_lambda, newx = x)

lasso_r2 <- 1 - sum((y - lasso_preds)^2) / sum((y - mean(y))^2)
lasso_rmse <- sqrt(mean((y - lasso_preds)^2))

cat("Lasso R²: ", round(lasso_r2, 3), "\n")
cat("Lasso RMSE: ", round(lasso_rmse), "\n")
cat("Valt lambda: ", round(best_lambda, 2), "\n")

print(coef(lasso_model, s = best_lambda))

# ============================
# 8. Konfidens- och Prediktionsintervall
# ============================

new_car <- data.frame(miles = 15000, hpower = 140)

conf_interval <- predict(ols_model, newdata = new_car, interval = "confidence")
pred_interval <- predict(ols_model, newdata = new_car, interval = "prediction")

cat("\nKonfidensintervall:\n")
print(conf_interval)

cat("\nPrediktionsintervall:\n")
print(pred_interval)


# ============================
# 9. Exportera resultat
# ============================

results <- data.frame(
  Model = c("OLS", "Lasso"),
  R2 = c(ols_r2, lasso_r2),
  RMSE = c(ols_rmse, lasso_rmse),
  BIC = c(ols_bic, NA)
)

write.csv(results, "model_comparison.csv", row.names = FALSE)



# ============================
# 10. Extra: Cross-validation utvärdering (OLS)
# ============================
library(caret) 

set.seed(123)

train_control <- trainControl(method = "cv", number = 5)

cv_model <- train(price ~ miles + hpower,
                  data = df,
                  method = "lm",
                  trControl = train_control)

cat("\nCV RMSE (OLS): ", cv_model$results$RMSE, "\n")
cat("CV R² (OLS): ", cv_model$results$Rsquared, "\n")


# =====================================================
# 📊 # 🔚 Slutsats och Resultat
# =====================================================

# - EDA genomfördes med hjälp av histogram, boxplots och korrelationsanalys.
# - Jämförelse mellan OLS och Lasso-regression visade tydliga mönster.
# - Modellvalidering utfördes med både train/test-split och 5-fold cross-validation.
# - Resultat exporterades till CSV för vidare presentation.
# - Konfidens- och prediktionsintervall beräknades för en ny bil.
# - Variabeltolkning visade att körsträcka och hästkrafter var mest avgörande för försäljningspriset.

# -----------------------------------------------------
# ❓ Frågeställning 1: Vilka variabler har störst påverkan på priset för en begagnad bil?
# -----------------------------------------------------
# - Både OLS och Lasso identifierade:
#     ▸ Körsträcka (miles) – negativ korrelation med priset
#     ▸ Hästkrafter (hpower) – positiv korrelation med priset
# - Dessa variabler var statistiskt signifikanta och återkom i båda modellerna.

# -----------------------------------------------------
# ❓ Frågeställning 2: Hur väl kan en regressionsmodell förklara variationen i priset?
# -----------------------------------------------------
# - OLS-modellen:
#     ▸ R² ≈ 0.693
#     ▸ RMSE ≈ 917.53kr

# - Lasso-modellen:
#     ▸ R² ≈ 0.680
#     ▸ RMSE ≈ 93 699 kr
#     ▸ Lambda ≈ 15 685

# - Cross-validation (OLS):
#     ▸ CV R² ≈ 0.678
#     ▸ CV RMSE ≈ 105.393 kr

# -----------------------------------------------------
# 🧠 Slutsats:
# -----------------------------------------------------
# Både OLS och Lasso visade god förklaringsgrad trots att endast två variabler användes.
# Lasso gav en något lägre förklaringsgrad men kan bidra till en enklare modell och minska risken för överanpassning.
# Modellerna lyckades förklara cirka 66–68 % av variationen i pris, vilket är rimligt med tanke på att viktiga faktorer som skick, utrustning och säsong inte fanns med i datan.
# Cross-validation visade att modellen presterar stabilt även på ny data, även om felmarginalen (RMSE) ökar något.
# Sammantaget visar analysen att regressionsmodeller kan ge värdefulla insikter för prissättning av begagnade bilar och hjälpa köpare och säljare att göra mer informerade beslut.

