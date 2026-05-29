
library(dplyr)
library(ggplot2)
library(GGally)
library(nnet)


## ---------------------------------------------------------
## 0. Data Loading and Review
## ---------------------------------------------------------

data <- read.csv("../data/Crop_recommendation.csv")
head(data)

#renaming some columns
data <- data %>% rename(nitrogen=N, phosphorous=P, potassium=K, crop=label)
str(data)
summary(data)
unique(data$crop)

# setting response to factor
data$crop <- as.factor(data$crop)

# checking for missing values (NAs and blank spaces)
sapply(data, function(x) {
  sum(is.na(x) | (is.character(x) & trimws(x) == ""))
})


head(data)

## ---------------------------------------------------------
## 1. Basic plots
## ---------------------------------------------------------

par(mfrow = c(1, 1), mar = c(10, 4, 4, 2))
table(data$crop)
barplot(table(data$crop), las = 2, col = "lightseagreen", main = "Crop Counts")



# boxplots: nitrogen, phosphorous, potassium
par(mfrow = c(1, 3), mar = c(10, 5.5, 4, 2), cex.lab = 1.5)

boxplot(nitrogen ~ crop, data = data, las = 2, col = "skyblue", xlab = "")
mtext("Crop", side = 1, line = 7, cex = .9)

boxplot(phosphorous ~ crop, data = data, las = 2, col = "gold", xlab = "")
mtext("Crop", side = 1, line = 7, cex = .9)

boxplot(potassium ~ crop, data = data, las = 2, col = "salmon", xlab = "")
mtext("Crop", side = 1, line = 7, cex = .9)

par(mfrow = c(1, 1))








# boxplots: temperature, humidity
par(mfrow = c(1, 2), mar = c(10, 5.5, 4, 2), cex.lab = 1)

boxplot(temperature ~ crop, data = data, las = 2, col = "thistle", xlab = "")
mtext("Crop", side = 1, line = 7, cex = .9)

boxplot(humidity ~ crop, data = data, las = 2, col = "yellowgreen", xlab = "")
mtext("Crop", side = 1, line = 7, cex = .9)

par(mfrow = c(1, 1))


# boxplots: ph, rainfall
par(mfrow = c(1, 2), mar = c(10, 5.5, 4, 2), cex.lab = 1)

boxplot(ph ~ crop, data = data, las = 2, col = "gold3", xlab = "")
mtext("Crop", side = 1, line = 7, cex = .9)

boxplot(rainfall ~ crop, data = data, las = 2, col = "aquamarine", xlab = "")
mtext("Crop", side = 1, line = 7, cex = .9)

par(mfrow = c(1, 1))




# continious comparisons
ggpairs(
  data[, c("nitrogen", "phosphorous", "potassium", "temperature", "humidity", "ph", "rainfall")],
  lower = list(
    continuous = wrap("points", color = "steelblue", alpha = 0.8)
  ),
  diag = list(
    continuous = wrap("densityDiag", fill = "lightblue", color = "darkblue")
  )
)



##### checking for possible interactions

library(patchwork)

# phosphorous vs. potassium
p1 <- ggplot(data, aes(x = phosphorous, y = potassium, color = crop)) +
  geom_point(alpha = 0.6) +
  labs(x = "Phosphorous", y = "Potassium", title = "Phosphorous vs Potassium by Crop")


# rainfall vs. ph
p2 <- ggplot(data, aes(x = rainfall, y = ph, color = crop)) + 
  geom_point(alpha = 0.6) +
  labs(x = "Rainfall", y = "PH", title = "Rainfall vs PH")



# nitrogen vs. temperature
p3 <- ggplot(data, aes(x = nitrogen, y = temperature, color = crop)) +
  geom_point(alpha = 0.6) +
  labs(x = "Nitrogen", y = "Temperature", title = "Nitrogen vs Temperature")

(p1 + p2 + p3) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")


## ---------------------------------------------------------
## 2. Fitting full multinomial model
## ---------------------------------------------------------


full_mod <- multinom(crop ~ ., data = data)

summary(full_mod)


## ---------------------------------------------------------
## 3. Predicted class probabilities
## ---------------------------------------------------------
pred_prob <- predict(full_mod, type = "probs")
head(pred_prob)

## Each row gives:
## P(apple), P(banana), P(blackgram), etc.

## ---------------------------------------------------------
## 4. Predicted class labels
## ---------------------------------------------------------
pred_class <- predict(full_mod, type = "class")
head(pred_class)

## Confusion matrix
table(Predicted = pred_class, Observed = data$crop)

## Overall classification accuracy
mean(pred_class == data$crop)


## ---------------------------------------------------------
## 5. Coefficient table with approximate z-values
## ---------------------------------------------------------
nom_sum <- summary(full_mod)

coef_mat <- nom_sum$coefficients
se_mat   <- nom_sum$standard.errors
z_mat    <- coef_mat / se_mat
p_mat    <- 2 * (1 - pnorm(abs(z_mat)))

coef_mat
se_mat
z_mat
p_mat


# screening p-values to see how many are below or above 0.05
sig_summary <- data.frame(
  Predictor = colnames(p_mat[, -1]),
  Significant = colSums(p_mat[, -1] < 0.05),
  Not_Significant = colSums(p_mat[, -1] >= 0.05)
)

sig_summary



## ---------------------------------------------------------
## 6. Likelihood ratio test for nested models
## ---------------------------------------------------------

# top 3 with least significant numbers: temperature, humidity, nitrogen

mod_no_t <- multinom(crop ~ nitrogen + phosphorous + potassium + humidity + ph + rainfall, data = data)

mod_no_h <- multinom(crop ~ nitrogen + phosphorous + potassium + temperature + ph + rainfall, data = data)

mod_no_n <- multinom(crop ~ phosphorous + potassium + temperature + humidity + ph + rainfall, data = data)

mod_no_phos <- multinom(crop ~ nitrogen + potassium + temperature + humidity + ph + rainfall, data = data)

mod_no_po <- multinom(crop ~ nitrogen + phosphorous + temperature + humidity + ph + rainfall, data = data)


mod_no_temp_hu <- multinom(crop ~ nitrogen + phosphorous + potassium + ph + rainfall, data = data)



# H_0: smaller model is adequate
# H_1: bigger model is better



## LR statistic (Likelihood ratio test)

lr_test <- function(fit_small, fit_big) {
  LL_small <- logLik(fit_small)
  LL_big   <- logLik(fit_big)
  
  LR_stat <- 2 * (as.numeric(LL_big) - as.numeric(LL_small))
  df_diff <- attr(LL_big, "df") - attr(LL_small, "df")
  p_value <- 1 - pchisq(LR_stat, df = df_diff)
  
  data.frame(
    LR_stat = LR_stat,
    df_diff = df_diff,
    p_value = p_value
  )
}


lr_test(mod_no_t, full_mod)
lr_test(mod_no_h, full_mod)
lr_test(mod_no_n, full_mod)
lr_test(mod_no_phos, full_mod)
lr_test(mod_no_po, full_mod)
lr_test(mod_no_temp_hu, full_mod)

#testing interactions


# phosphorous:potassium
full_mod_int1 <- multinom(crop ~ nitrogen + phosphorous + potassium + temperature + 
                            humidity + ph + rainfall + phosphorous:potassium, data = data)



# nitrogen:temperature
# maxit=1000, sets max # of iterations to help model converge
# trace=F to hid iteration details
full_mod_int2 <- multinom(crop ~ nitrogen + phosphorous + potassium + temperature + 
                            humidity + ph + rainfall + nitrogen:temperature, data = data, maxit=1000, trace=F)



lr_test(full_mod, full_mod_int1)
lr_test(full_mod, full_mod_int2)



## ---------------------------------------------------------
## 7. Selection Approach (AIC)
## ---------------------------------------------------------

# doing selection approach to select more model candidates to add to Cross-Validation

library(MASS)

# Intercept-only
m0 <- multinom(crop ~ 1, data=data, trace=F)

forward_model <- stepAIC(
  m0,
  scope = list(lower = ~1,
               upper = ~ nitrogen + phosphorous + potassium +
                 temperature + humidity + ph + rainfall),
  direction = "forward",
  trace = TRUE
)

backward_model <- stepAIC(
  full_mod,
  direction = "backward",
  trace = TRUE
)

both_model <- stepAIC(
  m0,
  scope = list(lower = ~1,
               upper = ~ nitrogen + phosphorous + potassium +
                 temperature + humidity + ph + rainfall),
  direction = "both",
  trace = TRUE
)


cat("Forward AIC:", AIC(forward_model), "\n")
cat("Backward AIC:", AIC(backward_model), "\n")
cat("Both AIC:", AIC(both_model), "\n")


#forward and both chose the same model
fb_mod <- multinom(crop ~ potassium + humidity + rainfall+ phosphorous, data=data, trace=F)

#backward chose a different model
b_mod <- multinom(crop ~ nitrogen + phosphorous + potassium + temperature + humidity + 
                    rainfall, data=data, trace=F)



## ---------------------------------------------------------
## 6. AIC Testing
## ---------------------------------------------------------

# lowest AIC number is best
AIC(full_mod, full_mod_int1, full_mod_int2)

summary(full_mod_int2)
summary(full_mod_int1)


## ---------------------------------------------------------
## 6. 5-fold Cross-Validation
## ---------------------------------------------------------

library(caret)
library(nnet)

set.seed(123)

ctrl <- trainControl(
  method = "cv",
  number = 5
)

cv_full <- train(
  crop ~ nitrogen + phosphorous + potassium + temperature + humidity + ph + rainfall,
  data = data,
  method = "multinom",
  trControl = ctrl,
  trace = FALSE
)

cv_int1 <- train(
  crop ~ nitrogen + phosphorous + potassium + temperature + humidity + ph + rainfall +
    phosphorous:potassium,
  data = data,
  method = "multinom",
  trControl = ctrl,
  trace = FALSE
)

cv_int2 <- train(
  crop ~ nitrogen + phosphorous + potassium + temperature + humidity + ph + rainfall +
    nitrogen:temperature,
  data = data,
  method = "multinom",
  trControl = ctrl,
  trace = FALSE,
  maxit = 1000
)


cv_fb <- train(
  crop ~ potassium + humidity + rainfall+ phosphorous,
  data = data,
  method = "multinom",
  trControl = ctrl,
  trace = FALSE
)


cv_b <- train(
  crop ~ nitrogen + phosphorous + potassium + temperature + humidity + rainfall,
  data = data,
  method = "multinom",
  trControl = ctrl,
  trace = FALSE
)


results <- resamples(list(
  full_mod = cv_full,
  full_mod_int1 = cv_int1,
  full_mod_int2 = cv_int2,
  fb_mod = cv_fb,
  b_mod = cv_b
))

summary(results)



# final AIC comparison

AIC(full_mod, full_mod_int1, full_mod_int2, fb_mod, b_mod)
