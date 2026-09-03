data_path <- "Appendix_A_Coded_Review_Dataset_2000.csv"
out_dir <- "."

df <- read.csv(data_path, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM")

touchpoint_group <- function(x) {
  if (grepl("Payment/refund/cancellation", x)) return("Payment/refund/cancellation")
  if (grepl("Customer support", x)) return("Customer support")
  if (grepl("Matching/dispatch", x)) return("Matching/dispatch")
  if (grepl("Booking/scheduling", x)) return("Booking/scheduling")
  if (grepl("Communication/automation", x)) return("Communication/automation")
  if (grepl("Review/rating/transparency", x)) return("Review/rating/transparency")
  return("General platform/service experience")
}

df$TouchpointGroup <- sapply(df$AI.Digital.Touchpoint, touchpoint_group)
df$Negative <- ifelse(df$Trust.Valence == "Negative", 1, 0)
df$Rating <- as.numeric(df$Rating)

df$Region <- as.factor(df$Region)
df$Platform <- as.factor(df$Platform)
df$Trust.Theme <- as.factor(df$Trust.Theme)
df$TouchpointGroup <- as.factor(df$TouchpointGroup)

# Model 1: Multiple linear regression for rating
model_lm <- lm(Rating ~ Region + Platform + Trust.Theme + TouchpointGroup, data = df)
summary(model_lm)

# Model 2: Binary logistic regression for negative review
model_logit <- glm(Negative ~ Region + Platform + Trust.Theme + TouchpointGroup,
                   data = df,
                   family = binomial)
summary(model_logit)
exp(coef(model_logit))  # odds ratios

# Model 3: Random forest classification for negative review
# Install if needed:
# install.packages("randomForest")
library(randomForest)

set.seed(42)
idx <- sample(seq_len(nrow(df)), size = floor(0.7 * nrow(df)))
train <- df[idx, ]
test <- df[-idx, ]

model_rf <- randomForest(as.factor(Negative) ~ Region + Platform + Trust.Theme + TouchpointGroup,
                         data = train,
                         ntree = 500,
                         importance = TRUE)

pred <- predict(model_rf, newdata = test)
confusion <- table(Predicted = pred, Actual = as.factor(test$Negative))
print(confusion)
print(importance(model_rf))

write.csv(summary(model_lm)$coefficients,
          file.path(out_dir, "r_model_linear_regression_coefficients.csv"))
write.csv(summary(model_logit)$coefficients,
          file.path(out_dir, "r_model_logistic_regression_coefficients.csv"))
write.csv(exp(coef(model_logit)),
          file.path(out_dir, "r_model_logistic_odds_ratios.csv"))
write.csv(importance(model_rf),
          file.path(out_dir, "r_model_random_forest_importance.csv"))
