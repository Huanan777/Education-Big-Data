# === 0. 設定環境 ===
setwd("C:\\Users\\huann\\OneDrive\\Desktop\\Education Big Data\\原始資料")

library(lavaan)
library(gtools)

# === 1. 載入資料與預處理 ===
data2 <- read.csv("data_1.csv", stringsAsFactors = FALSE, fileEncoding = "big5")
data_items <- data2[, grep("^v[0-9]+$", names(data2))]
data_items <- na.omit(data_items)

# === 2. CFA 檢查模型適配度 ===
model <- '
  F1 =~ v2 + v3 + v5 + v8
  F2 =~ v14 + v16 + v17 + v18
  F3 =~ v21 + v22 + v23 + v24
  F4 =~ v28 + v29 + v30 + v31
  F5 =~ v35 + v37 + v38 + v39 + v44
  F6 =~ v47 + v50 + v52 + v57
'
fit <- cfa(model, data = data_items, estimator = "MLR")
print(fitMeasures(fit, c("cfi", "tli", "rmsea", "srmr")))

# === SEM 路徑分析（檢驗六個潛在變項之間的假設關係）===
fscores <- lavPredict(fit)  # 產出 F1~F6 每位受試者的分數
sem_model <- '
  F1 =~ v2 + v3 + v5 + v8
  F2 =~ v14 + v16 + v17 + v18
  F3 =~ v21 + v22 + v23 + v24
  F4 =~ v28 + v29 + v30 + v31
  F5 =~ v35 + v37 + v38 + v39 + v44
  F6 =~ v47 + v50 + v52 + v57

  F4 ~ F1 + F5             # 自我效能 ← 生涯阻礙、未來時間觀
  F2 ~ F1 + F4 + F5        # 結果預期 ← 生涯阻礙、自我效能、未來時間觀
  F3 ~ F1 + F4 + F2        # 學習興趣 ← 生涯阻礙、自我效能、未來時間觀
  F6 ~ F2 + F5 + F4 + F3   # 生涯希望感 ← 生涯阻礙、結果預期、學習興趣
'
sem_model1 <- '
  F1 =~ v2 + v3 + v5 + v8
  F5 =~ v35 + v37 + v38 + v39 + v44
  F1 ~ F5
'

sem_model2 <- '
  F1 =~ v2 + v3 + v5 + v8
  F5 =~ v35 + v37 + v38 + v39 + v44
  F1 ~ F5
'

fit_sem <- sem(sem_model, data = data_items, estimator = "MLR")
fit_sem1 <- sem(sem_model1, data = data_items, estimator = "MLR")
fit_sem2 <- sem(sem_model2, data = data_items, estimator = "MLR")
summary(fit_sem, standardized = TRUE, fit.measures = TRUE)
summary(fit_sem1, standardized = TRUE, fit.measures = TRUE)
summary(fit_sem2, standardized = TRUE, fit.measures = TRUE)

# === F5/F1 -> 中介 -> F6 ===
model_q1 <- '
  # 測量模型
  F1 =~ v2 + v3 + v5 + v8
  F2 =~ v14 + v16 + v17 + v18
  F3 =~ v21 + v22 + v23 + v24
  F4 =~ v28 + v29 + v30 + v31
  F5 =~ v35 + v37 + v38 + v39 + v44
  F6 =~ v47 + v50 + v52 + v57

  # 中介結構模型
  F4 ~ a1*F1
  F2 ~ a2*F1
  F3 ~ a3*F1 + d1*F4 + d2*F2
  F6 ~ b1*F4 + b2*F2 + b3*F3 + c*F1

  # 間接效果定義（透過每條路徑）
  ind1 := a1 * b1   # F5 → F4 → F6
  ind2 := a2 * b2   # F5 → F2 → F6
  ind3 := a3 * b3   # F5 → F3 → F6
  ind4 := a1 * d1 * b3  # F5 → F4 → F3 → F6
  ind5 := a2 * d2 * b3  # F5 → F2 → F3 → F6

  # 總間接效果與總效果
  total_indirect := ind1 + ind2 + ind3 + ind4 + ind5
  total_effect := c + total_indirect
'
fit_q1 <- sem(model_q1, data = data_items, estimator = "MLR")
summary(fit_q1, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)
fit_q1 <- sem(model_q1, data = data_items, se = "bootstrap", bootstrap = 1000)
parameterEstimates(fit_q1, standardized = TRUE,ci = TRUE) |>
  subset(label %in% c("ind1", "ind2", "ind3", "ind4", "ind5", "total_indirect"))

# === F5/F1 -> 中介 -> F3 ===
model_q2 <- '
  # 測量模型
  F1 =~ v2 + v3 + v5 + v8
  F2 =~ v14 + v16 + v17 + v18
  F3 =~ v21 + v22 + v23 + v24
  F4 =~ v28 + v29 + v30 + v31
  F5 =~ v35 + v37 + v38 + v39 + v44
  F6 =~ v47 + v50 + v52 + v57

  # 中介結構模型
  F4 ~ a1*F5
  F2 ~ a2*F5
  F3 ~ b1*F4 + b2*F2 +  c*F5

  # 間接效果定義（透過每條路徑）
  ind1 := a1 * b1   # F5 → F4 → F3
  ind2 := a2 * b2   # F5 → F2 → F3

  # 總間接效果與總效果
  total_indirect := ind1 + ind2
  total_effect := c+total_indirect
'
fit_q2 <- sem(model_q2, data = data_items, estimator = "MLR")
summary(fit_q2, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)
fit_q1 <- sem(model_q2, data = data_items, se = "bootstrap", bootstrap = 1000)
parameterEstimates(fit_q2_boot, standardized = TRUE, ci = TRUE)[grep("ind|total", parameterEstimates(fit_q3_boot)$label), ]

# === 7. 匯出結果 CSV ===
write.csv(results, "C:\\Users\\huann\\OneDrive\\Desktop\\Education Big Data\\mediation_triplet_results_classified.csv", row.names = FALSE)
