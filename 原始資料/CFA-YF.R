setwd("D:\\Education-Big-Data\\原始資料")
library(lavaan)

# 載入資料
data1 <- read.csv("data_2.csv", stringsAsFactors = FALSE, fileEncoding = "big5")

# 選出題目欄位（v1 ~ v60）
data_items <- data1[, grep("^v[0-9]+$", names(data1))]

# 處理缺值（直接刪除不完整的個案）
data_items <- na.omit(data_items)

# 建立六因子模型語法 F5 F3 F2 F4 F1 F6
model <- '
  F1 =~ v7 + v8 + v10 + v11 + v13
  F2 =~ v14 + v16 + v17 + v18
  F3 =~ v21 + v22 + v23 + v24
  F4 =~ v28 + v29 + v30 + v31
  F5 =~ v33 + v37 + v38 + v39 + v40 + v41
  F6 =~ v46 + v47 + v50 +v52 + v54 + v57
'

# 執行確認性因素分析（CFA）
fit <- cfa(model, data = data_items, estimator = "MLR")


# 顯示適配度指標（包含 CFI）
fitMeasures(fit, c("cfi", "tli", "rmsea", "srmr"))


