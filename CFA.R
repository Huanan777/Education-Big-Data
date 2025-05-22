setwd("C:\\Users\\huann\\OneDrive\\Desktop\\Education Big Data\\原始資料")

library(lavaan)

# 載入資料
data1 <- read.csv("data_1.csv", stringsAsFactors = FALSE)

# 選出題目欄位（v1 ~ v60）
data_items <- data1[, grep("^v[0-9]+$", names(data1))]

# 處理缺值（直接刪除不完整的個案）
data_items <- na.omit(data_items)

# 建立六因子模型語法
model <- '
  F1 =~ v1 + v2 + v3 + v4 + v5 + v6 + v7 + v8 + v9 + v10 + v11 + v12 + v13 + v53 + v55
  F2 =~ v14 + v15 + v16 + v17 + v18 + v19 + v20
  F3 =~ v21 + v22 + v23 + v24 + v25
  F4 =~ v26 + v27 + v28 + v29 + v30 + v31
  F5 =~ v32 + v33 + v34 + v35 + v36 + v37 + v38 + v39 + v40 + v41 + v42 + v43 + v44
  F6 =~ v45 + v46 + v47 + v48 + v49 + v50 + v51 + v52 + v54 + v56 + v57 + v58 + v59 + v60
'

# 執行確認性因素分析（CFA）
fit <- cfa(model, data = data_items, estimator = "ML")

# 顯示適配度指標（包含 CFI）
fitMeasures(fit, c("cfi", "tli", "rmsea", "srmr"))
