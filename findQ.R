library(lavaan)
library(purrr)

# === 1. 題目分群設定 ===
# factor_groups <- list(
#   F1 = c("v1", "v2", "v3", "v4", "v5", "v9", "v10", "v11", "v13", "v55"),
#   F2 = c("v14", "v15", "v16", "v18", "v19", "v20"),
#   F3 = c("v21", "v22", "v23", "v24", "v25"),
#   F4 = c("v26", "v27", "v28", "v29", "v30", "v31"),
#   F5 = c("v32", "v35", "v36", "v38", "v39", "v40", "v41", "v43", "v44"),
#   F6 = c("v46", "v47", "v49", "v50", "v52", "v56", "v57", "v59", "v60")
# )

factor_groups <- list(
  F1 = c("v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8","v9", "v10", "v11", "v12", "v13", "v53", "v55"),
  F2 = c("v14", "v15", "v16", "v17","v18", "v19", "v20"),
  F3 = c("v21", "v22", "v23", "v24", "v25"),
  F4 = c("v26", "v27", "v28", "v29", "v30", "v31"),
  F5 = c("v32", "v33", "v34","v35", "v36", "v37", "v38", "v39", "v40", "v41", "v42","v43", "v44"),
  F6 = c("v45", "v46", "v47", "v48","v49", "v50", "v51", "v52", "v54", "v56", "v57", "v58", "v59", "v60")
   )



# === 2. 讀資料並處理缺值 ===
data1 <- read.csv("data_1.csv", stringsAsFactors = FALSE)
data_items <- data1[, grep("^v[0-9]+$", names(data1))]
data_items <- na.omit(data_items)

# === 3. 記錄符合 CFI ≥ 0.90 的結果 ===
results <- data.frame(Factor = character(), Items = character(), Estimator = character(),
                      CFI = numeric(), TLI = numeric(), RMSEA = numeric(), SRMR = numeric(),
                      stringsAsFactors = FALSE)

# === 4. 跑每個構面 ===
for (factor_name in names(factor_groups)) {
  item_pool <- factor_groups[[factor_name]]
  
  for (k in 3:length(item_pool)) {
    combos <- combn(item_pool, k, simplify = FALSE)
    
    for (item_set in combos) {
      item_str <- paste(item_set, collapse = " + ")
      model <- paste0(factor_name, " =~ ", item_str)
      
      for (est in c("MLR", "WLSMV")) {
        fit <- tryCatch({
          if (est == "WLSMV") {
            cfa(model, data = data_items, estimator = est, ordered= item_set)
          } else {
            cfa(model, data = data_items, estimator = est)
          }
        }, error = function(e) NULL)
        
        if (!is.null(fit) && lavInspect(fit, "converged")) {
          fit_index <- fitMeasures(fit, c("cfi", "tli", "rmsea", "srmr"))
          if (fit_index["cfi"] >= 0.90) {
            results <- rbind(results, data.frame(
              Factor = factor_name,
              Items = paste(item_set, collapse = ","),
              Estimator = est,
              CFI = fit_index["cfi"],
              TLI = fit_index["tli"],
              RMSEA = fit_index["rmsea"],
              SRMR = fit_index["srmr"]
            ))
          }
        }
      }
    }
  }
}

# === 5. 輸出結果 ===
results_clean <- subset(results,
                        CFI < 1 & CFI>=0.9    &              # 排除完美（但可能虛假的）CFI
                          TLI <= 1 & TLI>=0.9   &                # TLI 超過 1 通常為錯誤
                          RMSEA > 0 & RMSEA < 0.08 &    # RMSEA 太小（0）或太大都不理想
                          SRMR < 0.08                 # SRMR 太大代表模型誤差大
)

results_clean$Item_Count <- sapply(strsplit(results_clean$Items, ","), length)
View(results_clean)  # 可用來看表格
write.csv(results_clean, "C:\\Users\\huann\\OneDrive\\Desktop\\Education Big Data\\results1_CFI90.csv", row.names = FALSE)

d1 <- read.csv('results1_CFI90.csv')
d2 <- read.csv('results2_CFI90.csv')
# 假設 df1 和 df2 是你要合併的資料框
merged_data <- merge(d1, d2, by = c("Factor", "Items"), all = TRUE)
write.csv(merged_data, "C:\\Users\\huann\\OneDrive\\Desktop\\Education Big Data\\CFI90.csv", row.names = FALSE)

