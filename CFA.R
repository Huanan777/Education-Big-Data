# === 0. 設定環境 ===
setwd("C:\\Users\\huann\\OneDrive\\Desktop\\Education Big Data\\原始資料")

library(lavaan)
library(gtools)

# === 1. 載入資料與預處理 ===
data1 <- read.csv("data_2.csv", stringsAsFactors = FALSE, fileEncoding = "big5")
data_items <- data1[, grep("^v[0-9]+$", names(data1))]
data_items <- na.omit(data_items)

# === 2. CFA 檢查模型適配度（非必要但可以保留）===
model <- '
  F1 =~ v2 + v7 + v8 + v10 + v11  
  F2 =~ v14 + v16 + v17 + v18 
  F3 =~ v21 + v22 + v23 + v24 
  F4 =~ v27 + v28 + v30 + v31
  F5 =~ v33 + v37 + v39 + v40 + v41 + v44
  F6 =~ v46 + v50 + v51 + v52 + v54 + v57 
'
fit <- cfa(model, data = data_items, estimator = "WLSM")
fitMeasures(fit, c("cfi", "tli", "rmsea", "srmr"))

# === 3. 定義題目對應的潛在變項 ===
latent_items <- list(
  F1 = c("v2", "v7", "v8", "v10", "v11"),
  F2 = c("v14", "v16", "v17", "v18"),
  F3 = c("v21", "v22", "v23", "v24"),
  F4 = c("v27", "v28", "v30", "v31"),
  F5 = c("v33", "v37", "v39", "v40", "v41", "v44"),
  F6 = c("v46", "v50", "v51", "v52", "v54", "v57")
)

# === 4. 建立所有 X ➝ M ➝ Y 的排列組合 ===
factors <- names(latent_items)
triplets <- permutations(n = 6, r = 3, v = factors, repeats.allowed = FALSE)

# === 5. 建立結果儲存表 ===
results <- data.frame(
  X = character(),
  M = character(),
  Y = character(),
  Indirect = numeric(),
  Indirect_p = numeric(),
  Total = numeric(),
  Total_p = numeric(),
  CFI = numeric(),
  TLI = numeric(),
  RMSEA = numeric(),
  SRMR = numeric(),
  stringsAsFactors = FALSE
)

# === 6. 跑 120 組中介模型 ===
for (i in 1:nrow(triplets)) {
  x <- triplets[i, 1]
  m <- triplets[i, 2]
  y <- triplets[i, 3]
  
  x_items <- paste(latent_items[[x]], collapse = " + ")
  m_items <- paste(latent_items[[m]], collapse = " + ")
  y_items <- paste(latent_items[[y]], collapse = " + ")
  
  model <- paste0('
    ', x, ' =~ ', x_items, '
    ', m, ' =~ ', m_items, '
    ', y, ' =~ ', y_items, '

    ', m, ' ~ a*', x, '
    ', y, ' ~ b*', m, ' + c_prime*', x, '

    indirect := a*b
    total := c_prime + (a*b)
  ')
  
  fit <- tryCatch({
    sem(model, data = data_items, estimator = "WLSM")
  }, error = function(e) NULL)
  
  if (!is.null(fit) && lavInspect(fit, "converged")) {
    est <- parameterEstimates(fit, standardized = TRUE)
    
    ind_row <- est[est$label == "indirect", ]
    tot_row <- est[est$label == "total", ]
    
    if (nrow(ind_row) > 0 && nrow(tot_row) > 0) {
      fit_index <- fitMeasures(fit, c("cfi", "tli", "rmsea", "srmr"))
      
      results <- rbind(results, data.frame(
        X = x,
        M = m,
        Y = y,
        Indirect = round(ind_row$est, 4),
        Indirect_p = round(ind_row$pvalue, 4),
        Total = round(tot_row$est, 4),
        Total_p = round(tot_row$pvalue, 4),
        CFI = round(fit_index["cfi"], 4),
        TLI = round(fit_index["tli"], 4),
        RMSEA = round(fit_index["rmsea"], 4),
        SRMR = round(fit_index["srmr"], 4)
      ))
      
      cat("✓ Done:", x, "→", m, "→", y, "\n")
    } else {
      cat("⚠ Skipped (no indirect/total):", x, "→", m, "→", y, "\n")
    }
  } else {
    cat("⚠ Failed model:", x, "→", m, "→", y, "\n")
  }
}

# === 7. 輸出分析結果 CSV ===
write.csv(results, "C:\\Users\\huann\\OneDrive\\Desktop\\Education Big Data\\mediation_triplet_results_with_p.csv", row.names = FALSE)
