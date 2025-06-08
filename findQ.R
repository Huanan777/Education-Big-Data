library(lavaan)
library(purrr)

# === 題目分群設定 ===
factor_groups <- list(
  F1 = c("v1" , "v2" , "v3" , "v5" , "v7" , "v8" ,"v9" , "v10" , "v11" , "v13" , "v55"),
  F2 = c("v14" , "v15" , "v16" , "v17" , "v18" , "v19", "v20"),
  F3 = c("v21" , "v22" , "v23" , "v24" , "v25"),
  F4 = c("v26" , "v27" , "v28" , "v29" , "v30" , "v31"),
  F5 = c("v33" , "v35" , "v36" , "v37" , "v38" , "v39" , "v40" , "v41" , "v42","v43", "v44"),
  F6 = c("v46" , "v47" , "v49" , "v50" , "v51" , "v52" , "v54" , "v56" , "v57" , "v59" , "v60")
)

# === 封裝成函數，針對每筆資料執行 ===
run_cfa_analysis <- function(data_name, data_path) {
  library(readr)
  data <- read_csv("data_2.CSV", locale = locale(encoding = "UTF-8"))
  data_items <- data[, grep("^v[0-9]+$", names(data))]
  data_items <- na.omit(data_items)
  
  for (factor_name in names(factor_groups)) {
    item_pool <- factor_groups[[factor_name]]
    results <- data.frame(Factor = character(), Items = character(), Estimator = character(),
                          CFI = numeric(), TLI = numeric(), RMSEA = numeric(), SRMR = numeric(),
                          stringsAsFactors = FALSE)
    
    for (k in 3:length(item_pool)) {
      combos <- combn(item_pool, k, simplify = FALSE)
      
      for (item_set in combos) {
        item_str <- paste(item_set, collapse = " + ")
        model <- paste0(factor_name, " =~ ", item_str)
        
        for (est in c("MLR", "WLSMV")) {
          fit <- tryCatch({
            if (est == "WLSMV") {
              cfa(model, data = data_items, estimator = est, ordered = item_set)
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
    
    # 過濾 & 儲存結果
    results_clean <- subset(results,
                            CFI < 1 & CFI >= 0.9 &
                              TLI <= 1 & TLI >= 0.9 &
                              RMSEA > 0 & RMSEA < 0.08 &
                              SRMR < 0.08)
    results_clean$Item_Count <- sapply(strsplit(results_clean$Items, ","), length)
    
    # 儲存 CSV，區分資料名稱
    output_file <- paste0("C:\\Users\\huann\\OneDrive\\Desktop\\Education Big Data\\", 
                          data_name, "_", factor_name, "_CFI90.csv")
    if (nrow(results_clean) > 0) {
      write.csv(results_clean, output_file, row.names = FALSE)
      cat("✓ Saved:", output_file, "\n")
    } else {
      cat("⚠ No valid model found for", data_name, "-", factor_name, "\n")
    }
  }
}

# === 對 data_1 和 data_2 都執行一次 ===
run_cfa_analysis("data_1", "data_1.csv")
run_cfa_analysis("data_2", "data_2.csv")



# === 6. 將每個構面的 data_1 與 data_2 組合上下貼齊 ===
factors <- names(factor_groups)

for (f in factors) {
  file1 <- paste0("C:\\Users\\huann\\OneDrive\\Desktop\\Education Big Data\\data_1_", f, "_CFI90.csv")
  file2 <- paste0("C:\\Users\\huann\\OneDrive\\Desktop\\Education Big Data\\data_2_", f, "_CFI90.csv")
  
  if (file.exists(file1) && file.exists(file2)) {
    df1 <- read.csv(file1, stringsAsFactors = FALSE)
    df2 <- read.csv(file2, stringsAsFactors = FALSE)
    
    df1$Source <- "data_1"
    df2$Source <- "data_2"
    
    combined <- rbind(df1, df2)
    
    # 依照 Factor + Items + Estimator + Source 排序
    combined <- combined[order(combined$Factor, combined$Items, combined$Estimator, combined$Source), ]
    
    output_file <- paste0("C:\\Users\\huann\\OneDrive\\Desktop\\Education Big Data\\stacked_", f, "_CFI90.csv")
    write.csv(combined, output_file, row.names = FALSE)
    
    cat("✓ Stacked and saved:", output_file, "\n")
  } else {
    cat("⚠ 缺少檔案：", f, "\n")
  }
}


# === 6. 將每個構面的 data_1 與 data_2 組合上下貼齊 ===
factors <- names(factor_groups)

for (f in factors) {
  file1 <- paste0("D:\\Education-Big-Data\\data_1_", f, "_CFI90.csv")
  file2 <- paste0("D:\\Education-Big-Data\\data_2_", f, "_CFI90.csv")
  
  if (file.exists(file1) && file.exists(file2)) {
    df1 <- read.csv(file1, stringsAsFactors = FALSE)
    df2 <- read.csv(file2, stringsAsFactors = FALSE)
  
    merge_data <- merge(df1, df2, by = c("Factor","Items"))
    output_file <- paste0("D:\\Education Big Data\\s_", f, "_CFI90.csv")
    
    write.csv(merge_data, output_file, row.names = FALSE)
      
    cat("✓ Stacked and saved:", output_file, "\n")
  }
  else{
    cat("⚠ 缺少檔案：", f, "\n")
  }
}
