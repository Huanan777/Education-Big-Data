# 安裝必要套件
install.packages("psych")
install.packages("GPArotation")

library(psych)

# 載入資料
data1 <- read.csv("data_1.csv", stringsAsFactors = FALSE)

# 篩選題目欄位 v1~v60
data_items <- data1[, grep("^v[0-9]+$", names(data1))]

# 去除 NA 或補值（你可選擇）
data_items <- na.omit(data_items)

# 建立結果儲存列表, minres:最小平方法, ml:主軸抽取法, pa:最大似然法 
methods <- c("minres", "ml", "pa")
results_list <- list()


for (m in methods) {
  fa_res <- fa(data_items, nfactors = 6, rotate = "varimax", fm = m)
  loadings <- as.data.frame(unclass(fa_res$loadings))
  loadings$max <- colnames(loadings)[apply(abs(loadings), 1, which.max)]
  item_table <- table(loadings$max)
  metrics <- data.frame(
    method = m,
    RMSEA = fa_res$RMSEA[1],
    TLI = fa_res$TLI,
    RMSR = fa_res$rms
  )
  results_list[[m]] <- list(metrics = metrics, items = loadings)
}

# PCA 額外處理
pca_res <- principal(data_items, nfactors = 6, rotate = "varimax")
loadings_pca <- as.data.frame(unclass(pca_res$loadings))
loadings_pca$max <- colnames(loadings_pca)[apply(abs(loadings_pca), 1, which.max)]
metrics_pca <- data.frame(
  method = "pca",
  RMSEA = NA,
  TLI = NA,
  RMSR = pca_res$rms
)
results_list[["pca"]] <- list(metrics = metrics_pca, items = loadings_pca)

# 彙整成表格
all_metrics <- do.call(rbind, lapply(results_list, function(x) x$metrics))
all_items <- do.call(rbind, lapply(names(results_list), function(name) {
  df <- results_list[[name]]$items
  df$method <- name
  df$item <- rownames(df)
  return(df[, c("item", "max", "method")])
}))

library(tidyr)
item_groups <- pivot_wider(all_items, names_from = method, values_from = max)
colnames(item_groups) <- c("item", "minres_group", "ml_group", "pa_group", "pca_group")


# 輸出查看
print(all_metrics)
print(head(all_items, 10))
