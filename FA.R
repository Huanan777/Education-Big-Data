library(lavaan)
library(psych)
library(factoextra)
setwd("C:\\Users\\huann\\OneDrive\\Desktop\\Education Big Data\\原始資料")
data1 <- read.csv("data_1.csv", stringsAsFactors = FALSE)
sum(is.na(data1))
data1_complete <- na.omit(data1)
data_items <- data1_complete[ ,grep("^v[0-9]+$" ,names(data1_complete))]
eigenvalues <- eigen(cor(data_items))$values
print(eigenvalues)


pca_result <- fa(r = data_items, nfactors = 6, rotate = "varimax")
print(pca_result)

pa_result <- fa(r = data_items, nfactors = 6, rotate = "varimax" ,fm = "pa")

ml_result <- fa(r = data_items, nfactors = 6, rotate = "varimax" ,fm = "ml")


# 定義分析方法的名稱與對應 fa() 參數
methods <- list(
  minres = "minres",
  ml = "ml",
  pa = "pa"
)

# 建立空的 list 存結果
item_counts <- list()

# 根據每種方法執行因素分析，抓出 max loading 所屬因子
for (method in names(methods)) {
  result <- fa(data_items, nfactors = 6, rotate = "varimax", fm = methods[[method]])
  loadings <- as.data.frame(unclass(result$loadings))
  # 將每題歸屬於 loading 最大的因子
  loadings$max <- colnames(loadings)[apply(abs(loadings), 1, which.max)]
  # 計算每個因子底下有幾題
  item_counts[[method]] <- table(loadings$max)
}

# 主成分分析 (PCA)
pca_result <- principal(data_items, nfactors = 6, rotate = "varimax")
loadings_pca <- as.data.frame(unclass(pca_result$loadings))
loadings_pca$max <- colnames(loadings_pca)[apply(abs(loadings_pca), 1, which.max)]
item_counts[["pca"]] <- table(loadings_pca$max)

# 整理成資料框
count_df <- do.call(cbind, item_counts)
count_df[is.na(count_df)] <- 0  # 補 NA 為 0
count_df

