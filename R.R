# 計算敘述統計的函式
calc_stats <- function(data) {
  stats <- data.frame(
    mean = sapply(data, mean, na.rm = TRUE),
    sd = sapply(data, sd, na.rm = TRUE),
    min = sapply(data, min, na.rm = TRUE),
    max = sapply(data, max, na.rm = TRUE)
  )
  return(stats)
}

# 讀取 data1 和 data2 資料集
data1 <- read.csv("data_1.csv", stringsAsFactors = FALSE)
data2 <- read.csv("data_2.CSV", fileEncoding = "Big5", stringsAsFactors = FALSE)

# 找出共同變數
common_vars <- intersect(names(data1), names(data2))

# 選取共同變數進行數值型變數篩選
numeric_data1 <- data1[common_vars][sapply(data1[common_vars], is.numeric)]
numeric_data2 <- data2[common_vars][sapply(data2[common_vars], is.numeric)]

# 計算敘述統計
stats_data1 <- calc_stats(numeric_data1)
stats_data2 <- calc_stats(numeric_data2)

# 添加變數名稱到敘述統計
comparison <- cbind(stats_data1, stats_data2)
rownames(comparison) <- common_vars  # 設置變數名稱為行索引
colnames(comparison) <- c("Mean_data1", "SD_data1", "Min_data1", "Max_data1", 
                          "Mean_data2", "SD_data2", "Min_data2", "Max_data2")

# 查看合併結果
print(comparison)

# 將行名稱 (變數名稱) 轉換為資料框中的獨立欄位
comparison <- cbind(Variable = rownames(comparison), comparison)
rownames(comparison) <- NULL  # 移除行名稱，避免干擾

# 使用 writexl 匯出
if (!require("writexl")) {
  install.packages("writexl")
}
library(writexl)

# 匯出包含變數名稱的資料到 Excel
write_xlsx(comparison, "comparison_stats_with_variables.xlsx")

cat("敘述統計已成功匯出至檔案: comparison_stats_with_variables.xlsx\n")


# 合併資料並添加來源標籤
numeric_data1$source <- "data1"
numeric_data2$source <- "data2"
merged_data <- rbind(numeric_data1, numeric_data2)


library(ggplot2)

# 確認輸出目錄存在或創建目錄來保存圖表
output_dir <- "barplots"  # 輸出的資料夾名稱
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# 確定要繪製的變數列表，排除 "source" 欄位
variables <- setdiff(names(merged_data), "source")

# 迴圈生成每個變數的疊加柱狀圖
for (var in variables) {
  # 動態生成柱狀圖
  p <- ggplot(merged_data, aes_string(x = "source", fill = paste0("as.factor(", var, ")"))) +
    geom_bar(position = "fill") +
    labs(
      title = paste(var, "同意程度分佈"),
      x = "資料集",
      y = "百分比",
      fill = "同意程度"
    ) +
    theme_minimal()
  
  # 儲存圖表為 PNG 格式
  ggsave(
    filename = file.path(output_dir, paste0(var, "_barplot.png")),
    plot = p,
    width = 8,
    height = 6
  )
}

cat("所有變數的疊加柱狀圖已成功匯出至目錄:", output_dir, "\n")


# 繪製疊加柱狀圖
library(ggplot2)
ggplot(merged_data, aes(x = source, fill = as.factor(v1))) +
  geom_bar(position = "fill") +
  labs(title = "v1 同意程度分佈", x = "資料集", y = "百分比", fill = "同意程度") +
  theme_minimal()

# 繪製箱型圖
ggplot(merged_data, aes(x = source, y = v4, fill = source)) +
  geom_boxplot() +
  labs(title = "同意程度 v4 的比較", x = "資料集", y = "v4 數值") +
  theme_minimal()

library(ggplot2)

# 確認輸出目錄是否存在，若不存在則創建
output_dir <- "boxplots"  # 設定輸出目錄名稱
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# 確認要繪製的變數列表，排除 "source" 欄位
variables <- setdiff(names(merged_data), "source")

# 使用迴圈為每個變數繪製箱型圖
for (var in variables) {
  # 動態生成箱型圖
  p <- ggplot(merged_data, aes_string(x = "source", y = var, fill = "source")) +
    geom_boxplot() +
    labs(
      title = paste("同意程度", var, "的比較"),
      x = "資料集",
      y = paste(var, "數值"),
      fill = "資料集"
    ) +
    theme_minimal()
  
  # 儲存圖表為 PNG 格式
  ggsave(
    filename = file.path(output_dir, paste0(var, "_boxplot.png")),
    plot = p,
    width = 8,
    height = 6
  )
}

cat("所有變數的箱型圖已成功匯出至目錄:", output_dir, "\n")

# 保留原始資料順序，將變數設為有序因子
summary_data <- data.frame(
  variable = factor(names(data1)[4:60], levels = names(data1)[4:60], ordered = TRUE), # 確保順序
  mean_data1 = sapply(data1[4:60], mean, na.rm = TRUE),
  mean_data2 = sapply(data2[4:60], mean, na.rm = TRUE)
)

# 繪製點圖並確保 X 軸順序一致
ggplot(summary_data, aes(x = variable)) +
  geom_point(aes(y = mean_data1, color = "data1"), size = 2) +
  geom_point(aes(y = mean_data2, color = "data2"), size = 2) +
  labs(title = "變數平均值比較", x = "變數", y = "平均值") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # 旋轉 X 軸標籤以提高可讀性
