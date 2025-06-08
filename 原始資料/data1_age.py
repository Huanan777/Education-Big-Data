import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.font_manager as fm

# 讀資料
data2 = pd.read_csv("data_2.CSV", encoding="big5")

# 將 age 轉為數字
data2['age'] = pd.to_numeric(data2['age'], errors='coerce')

# 設定中文字體（Windows 通常可用「Microsoft JhengHei」）
plt.rcParams['font.family'] = 'Microsoft JhengHei'

# 畫圖
plt.figure(figsize=(6, 8))
plt.boxplot(data2['age'].dropna())
plt.xticks([1], ["年齡"])  # 指定 X 軸標籤
plt.title("年齡盒鬚圖")
plt.ylabel("年齡")
plt.grid(True)
plt.show()
