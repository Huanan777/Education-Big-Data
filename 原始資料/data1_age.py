import pandas as pd
import matplotlib.pyplot as plt

# === 1. 讀取資料 ===
df = pd.read_csv("data_1.CSV", encoding="big5")

# === 2. 年級轉中文標籤對應表 ===
grade_label_map = {
    1: "一年級",
    2: "二年級",
    3: "三年級",
    4: "四年級",
    5: "延畢",
    6: "研究生",
    11: "二年制專班一年級",
    12: "二年制專班二年級",
    13: "二年制專班延畢"
}

# === 3. 數值轉中文 + 計數 ===
df['grade'] = pd.to_numeric(df['grade'], errors='coerce')
df['grade_label'] = df['grade'].map(grade_label_map)
grade_counts = df['grade_label'].value_counts().reindex(grade_label_map.values()).dropna()

# === 4. 畫圖（放大畫布＋標籤旋轉）===
plt.figure(figsize=(12, 6))  # ✅ 放大畫布
grade_counts.plot(kind='bar', color='lightblue')

plt.title("年級長條圖( data_1 )", fontsize=16)
plt.xlabel("年級", fontsize=14)
plt.ylabel("人數", fontsize=14)
plt.xticks(rotation=45, ha='right', fontsize=12)  # ✅ 標籤旋轉 + 靠右 + 放大
plt.yticks(fontsize=12)
plt.grid(axis='y')
plt.tight_layout()
plt.show()
