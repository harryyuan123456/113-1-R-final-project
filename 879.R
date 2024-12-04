# 匯入必要套件
library(tidyverse)

# 手動輸入數據
data <- tibble(
  年齡別 = c("5歲", "6歲", "7歲", "8歲", "9歲", "10歲", "11歲", "12歲", "13歲", "14歲", 
          "15歲", "16歲", "17歲", "18歲", "19歲", "20歲", "21歲", "22歲以上"),
  `國民小學（人）` = c(13, 19553, 21008, 21833, 20770, 22054, 22100, 65, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  `國民中學（人）` = c(0, 0, 0, 0, 0, 0, 30, 21048, 20161, 21040, 135, 5, 0, 0, 0, 0, 0, 0),
  `高級中等學校（人）` = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 101, 25680, 26360, 25756, 1579, 387, 157, 63, 642)
)

# 數據清理
cleaned_data <- data %>%
  mutate(
    年齡別 = case_when(
      年齡別 == "22歲以上" ~ "22+",
      TRUE ~ 年齡別
    ),
    年齡別 = factor(年齡別, levels = c("5歲", "6歲", "7歲", "8歲", "9歲", "10歲", "11歲",
                                 "12歲", "13歲", "14歲", "15歲", "16歲", "17歲",
                                 "18歲", "19歲", "20歲", "21歲", "22+"))
  )

# 將數據轉換為長格式
long_data <- cleaned_data %>%
  pivot_longer(cols = starts_with("國民"), names_to = "學校級別", values_to = "人數")

# 確保數據中包含所有學校級別，即便某些學校級別人數為零
age_school_distribution <- long_data %>%
  group_by(年齡別) %>%
  filter(人數 == max(人數)) %>%  # 取出每個年齡層人數最多的學校級別
  ungroup()

# 確認數據正確性
print(age_school_distribution)

# 繪製圖表：包含高級中等學校
age_school_distribution %>%
  ggplot(aes(x = 年齡別, y = 人數, fill = 學校級別)) +
  geom_col() +
  scale_fill_manual(
    values = c("國民小學（人）" = "blue", "國民中學（人）" = "orange", "高級中等學校（人）" = "green"),
    breaks = c("國民小學（人）", "國民中學（人）", "高級中等學校（人）")
  ) +
  labs(
    title = "各年齡層主要分布的學校級別",
    x = "年齡層",
    y = "人數",
    fill = "學校級別"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

