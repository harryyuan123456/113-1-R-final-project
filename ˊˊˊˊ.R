library(tidyverse)

# 讀取檔案
file_path <- "e2270c3b-2f9a-458b-b582-c661eb83aeb6.csv"
loan_data <- read_csv(file_path)

# 資料整理：轉為長格式
tidy_data <- loan_data %>%
  pivot_longer(
    cols = starts_with("公立"):starts_with("私立"),
    names_to = "學校類型",
    values_to = "數值"
  ) %>%
  rename(貸款類型 = 項目) %>%
  mutate(學年度 = as.numeric(學年度))

# 檢查整理後的資料
glimpse(tidy_data)

# 繪製貸款金額趨勢圖
tidy_data %>%
  filter(貸款類型 == "貸款金額（元）") %>%
  ggplot(aes(x = 學年度, y = 數值, color = 學校類型, group = 學校類型)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "不同學校類型貸款金額趨勢",
    x = "學年度",
    y = "貸款金額（元）"
  )

# 繪製貸款比例分佈圖
tidy_data %>%
  group_by(學年度, 貸款類型) %>%
  mutate(比例 = 數值 / sum(數值)) %>%
  ggplot(aes(x = 學年度, y = 比例, fill = 學校類型)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "貸款比例分佈",
    x = "學年度",
    y = "比例"
  )

# 計算年增率
growth_rate <- tidy_data %>%
  group_by(貸款類型, 學校類型) %>%
  arrange(學年度) %>%
  mutate(年增率 = (數值 - lag(數值)) / lag(數值)) %>%
  filter(!is.na(年增率))

# 檢查年增率
print(growth_rate)
