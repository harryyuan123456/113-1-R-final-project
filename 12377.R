# 基本計算機功能
calculator <- function(num1, num2, operator) {
  # 根據操作符進行計算
  result <- switch(operator,
                   "+" = num1 + num2,
                   "-" = num1 - num2,
                   "*" = num1 * num2,
                   "/" = {
                     if (num2 != 0) {
                       num1 / num2
                     } else {
                       "除數不能為零"
                     }
                   },
                   "無效的運算符"
  )
  return(result)
}

# 使用範例
num1 <- as.numeric(readline(prompt = "輸入第一個數字: "))
num2 <- as.numeric(readline(prompt = "輸入第二個數字: "))
operator <- readline(prompt = "輸入運算符 (+, -, *, /): ")

# 輸出計算結果
cat("計算結果是: ", calculator(num1, num2, operator), "\n")
