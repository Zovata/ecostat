# 定义x的范围
x <- seq(-2 * pi, 2 * pi, length.out = 100)

# 计算y = cos(x)
y <- cos(x)

# 绘制图像
plot(x, y, type = "l", col = "blue", lwd = 2, 
     xlab = "x", ylab = "y", 
     main = "y = cos(x)")

# 添加网格线以便更好地观察
grid()
exp(-1/10)

exp(1/5)
