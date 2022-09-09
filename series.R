# Primeira questão


# Item A 
# Autoregressão
set.seed(3)
w = rnorm(150,0,1) # gerando white noise
x_a = filter(w, filter=c(0,-.9), method="recursive")[-(1:50)] # gerando AR(2)

# MA
p = 4
ma_a = filter(x_a, rep(1/p, p), sides = 1) # médias móveis

# Plot
par(mfrow=c(1,1))
plot(x_a, xlab = "Time", ylab = "Series", lty = 1, type="l")
lines(ma_a, pch = 18, col = "blue", type = "l", 
      lty = 2, lwd = 1)
legend("bottomleft", legend = c("Autoregression", "Moving average"),
       col = c("black", "blue"), lty = 1:2, cex = 0.7)






#Item B
set.seed(3)
x_b = c(cos(2*pi*1:100/4)) # geranta x_t

q = 4
# MA
ma_b = filter(x_b, rep(1/q, q), sides = 1) # médias móveis

# Plot
par(mfrow=c(1,1))
plot(x_b, xlab = "Time", ylab = "Series", lty = 1, type="l")
lines(ma_b, pch = 18, col = "blue", type = "l", 
      lty = 2, lwd = 1)
legend("bottomleft", legend = c("Deterministic", "Moving average"),
       col = c("black", "blue"), lty = 1:2, cex = 0.7)


# Item C
set.seed(5)
w_1 = w[-(1:50)] # 100 observações de ruído branco
x_c = x_b + w_1 # gerando o novo x_t

# MA
r = 4
ma_c = filter(x_c, rep(1/r, r), sides = 1)

# plot
plot(x_c, xlab = "Time", ylab = "Series", lty = 1, type="l")
lines(ma_c, pch = 18, col = "blue", type = "l", 
      lty = 2, lwd = 1)
legend("bottomleft", legend = c("Deterministic + WN", "Moving average"),
       col = c("black", "blue"), lty = 1:2, cex = 0.7)


# Em geral, ao aplicar as médias móveis o gráfico da série tende a "suavizar", aumentando a ordem do modelo (variáveis p, q e r)
# o gráfico tende a se aproximar de uma reta constante em 0. Também pode ser observaado que a presença do white noise pertuba
# o modelo (comparando o gráfico do item b com o do item c), para valores de q = r, o gráfico da b é evidentemente mais
# próximo de uma reta que o gráfico da c