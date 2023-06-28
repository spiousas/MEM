x <- scan()
y <- scan()
nivel <- 0.92

# Asumo que las varianzas son iguales
t.test(x, y, var.equal = T, conf.level = nivel)

