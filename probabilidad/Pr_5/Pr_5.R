y_0 <- c(0.0519, 0.1779, 0.2668, 0.2287, 0.1225, 0.0420, 0.0090, 0.0011, 0.0001)
y_1 <- c(0.0000, 0.0000, 0.0001, 0.0009, 0.0046, 0.0147, 0.0294, 0.0336, 0.0168)

# a ####
Py_0 <- sum(y_0)
Py_0

# b ####
Py_1 <- sum(y_1)
Py_1
Py_1 <- 1 - Py_0
Py_1

# c ####
g <- c(0,0,0,0,1,1,1,1,1)

sum(y_0[g!=0]) + sum(y_1[g!=1])
