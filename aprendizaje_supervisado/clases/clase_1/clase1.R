# Notion de gonza
# https://mem23.notion.site/Guias-1b0f6aa6247e4b37a03098d79408d974

### Ejercicio en clase

set.seed(1)
n_train = 50 
n_test = 500
sigma = 1

x_train = data.frame( x = runif(n_train, -1, 1) )
y_train = f(data_train$x) +rnorm(n_train, sd = sigma) 

x_test = data.frame( x = runif(n_test, -1, 1) )
y_test = f(x_test$x) + rnorm(n_test, sd = sigma)

f = function(x){10*x*(x-1)*(x+1)}
xx = seq(-1, 1, 0.05)

plot(x_train$x, y_train)
lines(xx, f(xx), lwd = 2)

K = 5

library(FNN)

y_hat_knn5 = knn.reg( x_train, test = data.frame(x = xx)  , y = y_train, k = K)$pred
K = 10
y_hat_knn10 = knn.reg(x_train, test = data.frame(x = xx)  , y = y_train, k = K)$pred
K = 2
y_hat_knn2 = knn.reg(x_train, test =data.frame(x = xx)  , y = y_train, k = K)$pred

lines(xx, y_hat_knn5, col ='orange', lwd = 2)
lines(xx, y_hat_knn10, col ='magenta', lwd = 2)
lines(xx, y_hat_knn2, col ='blue', lwd = 2)

Ks = 1:20
k_test_mse = rep(0, length(Ks))
k_train_mse = rep( 0, length(Ks))

for (j in 1:20) {
   k_test_mse[j] = mean( (y_test -  knn.reg(x_train, test =x_test, 
                                             y = y_train, k = Ks[j])$pred)^2 ) 
   k_train_mse[j] = mean((y_train -  knn.reg(x_train, test =x_train, 
                                            y = y_train, k = Ks[j])$pred)^2 )
}

plot( Ks, k_test_mse, type = 'l', ylim =c(0, max(k_test_mse)), col = 'orange', ylab = 'MSE')
lines(Ks, k_train_mse, col = 'blue')

k_min = Ks[ which.min(k_test_mse)]

points( k_min, k_test_mse[which.min(k_test_mse)], col ='orange')

## Que sucede con la curva del MSE si aumenta la varianza de los errores? Piense primero y despues verifique.