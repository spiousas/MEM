pacman::p_load(tidyverse, ISLR, corrplot, Metrics, caret, pROC)

# genero funcion que a partir de las frecuencias de las 4 metricas me de todas las
# metricas de evaluacion de un clasificador
recall <- function(tp, fn) {
  tp/(tp+fn)
}

precision <- function(tp, fp) {
  tp/(fp+tp)
}

fpr <- function(fp, tn) {
  fp / (fp+tn)
}

f1 <- function(tp, fp, fn) {
  recall    <- tp/(tp+fn)
  precision <- tp/(fp+tp)
  
  (2*recall*precision) / (recall+precision)
}

tabla <- tibble(th = seq(0,1,0.1),
                tp = c(50,48,47,45,44,42,36,30,20,12,0),
                fp = c(50,47,40,31,23,16,12,11,4,3,0),
                tn = c(0,3,9,16,22,29,34,38,43,45,50),
                fn = c(0,2,4,8,11,13,18,21,33,40,50)
                ) %>%
  mutate(recall = recall(tp, fn),
         precision = precision(tp, fp),
         fpr = fpr(fp, tn),
         f1 = f1(tp, fp, fn))

tabla %>%
  select(c("th", "precision", "recall", "f1")) %>%
  pivot_longer(cols = precision:f1, names_to = "metrica", values_to = "valor") %>%
  ggplot(aes(x = th,
             y = valor,
             color = metrica)) +
  geom_point(size =3) +
  geom_line(linewidth = 2, alpha = .7) +
  labs(x = "Umbral", Y = "Metrica", title = "Metricas en función del umbral", color = NULL) +
  scale_color_brewer(palette = "Dark2") +
  coord_equal(xlim = c(0,1),
              ylim = c(0,1)) +
  theme_bw() +
  theme(legend.position = "bottom")

tabla %>%
  ggplot(aes(x = fpr,
             y = recall)) +
  geom_point(size =3, color = "steelblue") +
  geom_line(linewidth = 2, alpha = .7, color = "steelblue") +
  geom_area(alpha = .2,  fill = "steelblue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = "FPR", Y = "TPR", title = "ROC curve") +
  coord_equal(xlim = c(0,1),
              ylim = c(0,1)) +
  theme_bw()

# Ejemplo del mnarket ####
data("Smarket")
attach(Smarket)

cor(Smarket[, -9])
corrplot(cor(Smarket[, -9]), type = "lower", diag = FALSE, method = "ellipse")

fit <- glm(
  Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
  data = Smarket,
  family = binomial
)
summary(fit)

pred <- predict(fit, type = "response") > 0.5
(t <- table(ifelse(pred, "Up (pred)", "Down (pred)"), Smarket$Direction))

train <- (Smarket$Year < 2005)
Smarket.2005 <- Smarket[!train, ]
dim(Smarket.2005)
Direction.2005 <- Smarket$Direction[!train]

glm.fits <- glm(
  Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
  data = Smarket, family = binomial, subset = train
)
glm.probs <- predict(glm.fits, Smarket.2005,
                     type = "response")
glm.pred <- predict(glm.fits, Smarket.2005,
                    type = "response") > 0.5

cat.pred <- factor(ifelse(glm.pred, "Up", "Down"))

(t <- table(ifelse(glm.pred, "Up (pred)", "Down (pred)"), Direction.2005))

mean(cat.pred == Direction.2005)
mean(cat.pred != Direction.2005)

CM <- confusionMatrix(cat.pred, Direction.2005)
CM$table
acc <- Metrics::accuracy(cat.pred, Direction.2005)
acc

ROC <- roc(Direction.2005, 
           glm.probs, 
           plot = TRUE, 
           legacy.axes = TRUE,
           percent = FALSE, xlab = "1 - Especificidad",
           ylab = "Sensibilidad", col = "#377eb8", lwd = 2,
           print.auc = TRUE)