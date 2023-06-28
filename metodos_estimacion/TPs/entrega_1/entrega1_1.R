library(here)
library(tidyverse)

data <- read_csv(here("metodos_estimacion/TPs/entrega_1/entrega1_1-data.csv")) %>%
  pivot_longer(cols = !NOMBRE, names_to = "n", values_to = "p") %>%
  mutate(n = as.numeric(n))
data

pdf(file = here("metodos_estimacion/TPs/entregas/figs/entrega1_1-boxplot.pdf"),  
    width = 8,
    height = 6)

data %>% ggplot(aes(x = factor(n),
           y = p,
           color = factor(n))) +
  geom_boxplot() +
  labs(title = "Boxplot paralelos del estimador p (22 alumnos)",
       x = "N",
       y = "Estimación de p") +
  theme_bw() +
  theme(legend.position = "none")

dev.off()

pdf(file = here("metodos_estimacion/TPs/entregas/figs/entrega1_1-histogram.pdf"),  
    width = 8,
    height = 8)
  
data %>% mutate(N=factor(n)) %>%
  ggplot(aes(x = p,
             fill = N)) +
  geom_histogram() +
  labs(title = "Histogramas del estimador p (22 alumnos)",
       x = "Estimación de p") +
  facet_grid(N~., labeller = label_both) +
  theme_bw() +
  theme(legend.position = "none")

dev.off()
