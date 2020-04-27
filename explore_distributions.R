library(tidyverse)

# try with one sample y
df <- tibble(y = rchisq(1000, 10),
             y_log  = log(y),
             x = y + rnorm(1000, mean = 0, sd = 20),
             y_best = (y^.263 - 1)/.263)

plot_them <- function(df) {
  df %>%
    select(-x) %>%
    pivot_longer(everything()) %>%
    ggplot(aes(value, colour = name)) +
    geom_density()
}
qplot(df$y)

plot_them(df)

res <- car::boxCox(my_model)
tibble(x = res$x, y = res$y) %>%
  arrange(desc(y))



df %>%
  mutate(y_1 = (y^1 - 1)/1,
         y_01 = (y^-1 - 1)/-1,
         y_best = (y^.303 - 1)/.303) %>%
  plot_them() +
  coord_cartesian(xlim = c(-10, 10))


df %>%
  ggplot(aes(x, y)) +
  geom_point() +
  geom_smooth()
df %>%
  ggplot(aes(x, y_best)) +
  geom_point() +
  geom_smooth()

m1 <- lm(y ~ x, data = df)
m2 <- lm(y_best ~ x, data = df)

m1
m2
summary(m1)
summary(m2)


sum(m1$residuals^2)
sum(m2$residuals^2)


AIC(m1, m2)


