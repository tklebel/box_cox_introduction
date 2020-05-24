library(tidyverse)

# try with one sample y
df <- tibble(y = rchisq(100, 20),
             y_log  = log(y),
             x = y + rnorm(100, mean = 0, sd = 10),
             x_2 = y,
             y_best = (y^.263 - 1)/.263)

# plot_them <- function(df) {
#   df %>%
#     select(-x) %>%
#     pivot_longer(everything()) %>%
#     ggplot(aes(value, colour = name)) +
#     geom_density()
# }
# qplot(df$y)
#
# plot_them(df)

m1 <- lm(y ~ x, data = df)
res <- car::boxCox(m1)
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


# show different transformations ----
# try with one sample y
df <- tibble(y = rchisq(1000, 10),
             x = y + rnorm(1000, mean = 0, sd = 2))

df %>%
  mutate(lambda = list(seq(-2, 2, by = .5))) %>%
  unnest(lambda) %>%
  mutate(y_trans = case_when(
    lambda == 0 ~ log(y),
    TRUE ~ (y^lambda - 1)/lambda)) %>%
  pivot_longer(c(y, y_trans)) %>%
  ggplot(aes(value, colour = name)) +
  geom_freqpoly(binwidth = 1) +
  facet_wrap(~lambda, scales = "free") +
  coord_cartesian(ylim = c(0, 400), xlim = c(0, 30))



# try with gganimate
library(gganimate)
df <- data.frame(y = rchisq(n = 500, df = 5))
transform_box_cox <- function(y, lambda) {
  (y ^ lambda - 1)/lambda
}



df <- df %>%
  mutate(lambda = list(seq(-2, 2, by = .1))) %>%
  unnest(lambda) %>%
  mutate(y_trans = case_when(lambda == 0 ~ log(y),
                             TRUE ~ transform_box_cox(y, lambda)))



ggplot(df, aes(y_trans)) +
  geom_density() +
  geom_density(aes(y), colour = "red") +
  coord_cartesian(xlim = c(0, 40)) +
  facet_wrap(~lambda, scales = "free_y")

# , ylim = c(0, .2)
#

anim <- ggplot(df, aes(y_trans)) +
  geom_density() +
  geom_density(aes(y), colour = "red") +
  view_follow(fixed_x = c(0, 40)) +
  transition_states(lambda) +
  ease_aes() +
  theme_bw() +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  labs(title = "Transformation in Abhängigkeit von Lambda",
       subtitle = "Lambda = {closest_state}", x = NULL, y = "Dichte")


animate(anim, nframes = 400, duration = 20, end_pause = 0,
        start_pause = 10, rewind = TRUE)

# for final anim
animate(anim, nframes = 500, duration = 20, end_pause = 0,
        start_pause = 10, rewind = TRUE, height = 5, width = 9, units = "in",
        res = 300)
save_animation(file = "dist_change")


# create static figure
library(tidyverse)
library(ggtext)

transform_box_cox <- function(y, lambda) {
  (y ^ lambda - 1)/lambda
}

set.seed(202005)
df <- tibble(y = rchisq(n = 500, df = 5))

df <- df %>%
  mutate(lambda = list(seq(-2, 2, by = .5))) %>%
  unnest(lambda) %>%
  mutate(y_trans = case_when(lambda == 0 ~ log(y),
                             TRUE ~ transform_box_cox(y, lambda)))

ggplot(df, aes(y_trans)) +
  geom_density(fill = "#e41a1c", alpha = .5) +
  geom_density(aes(y), fill = "#377eb8", alpha = .5) +
  facet_wrap(~lambda, scales = "free_y") +
  coord_cartesian(xlim = c(0, 40)) +
  theme_bw() +
  theme(plot.subtitle = element_markdown()) +
  labs(title = "Transformation von Y in Abhängigkeit von Lambda",
       subtitle = "<span style='color:#377eb8'>Originale</span> vs. <span style='color:#e41a1c'>transformierte</span> Variable", x = NULL, y = "Dichte")


