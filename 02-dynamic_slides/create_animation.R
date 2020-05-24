library(tidyverse)
library(gganimate)
library(ggtext)

set.seed(202005)
df <- tibble(y = rchisq(n = 500, df = 5))

transform_box_cox <- function(y, lambda) {
  (y ^ lambda - 1)/lambda
}


df <- df %>%
  mutate(lambda = list(seq(-2, 2, by = .1))) %>%
  unnest(lambda) %>%
  mutate(y_trans = case_when(lambda == 0 ~ log(y),
                             TRUE ~ transform_box_cox(y, lambda)),
         lambda = as.factor(lambda))

anim <- ggplot(df, aes(y_trans)) +
  geom_density(fill = "#e41a1c", alpha = .5) +
  geom_density(aes(y), fill = "#377eb8", alpha = .5) +
  view_follow(fixed_x = c(0, 40)) +
  transition_states(lambda) +
  ease_aes() +
  theme_bw() +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        plot.caption = element_markdown()) +
  labs(title = "Transformation in Abhängigkeit von Lambda",
       subtitle = "Lambda = {closest_state}", x = NULL, y = "Dichte",
       caption = "<span style='color:#377eb8'>Originale</span> vs. <span style='color:#e41a1c'>transformierte</span> Variable")

# animate(anim, nframes = 200, duration = 20, end_pause = 5,
#         start_pause = 0, rewind = TRUE)
# anim_save(file = "dist_change_test.gif")
animate(anim, nframes = 400, duration = 20, end_pause = 0,
        renderer = gifski_renderer(),
        start_pause = 10, rewind = TRUE, height = 5, width = 9, units = "in",
        res = 300)
anim_save(file = "dist_change.gif")
