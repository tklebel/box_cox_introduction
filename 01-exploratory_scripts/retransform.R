pdata <- expand_grid(
  x = 0:50,
  lambda = seq(-1.5, 1.5, by = .5),
  beta = .1
) %>%
  mutate(diff_y = case_when(lambda == 0 ~ beta/x,
                            TRUE ~ beta*(lambda*beta*x+1)^(1/lambda-1))) %>%
  drop_na() %>%
  filter(!is.infinite(diff_y))

# skimr::skim(pdata)
ggplot(pdata, aes(x, diff_y, colour = as.factor(lambda))) +
  geom_line() +
  geom_point() +
  scale_y_log10() +
  scale_color_brewer(palette = "Set1")
