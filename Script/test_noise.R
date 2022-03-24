library(tidyverse)

n <- 10000

dat <- tibble(id = 1:n) %>% 
  mutate(
    x = rnorm(n),
    z = rnorm(n),
    w = rnorm(n),
    u = rnorm(n, 0, 0.1),
    y = 3 + 0.8*x + 0.2*z + 0.1*w + u
  )

reg_x <- lm(data = dat, y ~ x) %>% broom::tidy()
reg_xz <- lm(data = dat, y ~ x + z) %>% broom::tidy()
reg_xzw <- lm(data = dat, y ~ x + z + w) %>% broom::tidy()

reg_x
reg_xz
reg_xzw


# dat %>%
#   ggplot() +
#   geom_point(aes(x = x, y = y))
