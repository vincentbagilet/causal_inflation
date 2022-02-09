library(tidyverse)
library(retrodesign)
library(mediocrethemes)

set_mediocre_all(pal = "coty", gradient = "left")

exaggeration <- tibble(effect = 1:30) %>%
  # crossing(sd = seq(5, 35, 5)) %>% 
  crossing(sd = seq(1, 10, 1)) %>% 
  group_by(sd) %>% 
  mutate(
    exaggeration = retrodesign(effect, unique(sd))$exaggeration,
    snr = effect/sd,
    sd = factor(sd)
  ) %>% 
  ungroup()

exaggeration %>% 
  filter(snr < 10) %>%
  filter(exaggeration < 5) %>% 
  ggplot(aes(x = snr, y = exaggeration, color = sd)) +
  geom_line(size = 0.8) 
  # geom_vline(xintercept = 1.96)


