# library(tidyverse)
# library(mediocrethemes)
# 
# set_mediocre_all()
n <- 10000
n_iter <- 300
res <- rep(NA, n_iter)
alpha <- 1
delta <- 1
beta <- 1
gamma <- 1
bw <- 0.2 #bw as the proportion of observations considered below or above the threshold

for (i in 1:n_iter) {
  u <- runif(n, 0, 1)
  x <- rnorm(n, 0, 1)  + delta*u
  treated <-  ifelse(x < median(x), 1, 0)
  in_bw <- dplyr::between(x, quantile(x, 0.3 - bw), quantile(x, 0.3 + bw))
  e <- rnorm(n, 0, 0.5)
  # y <- alpha + rnorm(n, 1, 1)*treated + gamma*x + delta*u + e #non constant effect
  y <- alpha + beta*treated + gamma*x + delta*u^3 + e

  data_sharp <- tibble(x, y, u, treated, in_bw, e)

  res[i] <- lm(y ~ treated + x, data = filter(data_sharp, in_bw)) %>% 
    coef() %>% 
    .[["treated"]]/beta
}

res %>%
  qplot() +
  geom_vline(aes(xintercept = mean(res))) +
  geom_vline(aes(xintercept = 1), linetype = "solid")

mean(res) - 1

#Paper Magdalena Bennett

# n <- 2000
# n_iter <- 500
# res <- rep(NA, n_iter)
# beta_rx <- 1
# beta_ru <- 3
# beta_yx <- 2
# beta_yu <- 0.5
# bw <- 0.5 #bw as the proportion of observations considered below or above the threshold
# 
# for (i in 1:n_iter) {
#   u <- rnorm(n, 0, 10)
#   x <- rnorm(n, 0, 10)
#   r <- beta_rx*x + beta_ru*u + rnorm(n, 0, 1)
#   y0 <- beta_yx*x + beta_yu*u + rnorm(n, 0, 1)
#   tau <- 0.2*var(y0)
#   treated <-  ifelse(r < median(r), 1, 0)
#   in_bw <- dplyr::between(r, quantile(r, 0.5 - bw), quantile(r, 0.5 + bw))
#   # treated <- ifelse(dplyr::between(r, quantile(r, 0.5 - bw), quantile(r, 0.5 + bw)), treated, NA)
#   y <- y0 + tau*treated
# 
#   data_bennett <- tibble(x, y, y0, r, u, treated, in_bw)
# 
#   # qplot(r, y0)
#   # qplot(r, y0, color = factor(treated))
# 
#   res[i] <- lm(y ~ treated*x + r, data = filter(data_bennett, in_bw)) %>% coef() %>% .[["treated"]]
#   res[i] <- res[i]/tau
# }
# 
# res %>%
#     qplot() +
#     geom_vline(aes(xintercept = mean(res))) +
#     geom_vline(aes(xintercept = 1), linetype ="dashed")
# mean(res) - 1


#fuzzy RDD
# n <- 10000
# n_iter <- 1000
# res <- rep(NA, n_iter)
# alpha <- 1
# delta <- 4
# beta <- 1
# gamma <- 1
# bw <- 0.35 #bw as the proportion of observations considered below or above the threshold
# 
# for (i in 1:n_iter) {
#   u <- rnorm(n, 0, 0.05)
#   x <- rnorm(n, 0, 1)
#   treated <- rbernoulli(n, ifelse(x < median(x), 1 + delta*u*x^2, delta*u*x^2))
#   # treated <- (x < 0)
#   in_bw <- dplyr::between(x, quantile(x, 0.5 - bw), quantile(x, 0.5 + bw))
#   e <- rnorm(n, 0, 0.5)
#   y <- alpha + beta*treated + gamma*x + delta*u + e
# 
#   data_fuzzy <- tibble(x, y, u, treated, in_bw, e)
#   # data_fuzzy %>%
#   #   ggplot(aes(x = x, y = y, color = in_bw)) +
#   #   geom_point()
# 
#   res[i] <- lm(y ~ treated*x, data = filter(data_fuzzy, in_bw)) %>% coef() %>% .[["treatedTRUE"]]/beta
# }
# 
# res %>%
#   qplot() +
#   geom_vline(aes(xintercept = mean(res))) +
#   geom_vline(aes(xintercept = 1), linetype ="dashed")
# 
# print(mean(res) - 1)









  

  
  
  
  
  
  
  


