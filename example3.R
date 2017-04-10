### Example 3 from 'On label dependence and loss minimization in multi-label classificatino' ###
library(tidyverse)
library(MASS)

example3 <- function(n = 10, alpha = pi/4, plot = TRUE, err = 0.1, err.dep = 0) {

  x <- runif(n = n*2, min = -1, max = 1)
  X <- matrix(x, ncol = 2)
  colnames(X) <- paste0("X", 1:2)
  
  alpha <- pi/2 - alpha
  
  beta1 <- c(0, 1, 0)
  beta2 <- c(0, tan(alpha), -1)
  
  y <- cbind(1, X) %*% cbind(beta1, beta2)
  err_sample <- ifelse(mvrnorm(n = n, mu = c(0, 0), 
                               Sigma = matrix(c(1, err.dep, err.dep, 1), ncol = 2)) < qnorm(err), -1, 1)
  
  y <- y * err_sample
  Y <- ifelse(y < 0, 0, 1)
  colnames(Y) <- paste0("Y", 1:2)
  
  p <- NULL
  if(plot) {
    bound_data <- data.frame(X = seq(-1.05, 1.05, len = 500), Y = seq(-1.05, 1.05, len = 500) * tan(alpha))
    
    p <- data.frame(X, Y) %>% 
      ggplot(aes(X1, X2)) + 
      #geom_point(aes(color = factor(Y1), shape = factor(Y2)), size = 5) + 
      theme(line = element_blank(), text = element_blank(), rect = element_blank()) +
      scale_y_continuous(limits = c(-1.05, 1.05), expand = c(0, 0)) +
      scale_x_continuous(limits = c(-1.05, 1.05), expand = c(0, 0)) +
      geom_abline(slope = tan(alpha), intercept = 0, linetype = "dashed") +
      geom_vline(xintercept = 0, linetype = "dashed") +
      geom_text(aes(label = paste0(Y1, Y2))) +
      geom_ribbon(data = bound_data[bound_data$X >= 0,], aes(ymin = Y, ymax = Inf, x = X), inherit.aes = FALSE, alpha = 0.35) +
      geom_ribbon(data = bound_data[bound_data$X < 0,], aes(ymin = -Inf, ymax = Y, x = X), inherit.aes = FALSE, alpha = 0.35)
  }
  
  list(data = data.frame(X, Y), plot = p)
}

example3(n = 20)

library(latex2exp)
library(stringr)

angle <- seq(0, pi, len = 100)
uncon_corr <- lapply(seq(0, 0.5, by = 0.1), function(e) {
  sapply(angle, function(a) {
    temp <- example3(n = 2000, alpha = a, plot = FALSE, err = e)$data[, c(3,4)]
    cor(temp$Y1, temp$Y2)
  })
})

cor_mat <- do.call("cbind", uncon_corr)
colnames(cor_mat) <- paste(seq(0, 0.5, by = 0.1))


data.frame(angle, cor_mat) %>%
  gather(error, correlation, -angle) %>%
  mutate(error = str_replace_all(error, pattern = "X", "")) %>% 
  ggplot(aes(angle, correlation)) +
  geom_line(aes(color = error)) + 
  theme_minimal() +
  scale_color_brewer()+
  #geom_smooth() +
  scale_x_continuous(breaks = c(0, pi/4, pi/2, 3*pi/4, pi), 
                     labels = TeX(c("0", "$\\frac{\\pi}{4}$", "$\\frac{\\pi}{2}$", "$\\frac{3\\pi}{4}$", "$\\pi$")),
                     name = TeX("$\\alpha$"))

uncon_corr <- lapply(seq(0, 1, by = 0.2), function(dep) {
  sapply(angle, function(a) {
    temp <- example3(n = 2000, alpha = a, plot = FALSE, err = 0.25, err.dep = dep)$data[, c(3,4)]
    cor(temp$Y1, temp$Y2)
  })
})

cor_mat <- do.call("cbind", uncon_corr)
colnames(cor_mat) <- paste(seq(0, 1, by = 0.2))


data.frame(angle, cor_mat) %>%
  gather(error.dep, correlation, -angle) %>%
  mutate(error.dep = str_replace_all(error.dep, pattern = "X", "")) %>% 
  ggplot(aes(angle, correlation)) +
  geom_line(aes(color = error.dep)) + 
  theme_minimal() +
  scale_color_brewer()+
  #geom_smooth() +
  scale_x_continuous(breaks = c(0, pi/4, pi/2, 3*pi/4, pi), 
                     labels = TeX(c("0", "$\\frac{\\pi}{4}$", "$\\frac{\\pi}{2}$", "$\\frac{3\\pi}{4}$", "$\\pi$")),
                     name = TeX("$\\alpha$"))

