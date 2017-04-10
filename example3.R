### Example 3 from 'On label dependence and loss minimization in multi-label classificatino' ###
library(tidyverse)

example3 <- function(n = 10, alpha = pi/4, plot = TRUE, err = 0.1) {

  x <- runif(n = n*2, min = -1, max = 1)
  X <- matrix(x, ncol = 2)
  colnames(X) <- paste0("X", 1:2)
  
  beta1 <- c(0, 1, 0)
  beta2 <- c(0, tan(alpha), -1)
  
  y <- cbind(1, X) %*% cbind(beta1, beta2)
  err1_sample <- sample(c(-1, 1), prob = c(err, 1-err), size = n, replace = TRUE)
  err2_sample <- sample(c(-1, 1), prob = c(err, 1-err), size = n, replace = TRUE)
  y <- y * cbind(err1_sample, err2_sample)
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