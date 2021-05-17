#
# this code is inspired from this fantastic resource here: 
# https://www.r-bloggers.com/hidden-markov-model-example-in-r-with-the-depmixs4-package/
#

# load libraries
library(depmixS4)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(dplyr)

# set random seed for reproducibility
set.seed(124)

# set labels for the two activities
state1.name <- "Walking"
state2.name <- "Sitting"

# set sample size
N <- 200
x <- rnorm(N)
y <- rnorm(N)

#mycols <- c("darkmagenta", "turquoise")
mycols <- c("#5270B3","#FF7052")

# data generating process: jump between two states with chance of 10%
true.states <- rep(state1.name,N)
cur <- state1.name
for (i in 2:N) {
  if (runif(n = 1)>.9) {
    if (cur==state1.name) {cur=state2.name} else {cur=state1.name}
  }
  true.states[i] <- cur
}

# add offset to observations depending on state
x[which(true.states==state1.name)] <- x[which(true.states==state1.name)] + 2

draws <- data.frame(Observed=x, state=true.states, time=1:N)

# define function to fit the HMM
fit.hmm <- function(draws){
  
  # HMM with depmix
  mod <- depmix(Observed ~ 1, data = draws, 
                nstates = 2, family = gaussian()) # use gaussian() for normally distributed data
  fit.mod <- fit(mod)
  
  # predict the states by estimating the posterior
  est.states <- posterior(fit.mod)
  head(est.states)
  
  # results
  tbl <- table(est.states$state, draws$state)
  draws$est.state.labels <- c(colnames(tbl)[which.max(tbl[1,])], colnames(tbl)[which.max(tbl[2,])])[est.states$state]
  est.states$time <- 1:N
  colnames(est.states)[2:3] <- c(colnames(tbl)[which.max(tbl[1,])], colnames(tbl)[which.max(tbl[2,])])
  hmm.post.df <- melt(est.states, measure.vars = c(state1.name, state2.name))
  
  # print the table
  print(table(draws[,c("state", "est.state.labels")]))
  
  # return it
  return(list(draws = draws, hmm.post.df = hmm.post.df))
}

library(firatheme)
mytheme <- theme_fira() +
  scale_colour_fira()

mytheme <- ggthemes::theme_tufte()

plot.hmm.output <- function(model.output){
  g0 <- (ggplot(model.output$draws, aes(x = time, y = Observed)) + geom_line() +
           theme(axis.ticks = element_blank(), axis.title.y = element_blank())+ mytheme) %>% ggplotGrob
  g1 <- (ggplot(model.output$draws, aes(x = time, fill = state, col = state)) + 
           geom_bar(stat = "count", alpha = I(0.7)) + 
           scale_fill_manual(values = mycols, name = "State:\n", labels = c(state1.name, state2.name)) +
           scale_color_manual(values = mycols, name = "State:\n", labels = c(state1.name, state2.name)) +
           theme(axis.ticks = element_blank(), axis.text.y = element_blank()) +
           labs(y = "True State") + mytheme) %>% ggplotGrob
  g2 <- (ggplot(model.output$draws, aes(x = time, y = est.state.labels, fill = est.state.labels, col = est.state.labels)) + 
           geom_bar(stat = "identity", alpha = I(0.7)) +
           scale_fill_manual(values = mycols, name = "State:\n", labels = c(state1.name, state2.name)) +
           scale_color_manual(values = mycols, name = "State:\n", labels = c(state1.name, state2.name)) +
           theme(axis.ticks = element_blank(), axis.text.y = element_blank()) + 
           labs(y = "Estimated State") + mytheme) %>% ggplotGrob
  g3 <- (ggplot(model.output$hmm.post.df, aes(x = time, y = value, col = variable)) + geom_line() +
           scale_color_manual(values = mycols, name = "State:\n", labels = c(state1.name, state2.name)) +
           theme(axis.ticks = element_blank(), axis.text.y = element_blank()) + mytheme +
           labs(y = "Posterior")) %>%
    ggplotGrob()
  g0$widths <- g1$widths

  return(grid.arrange(g0, g1,  g3, widths = 1, nrow = 3))
}

hmm1 <- fit.hmm(draws)

plot.hmm.output(hmm1)

