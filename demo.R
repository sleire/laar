library(ggplot2)
library(reshape2)

# sigmoid strategy simulation with GBM process
################

# number of simulations
sim <- 10

# GBM price process parameters
mu <- 0.60
sigma <- 0.15
S0 <- 30

# time
Y <- 200
N <- 250 * Y
delta <- Y/N
t <- seq (0, T, length = N + 1)

# simulation of GBM processes
Day <- 1:(N+1)
market <- data.frame(Day)
portfolio <- data.frame(Day)

for (i in 1:sim){
  W <- c(0, cumsum ( sqrt(delta) * rnorm (N)))
  GBM <- S0 * exp(mu * t + sigma * W)
  PFO <- sfpi(q=100, tdate=Day, f=GBM, theta = 50, tcost=0, int=TRUE)$PortfPrice
  market <- cbind(market,GBM)
  portfolio <- cbind(portfolio, PFO)
}

colnames(market) <- c("Day",paste0("GBM",1:sim))
colnames(portfolio) <- c("Day",paste0("PFO",1:sim))


market_melt <-melt(market, id = "Day")
portfolio_melt <-melt(portfolio, id = "Day")

market_melt <- cbind(market_melt,Type="Market")
portfolio_melt <- cbind(portfolio_melt,Type="Portfolio")

ggplot(market_melt,aes(x = Day, y = value, colour = variable)) +
  geom_line() +
  theme(legend.position="none")

portfolio_melt <-melt(portfolio, id = "Day")

ggplot(portfolio_melt,aes(x = Day, y = value, colour = variable)) +
  geom_line() +
  theme(legend.position="none")
