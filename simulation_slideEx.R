outcomes <- c("heads", "tails")
N <- 10000
sim_coin <- numeric(N)
for(i in 1:N){
  trial <- sample(outcomes, size = 70, replace = TRUE)
  sim_coin[i] <- sum(trial=="heads")
}
hist(sim_coin, breaks=(-1):70)
sim_coin
