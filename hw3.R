bags <- c(0, 1, 2)
Lug_0 <- 0
Lug_1 <- 30
Lug_2 <- Lug_1 +35

baggage_fees <- c(Lug_0, Lug_1, Lug_2)
baggage_per_bag <- c(0.49, 0.35, 0.16)

E_revenue <- baggage_fees * baggage_per_bag *130
Ex <- sum(E_revenue)
Ex

baggage_variance <- baggage_fees - Ex
baggage_variance <- baggage_variance^2 * baggage_per_bag
Variance2 <- sum(baggage_variance)
sd <- Variance2^(1/2)
sd
