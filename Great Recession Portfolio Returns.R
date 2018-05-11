vfinx <- -.50825
treas <- .17995
portReturn <- rep(0,11)
equityAllocation <- rep(0,11)
port.df <- data.frame(portReturn,equityAllocation)

i <- 1

for (eA in seq(0,1,.1)) {
  bondAlloc[i] <- 1- eA
  port.df$equityAllocation[i] <- eA
  port.df$portReturn[i] <- bondAlloc[i] * treas + port.df$equityAllocation[i] * vfinx
  cat("\nPort Return is",bondAlloc[i] * treas + port.df$equityAllocation[i] * vfinx, bondAlloc[i]," ",port.df$equityAllocation[i] )
  i <- i + 1
}

port.df$colour <- ifelse(port.df$portReturn < 0, "negative","positive")
print(
  ggplot (port.df,(aes(x=seq(0,1,.1),y=port.df$portReturn))) +
    geom_bar(stat="identity",position="identity",aes(fill = colour)) +
    scale_fill_manual(values=c(positive="darkgreen",negative="red")) +
    labs(x="Equity Allocation",y="Portfolio Return",title="Great Recession October 2007 - March 2009",subtitle="Portfolio Returns by Asset Allocation") +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(breaks=seq(0,1,.1),labels = scales::percent) +
    theme(legend.position="none")
)