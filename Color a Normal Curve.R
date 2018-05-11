
cord.x <- c(-5,seq(-5,-2,0.01),-2) 
cord.y <- c(0,dnorm(seq(-5,-2,0.01)),0) 
curve(dnorm(x,0,1),xlim=c(-5,5),main='Tail Risk',xlab = " ",ylab=" ",axis) 
polygon(cord.x,cord.y,col='red')

cord.x <- c(2,seq(2,5,0.01),5) 
cord.y <- c(0,dnorm(seq(2,5,0.01)),0) 
polygon(cord.x,cord.y,col='darkgreen')

cord.x <- c(-2,seq(-2,2,0.01),2) 
cord.y <- c(0,dnorm(seq(-2,2,0.01)),0) 
polygon(cord.x,cord.y,col='limegreen')
text(0,.15,"Most\nMonte Carlo\nScenarios",col='white')

text(-3.5,.1,"Thar Be Dragons",col = "red")
text(3.7,.1,"Thar Be\nPowerBall Winners",col = "darkgreen")
arrows(-3.5,.08,-3.5,0.005,length=.1,angle=30,col ='red')
arrows(4,.07,4,0.005,length=.1,angle=30,col ='darkgreen')
