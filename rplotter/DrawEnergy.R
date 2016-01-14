require(latex2exp)
E <- function(th1, th2, dth, dz, A, k) {
  (A*exp(-k*dz)/dz) - (3*cos(th1)*cos(th2) - cos(dth))/(dz^3)
}

rad <- function(degree){
  degree * 180/pi
}

colors <- c("red", "green", "blue", "orange", "blueviolet")

addLines <- function( A_, k_, color_, theta1_, theta2_, delta_ ){
  lines(
    seq(0, 2, length = 1000),
    E(rad(theta1_), rad(theta2_), rad(delta_), seq(0, 2, length = 1000), A_, k_),
    col = colors[color_] , lwd = 2)
  
  points(
    seq(0, 2, length = 20),
    E(rad(theta1_), rad(theta2_), rad(delta_), seq(0, 2, length = 20), A_, k_),
    pch = color_-1, col = colors[color_], lwd = 2)
}

setEPS()
postscript(file = "~/Documents/ColloidMC_texts/Images/particle_interaction_potential.eps",
    width = 9, height = 7, 
    bg = "white")

A_toPlot <- c(1000, 800, 500, 300, 100)
k_toPlot <- c(3, 5, 8, 10, 1000)

par(mfrow = c(2, 2))
par(cex = 1.5)
par(mar = c(0, 0, 0, 0), oma = c(2, 2, 0.6, 0.6))
par(xpd=F)
par(ann = F)

xlim_ <- c(0, 2)
ylim_ <- c(-10, 10)

plot(NULL, NULL, ylim = ylim_, xlim = xlim_, xaxs = "i", axes = F)
axis(2, pos = 0.07, col = 0, at = c(-10, -5, 0, 5, 10))
axis(2, labels = F, at = c(-10, -5, 0, 5, 10))
axis(1, at = c(-100, 100))
axis(2, at = c(-100, 100))
axis(3, at = c(-100, 100))
axis(4, at = c(-100, 100))
mtext("(a)", side = 3, line = -1.1, cex = 1.8, adj = 0.95)

for(i in seq_along(A_toPlot)){
  addLines(A_toPlot[i], 10, i, 0, 0, 0)
}

plot(NULL, NULL, ylim = ylim_, xlim = xlim_, xaxs = "i", axes = F)
axis(1, at = c(-100, 100))
axis(3, at = c(-100, 100))
axis(4, at = c(-100, 100))
mtext("(b)", side = 3, line = -1.1, cex = 1.8, adj = 0.95)

for(i in seq_along(k_toPlot)){
  addLines(1000, k_toPlot[i], i, 0, 0, 0)
}

xlim_ <- c(0, 2)
ylim_ <- c(0, 5)

plot(NULL, NULL, ylim = ylim_, xlim = xlim_, xaxs = "i", axes = F)
axis(1, pos = 0.2, col = 0, at = c(0, 0.5, 1, 1.5))
axis(2, pos = 0.07, col = 0, at = seq(0, 4))

legend("topright",
       legend = sprintf("A = %1i", A_toPlot),
       col = colors,
       pch = seq_along(k_toPlot)-1, bty = "n", lwd = 2, lty = 0, inset = c(0, 0.15))

axis(1, labels = F, at = c(0, 0.5, 1, 1.5))
axis(2, labels = F, at = seq(0, 4))
axis(1, labels = F, tick = F, at = c(1.5, 2))
axis(1, at = c(-100, 100))
axis(2, at = c(-100, 100))
axis(4, at = c(-100, 100))
mtext("(c)", side = 3, line = -1.1, cex = 1.8, adj = 0.95)

for(i in seq_along(A_toPlot)){
  addLines(A_toPlot[i], 10, i, 90, 0, 90)
}

plot(NULL, NULL, ylim = ylim_, xlim = xlim_, xaxs = "i", axes = F)
axis(1, pos = 0.2, col = 0, at = c(0, 0.5, 1, 1.5, 2))

legend("bottomleft",
       legend = sprintf("k = %1i", k_toPlot),
       col = colors,
       pch = seq_along(k_toPlot)-1, inset = c(-0.02, -0.03), bty = "n", lwd = 2, lty = 0)

for(i in seq_along(k_toPlot)){
  addLines(1000, k_toPlot[i], i, 90, 0, 90)
}

axis(1, labels = F, at = c(0, 0.5, 1, 1.5, 2))
axis(1, at = c(-100, 100))
axis(2, at = c(-100, 100))
axis(4, at = c(-100, 100))
mtext("(d)", side = 3, line = -1.1, cex = 1.8, adj = 0.95)

dev.off()