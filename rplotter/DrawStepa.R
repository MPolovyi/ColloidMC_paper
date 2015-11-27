E <- function(th1, th2, dth, dz, A, k) {
  (A*exp(-k*dz)/dz) - (3*cos(th1)*cos(th2) - cos(dth))/(dz^3)
}

rad <- function(degree){
  degree * 180/pi
}

colors <- c("red", "green", "blue", "orange", "blueviolet")
setEPS()
postscript(file = "~/Documents/ColloidMC_texts/k=10_perp.eps",
    width = 5.5, height = 5, 
    bg = "white")

A <- 1000
k <- 10
color <- colors[1]
plot(seq(0, 2, length=20),
     E(rad(0), rad(90), rad(90), seq(0, 2, length=20), A, k),
     ylim=c(-1, 5),
     xlim=c(0.3, 2),
     lwd=2,
     col=color,
     xaxs='i',
     cex.axis=1.8,
     cex = 1.8,
     lty=0,
     ann = F)

mtext(side = 2, text = latex2exp('$E_{12}$'), line = 2.5, cex = 1.8)
mtext(side = 1, text = latex2exp('$\\Delta z$'), line = 2.5, cex = 1.8)

lines(seq(0, 2, length=1000),
      E(rad(0), rad(90), rad(90), seq(0, 2, length=1000), A, k),
      col=color,
      type = "l", lwd = 2)

A <- 800
k <- 10
color <- colors[2]
pc <- 2
lines(seq(0, 2, length=20),
     E(rad(0), rad(90), rad(90), seq(0, 2, length=20), A, k),
     ylim=c(-1, 5),
     xlim=c(0.3, 2),
     lwd=2,
     col=color,
     type='o',
     pch=pc,
     xaxs='i',
     cex = 1.8,
     lty=0,
     ylab=latex2exp('$E_{12}$'),
     xlab=latex2exp('$\\Delta z$'))

lines(seq(0, 2, length=1000),
      E(rad(0), rad(90), rad(90), seq(0, 2, length=1000), A, k),
      col=color,
      type = "l", lwd = 2)

A <- 500
k <- 10
color <- colors[3]
pc <- 3
lines(seq(0, 2, length=20),
     E(rad(0), rad(90), rad(90), seq(0, 2, length=20), A, k),
     ylim=c(-1, 5),
     xlim=c(0.3, 2),
     lwd=2,
     col=color,
     type='o',
     pch=pc,
     xaxs='i',
     cex = 1.8,
     lty=0,
     ylab=latex2exp('$E_{12}$'),
     xlab=latex2exp('$\\Delta z$'))

lines(seq(0, 2, length=1000),
      E(rad(0), rad(90), rad(90), seq(0, 2, length=1000), A, k),
      col=color,
      type = "l", lwd = 2)

A <- 300
k <- 10
color <- colors[4]
pc <- 4
lines(seq(0, 2, length=20),
     E(rad(0), rad(90), rad(90), seq(0, 2, length=20), A, k),
     ylim=c(-1, 5),
     xlim=c(0.3, 2),
     lwd=2,
     col=color,
     type='o',
     pch=pc,
     xaxs='i',
     cex = 1.8,
     lty=0,
     ylab=latex2exp('$E_{12}$'),
     xlab=latex2exp('$\\Delta z$'))

lines(seq(0, 2, length=1000),
      E(rad(0), rad(90), rad(90), seq(0, 2, length=1000), A, k),
      col=color,
      type = "l", lwd = 2)

A <- 100
k <- 10
color <- colors[5]
pc <- 5
lines(seq(0, 2, length=20),
     E(rad(0), rad(90), rad(90), seq(0, 2, length=20), A, k),
     ylim=c(-1, 5),
     xlim=c(0.3, 2),
     lwd=2,
     col=color,
     type='o',
     cex = 1.8,
     pch=pc,
     xaxs='i',
     lty=0
     )

lines(seq(0, 2, length=1000),
      E(rad(0), rad(90), rad(90), seq(0, 2, length=1000), A, k),
      col=color,
      type = "l", lwd = 2)

legend(
  x=1.25,
  y=5,
  legend = c("A = 1000", "A = 800", "A = 500", "A = 300", "A = 100"),
  col = colors,
  lwd = c(2, 2, 2, 2, 2),
  pch = c(0, 2, 3, 4, 5)
)

dev.off()