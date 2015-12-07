require(magicaxis)

setEPS()
postscript(file = "~/Documents/ColloidMC_texts/time_dep.eps",
           width = 5.5, height = 5, 
           bg = "white", family = "sans")

matplot(unique(sw),
        ylim = c(0, 1),
        test_op,
        type = "p",
        pch = c(0, 1, 0, 3, 4),
        lwd = 2,
        cex.axis=1.8,
        cex = 2,
        ann = F,
        col = c(1, 2, 0, 4, 6),
        lty = c(1, 1, 0, 1, 1),
        log = "x"
        )
matlines(unique(sw),
         test_op,
         type = "l",
         pch = c(0, 1, 2, 3, 4),
         lty = c(1, 1, 0, 1, 1),
         lwd = 2,
         cex = 2,
         col = c(1, 2, 0, 4, 6))

mtext(side = 2, text = latex2exp('$E_{12}$'), line = 2.5, cex = 1.8)
mtext(side = 1, text = "MC Sweeps", line = 2.5, cex = 1.8)

magaxis(side = 1, labels = F, minorn = 10, family = "sans")

legend("topleft",
       legend = c("E = 2.00, N = 50", "E = 1.93, N = 500", "E = 2.05, N = 5000", "E = 2.06, N = 50000"),
       lwd = 2, col = c(1, 2, 4, 6), pch = c(0, 1, 3, 4), inset=c(0.03, 0.03))

dev.off()