rb <- boxplot(decrease ~ treatment, data = OrchardSprays, col="bisque")
title("Comparing boxplot()s and non-robust mean +/- SD")
mn.t <- tapply(OrchardSprays$decrease, OrchardSprays$treatment, mean)
sd.t <- tapply(OrchardSprays$decrease, OrchardSprays$treatment, sd)
xi <- 0.2 + seq(rb$n)

points(xi, mn.t, col = "orange")

arrows(xi, mn.t - sd.t, xi, mn.t + sd.t, code = 3, col = "orange")