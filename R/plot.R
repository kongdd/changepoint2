# bp_plot(x, r, main = "bp")
#' @import data.table
bp_plot <- function(y, d_bp, pred, main = "bp") {
    # par(mar = c(3, 2, 2, 1), mgp = c(3, 0.6, 0), mfrow = c(4, 4))
    
    plot(y, pch = 16, col = "grey")
    pvalue = r$bp[2]
    lty = ifelse(pvalue <= 0.05, 1, 2)
    abline(v = r$bp$brk + 0.5, lty = lty, lwd = 1)
    legend("topright", legend = main, bty = "n", cex = 1)
    # lines(x)
    pred <- r$pred

    lines(y ~ x, pred[period == "before"], col = "red")
    lines(y ~ x, pred[period == "after"], col = "blue")
}
