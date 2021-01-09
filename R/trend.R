#' @export 
trend_bp <- function(y, bp) {
    y_a <- y[1:bp]
    y_b <- y[(bp + 1):length(y)]

    trend_a <- mkTrend(y_a)
    trend_b <- mkTrend(y_b)
    trend_all <- mkTrend(y)
    trend <- rbind(trend_a, trend_b, trend_all) %>%
        data.table(period = c("before", "after", "whole"), .)

    n <- length(y)
    pred <- NULL
    if (predict) {
        pred <- predict_mk(trend_a, trend_b, n, bp)
    }
    listk(trend, pred)
}

#' @export
predict_mk <- function(trend_a, trend_b, n, bp) {
    x_a <- (1:bp)
    x_b <- ((bp + 1)):n
    y_a <- trend_a["slp"] * x_a + trend_a["intercept"]
    y_b <- trend_b["slp"] * x_b + trend_b["intercept"]

    d <- rbind(
        data.table(x = x_a, y = y_a, period = "before"),
        data.table(x = x_b, y = y_b, period = "after")
    )
    d
}
