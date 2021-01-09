# # 为保证分段回归数据的长度，对突变点位置进行调整
# bp_adjust <- function(x) {
#     if (x[1] < 5) {
#         x[1] <- 5
#     } else if (x[1] > 28) {
#         x[1] <- 28
#     }
#     x # quickly return
# }

#' At least K elements before or after breakpoint `bk`
#'
#' NA bp will return NA
#' @export
check_bp <- function(bp, n = 34, k = 5) {
    pmin(bp[1], n - k + 1) %>% pmax(k)
}
