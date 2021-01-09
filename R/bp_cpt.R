#' @export 
bp_cpt <- function(x) {
  if (all(is.na(x))) {
    data.frame(mean = NA, mean_conf = NA, var = NA, var_conf = NA) # quickly return
  } else {
    cpt_mean <- cpt.mean(x, method = "AMOC", class = F) # mean changepoint
    cpt_var <- cpt.var(x, method = "AMOC", class = F) # var changepoint

    # 调整断点以保证分段回归的长度
    cpt_mean <- cpt_adjust(cpt_mean)
    cpt_var <- cpt_adjust(cpt_var)

    data.frame(mean = cpt_mean[1], mean_conf = cpt_mean[2], var = cpt_var[1], var_conf = cpt_var[2]) # quickly return
  }
}

## ?如何挑选cptmean与cptvar
cpt_select <- function(x, p = 0.9) {
  x_adjust <- x[, 1:2]
  ## 只有一种情况进行调整：cpt.mean不显著，而cpt.var显著
  id <- which(x[, 4] >= 0.9 & x[, 2] < 0.9)
  x_adjust[id, ] <- x[id, 3:4]
  x_adjust # 调整后changePoint
}
