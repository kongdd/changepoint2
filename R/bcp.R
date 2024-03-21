library(bcp)

bp_bcp <- function(x, ...) {
  l = bcp(x, ...)
  prob = l$posterior.prob
  data.table(I = 1:length(prob), prob) %>%
    .[prob > 0.8, ]
  # inds =  %>% {which(. > 0.8)}
  # inds
}
