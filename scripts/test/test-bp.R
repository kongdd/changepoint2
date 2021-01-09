library(foreach)
library(iterators)
library(tidyverse)

d <- fread("data-raw/Elasticity_forest_vs_nonforest.csv")
# df = melt(d, "month_scale")
# ggplot(df, aes(month_scale, value)) +
#     facet_wrap(~variable, scale = "free_y") +
#     geom_point()
scales = d$month_scale
mat = d[,-1] %>% as.matrix()
names = colnames(mat) %>% label_tag()

res = foreach(y = mat, name = names, i = icount()) %do% {
    runningId(i)
    r = bp_mcp(y)
    # bp_plot(y, r, main = name)
    r
}

# write_fig({
#     par(mar = c(2, 2, 1, 1), mgp = c(3, 0.6, 0), mfrow = c(3, 4))
#     res = foreach(x = mat, name = names) %do% {
#         r = bp_segmented(y)
#         bp_plot(x, r, main = name)
#         r
#     }
# }, "bp.pdf")
