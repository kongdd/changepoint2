#' @references
#' https://cran.r-project.org/web/packages/mcp/index.html
#' https://lindeloev.github.io/mcp/
bp_mcp <- function(y, isplot = TRUE) {
  df <- data.table(y = as.numeric(y), x = seq_along(y))
  # Define the model
  model <- list(
    y ~ 1 + x, # plateau (int_1)
    ~0 # joined slope (time_2) at cp_1
  )
  # Fit it. The `ex_demo` dataset is included in mcp. Sample the prior too.
  # options(mc.cores = 3)  # Uncomment to speed up sampling
  fit <- mcp(model, data = df, sample = "both")
  s <- summary(fit) # See parameter estimates
  par <- s %>% subset(name == "cp_1", )

  if (isplot) {
    p <- plot(fit)
    p <- p + geom_vline(xintercept = par$mean, linewidth = 1)
    print(p)
  }
  par
}
