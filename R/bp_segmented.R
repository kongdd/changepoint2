
#' PieceWise Regression
#'
#' @param y Numeric vector
#' @param year.origin first year of y
#' @param predict If true, returned predicted data of piecewise regression.
#' @param bp If provide, changing point detected will not applied.
#' 
#' @importFrom segmented davies.test segmented seg.control
#' @export
bp_segmented <- function(y, bp = NULL){
    n <- length(y)
    middle <- floor(n/2)
    pvalue <- NA

    if (is.null(bp)) {
        if (length(unique(y)) > 3){#如果全部为0,则把该点数据剔除
            x <- seq_along(y)
            lm_fit <- lm(y ~x)
            # test beta2 significant
            dtest <- davies.test(lm_fit, ~x, k = 10)
            pvalue <- dtest$p.value # pvalue: smaller better
            #如果dtest返回的有brk，则将此brk作为segmented的起始值
            brk0 = floor(dtest$statistic[[1]])
            # brk_init <- tryCatch(), error = function(e)middle)
            piece_fit <- segmented(lm_fit, seg.Z = ~x, bp = brk0
                # control = seg.control(stop.if.error = FALSE, n.boot = 0, it.max = 100)
            )
            psi <- piece_fit$psi
            if ( !is.null(psi) && (length(psi) >= 1)){ #判断是否存在突变点
                bp <- floor(psi[2]) # Initial, Est., St.Err
                #bp <- ifelse(is.na(bp), 16, bp)#断点不存在时处理
            } else {
                bp <- tryCatch(floor(dtest$statistic[[1]]), error = function(e) NA)
            }
        }
        bp %<>% check_bp(n, 5)
    }
    data.frame(bp = bp, pvalue = pvalue)
}
