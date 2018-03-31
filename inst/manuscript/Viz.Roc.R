#' Generate an ROC plot of this pkgs internal model data
#'
#' @return returns a ggplot obj with roc curve labels
#' @export
#' @import ggplot2
#' @importFrom vizR "facet_"
#' @importFrom magrittr "%<>%"
#'
#' @examples
#' Viz.Roc()
Viz.Roc <- function() {
    data(roc_tbl, package="candi")

    data(perf_tbl, package="candi")
    roc_tbl$indication %<>% as.factor()

    DATA <- roc_tbl

    LABELS <- list(
        x = "False Alarm Rate",
        y = "Sensitivity",
        col = "Indication",
        title = glue("ROC curves for models trained on various cardiothoracic imaging indications")
    )

    gg_roc_layers <- function() {
        list(
            geom_abline(alpha=0.2),
            geom_line(),
            coord_equal(expand=FALSE)
        )
    }

    p <- DATA %>%
        {
            ggplot(., aes(x=x, y=y, col=fct_reorder2(indication, x, y))) +
                gg_roc_layers()
        }

    p + theme(axis.text=element_blank(),
              axis.ticks =element_blank(),
              strip.text.y = element_text(angle=180),
              legend.position = "right",
              legend.direction = "vertical"
        )

    p + do.call(labs, LABELS)
}
