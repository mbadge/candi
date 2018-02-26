#' ---
#' title: "Generating JAMIA Classifier Perf Figs/Tbls"
#' author: mabadgeley
#' ---
#'
#' Render this R script to html by running (from parent dir):
#' `render_rmd.r -f generate_figures.R -m final`
#'
#' # JAMIA GUIDELINES
#' ## Images
#' https://academic.oup.com/journals/pages/authors/figures
#' Must be uploaded as separate files
#' Preferred file format .tif or .eps
#' line drawings: 600 DPI
#' Color images: 300 DPI
#' ### Sizes:
#' * Single col: 3.54 in width

#' Configure journal figure size options
save_figure_1col <- partial(cowplot::ggsave, width=3.54, units="in", dpi=600)
save_figure_2col <- partial(cowplot::ggsave, width=7.09, units="in", dpi=600)

#' legends at the end of the manuscript
#' appendices should be uploaded using the file designation Supplementary File and cited in main text
#'
#' ## Tables:
#' word format and placed in main text where the table is first cited
#'
#' ## Data:
#' Deposit data in public repo Dryad

devtools::load_all()

# Set side effects
theme_set(cowplot::theme_cowplot())


#' # Output
# Save svg for each mutability, and tiff b/c that's the preferred journal format
SAVE_TYPES <- c("svg", "tiff")

#' ## ROC curves
F_STEM <- "rocs"
save_fps <- map_chr(SAVE_TYPES, ~str_c(F_STEM, .x, sep="."))

leg <- guide_legend("Imaging Indicaiton", direction="vertical", ncol=2)
rocs <- Viz.Roc() %+%
    ggplot2::ggtitle("") %+%
    theme(legend.position = "bottom") %+%
    guides(color=leg)

roc <- rocs %+% theme(plot.margin = margin(0, 1.5, 0, 0, "cm"))
walk(save_fps, save_figure_1col, plot=roc)


#' ## Perf table
# I'll copy paste into word for the real submission, for now make html tables to paste to doc
perf_tbl
