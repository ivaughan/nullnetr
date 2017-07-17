#' Plot the resource preferences of a consumer
#'
#' Takes a 'nullnet' object from running a null model with
#'   \code{generate_null_net} and plots the observed and expected link
#'   strengths for every resource for a selected consumer species (node in
#'   the network).  The function is based on \code{\link[graphics]{barplot}}, so
#'   see \code{?barplot} for further options for customising the plots.
#'
#' @param nullnet An object of class nullnet from \code{generate_null_net}
#' @param node A string specifying the consumer node (species) whose
#'   preferences will be plotted
#' @param signif.level An optional value specifying the threshold used for
#'   testing for 'significant' deviations from the null model.  Defaults to 0.95
#' @param bar.type Optional string to specify what the bars show. The default
#'   (\code{bar.type = "counts"}) is the total number of interactions or mean
#'   interaction strength, depending upon which one was requested in the
#'   original call to \code{\link{generate_null_net}} using the
#'   \code{summary.type} argument.  The alternative is the standardised effect
#'   size (\code{bar.type = "SES"}).  With \code{bar.type = "counts"}, confidence
#'   limits will be shown as error bars, whilst for the standardised effect size,
#'   dashed lines are added at +2 and -2, which is approximately equivalent to 5\%
#'   significance level.  Both types of bars will be coloured if \code{bar.colours}
#'   is specified.
#' @param res.order An optional data frame used to set the order in which the
#'   resource species are plotted.  Should have two columns: the first listing
#'   the resource species (names must be identical to those used by
#'   \code{generate_null_net}) and the second the plotting order (bars will be
#'   plotted in ascending order)
#' @param bar.colours An optional character vector of length three specifying
#'   the colours with which to fill bars representing interactions that are
#'   weaker than expected, consistent with the null model and stronger than
#'   expected (in that order).  If this argument is not included, the bars will
#'   be unfilled.
#' @param bar.colors Alternative spelling for \code{bar.colours}
#' @param ... Other arguments to control basic plotting functions in R, such as
#'   \code{xlab}, \code{ylab}, \code{main}, \code{xlim} and \code{ylim}.
#'
#' @details Plots the preferences for individual consumer species, following the
#'   basic format of King \emph{et al}. (2010; Figure 3) and Davey \emph{et al}.
#'   (2013; Figure 3).
#'
#' @seealso \code{\link{generate_null_net}}, \code{\link{plot_bipartite}}
#'
#' @references Brewer, C.A. (2017) \url{http://www.ColorBrewer.org}
#'
#'   Davey, J.S., Vaughan, I.P., King, R.A., Bell, J.R., Bohan, D.A., Bruford,
#'   M.W., Holland, J.M. & Symondson, W.O.C. (2013) Intraguild predation in
#'   winter wheat: prey choice by a common epigeal carabid consuming spiders.
#'   \emph{Journal of Applied Ecology}, \strong{50}, 271--279.
#'
#'   King, R.A, Vaughan, I.P., Bell, J.R., Bohan, D.A, & Symondson, W.O.C.
#'   (2010) Prey choice by carabid beetles feeding on an earthworm community
#'   analysed using species- and lineage-specific PCR primers. \emph{Molecular
#'   Ecology}, \strong{19}, 1721--1732.
#'
#'   Vaughan, I.P., Gotelli, N.J., Memmott, J., Pearson, C.E.,
#'   Woodward, G. and Symondson, W.O.C. (2017) nullnetr: an R package using null
#'   models to analyse the structure of ecological networks and identify
#'   resource selection (submitted)
#'
#' @examples
#' null.1 <- generate_null_net(WelshStreams[, 2:18], WelshStreams.prey[, 2:17],
#'                             sims = 10, c.samples = WelshStreams[, 1],
#'                             r.samples = WelshStreams.prey[, 1])
#' # Generate plot using three-level diverging colour scheme to fill the bars
#' #   (Brewer 2017), increasing the lower margin so that names fit
#' par(mar = c(9, 4, 4, 2) + 0.1)
#' plot_preferences(null.1, "Dinocras", signif.level = 0.95,
#'                  bar.colours = c("#998EC3", "#F7F7F7", "#F1A340"),
#'                  bar.type = "counts", ylab = "Num. of visits")
#'
#' # Same results, this time showing the standardised effect sizes
#' plot_preferences(null.1, "Rhyacophila", signif.level = 0.95,
#'                  bar.colours = c("#998EC3", "#F7F7F7", "#F1A340"),
#'                  bar.type = "SES", ylab = "SES")
#'
#' @export


plot_preferences <- function(nullnet, node, signif.level = 0.95,
                             bar.type = "counts", res.order = NULL,
                             bar.colours = NULL, bar.colors = NULL, ...) {
  # ======================================
  # PART 1: link-level tests
  # ======================================
  # --------------------------------------
  # Set significance level
  if (signif.level <= 0 || signif.level >= 1) {
    stop("Invalid percentile value specified")}
  p <- (1 - signif.level) / 2
  # --------------------------------------

  # --------------------------------------
  # Initial error handling:
  # 1. Return an error if a object from 'generate_null_net' is not supplied
  if(class(nullnet) != "nullnet") {
    stop("plot_preferences requires a nullnet object")}

  # 2. Bar type <> counts or SES
  if(bar.type != "counts" && bar.type != "SES") {
    stop("Invalid bar type specified")}

  # 3. If a 'res.order' variable is specified, check that names match those
  #    in the nullnet object
  if (!is.null(res.order)) {
    if(!identical(sort(colnames(nullnet$rand.data[, 3:ncol(nullnet$rand.data)])),
       as.character(sort(res.order[, 1])) )) {
    stop("Names in 'res.order' do not match resource names in nullnet object")}
  }
  # --------------------------------------

  # --------------------------------------
  # Summarise modelled interactions
  null.mean <- stats::aggregate(nullnet$rand.data[, 3:ncol(nullnet$rand.data)],
                                list(nullnet$rand.data$Consumer), mean)
  null.upp <- stats::aggregate(nullnet$rand.data[, 3:ncol(nullnet$rand.data)],
                               list(nullnet$rand.data$Consumer), stats::quantile,
                               probs = 1 - p)
  null.low <- stats::aggregate(nullnet$rand.data[, 3:ncol(nullnet$rand.data)],
                               list(nullnet$rand.data$Consumer), stats::quantile,
                               probs = p)
  sd.inters <- stats::aggregate(nullnet$rand.data[, 3:ncol(nullnet$rand.data)],
                                list(nullnet$rand.data$Consumer), stats::sd)
  # --------------------------------------

  # --------------------------------------
  # Convert to long format using 'melt' ('reshape2' package)
  obs.inters <- reshape2::melt(nullnet$obs.interactions, id.vars = "Consumer")
  colnames(obs.inters)[2] <- "Resource"
  null.mean <- reshape2::melt(null.mean, id.vars = "Group.1")
  colnames(null.mean)[c(1, 2)] <- c("Consumer", "Resource")
  null.low <- reshape2::melt(null.low, id.vars = "Group.1")
  colnames(null.low)[c(1, 2)] <- c("Consumer", "Resource")
  null.upp <- reshape2::melt(null.upp, id.vars = "Group.1")
  colnames(null.upp)[c(1, 2)] <- c("Consumer", "Resource")
  sd.inters <- reshape2::melt(sd.inters, id.vars = "Group.1")
  colnames(sd.inters)[c(1, 2)] <- c("Consumer", "Resource")
  # --------------------------------------

  # --------------------------------------
  # Merge the observed interaction strengths with those from the null model,
  #   the upper and lower confidence limits and standard deviation
  output <- merge(obs.inters, null.mean, by.x = c("Consumer", "Resource"),
                  by.y = c("Consumer", "Resource"))
  colnames(output)[c(3, 4)] <- c("Observed", "Null")
  output <- merge(output, null.low, by.x = c("Consumer", "Resource"),
                  by.y = c("Consumer", "Resource"))
  colnames(output)[5] <- paste('Lower.', signif.level * 100, '.CL', sep = "")
  output <- merge(output, null.upp, by.x = c("Consumer", "Resource"),
                  by.y = c("Consumer", "Resource"))
  colnames(output)[6] <- paste('Upper.', signif.level * 100, '.CL', sep = "")
  output <- merge(output, sd.inters, by.x = c("Consumer", "Resource"),
                  by.y = c("Consumer", "Resource"))
  colnames(output)[7] <- "sd"
  # --------------------------------------

  # --------------------------------------
  # Test significance, comparing observed link strength to the CLs
  output$Test <- rep(0)
  for (i in 1:nrow(output)) {
	  if (output$Observed[i] > output$Upper[i]) {output$Test[i] <- "Stronger"}
	  if (output$Observed[i] < output$Lower[i]) {output$Test[i] <- "Weaker"}
	  if (output$sd[i] == 0) {output$Test[i] <- "NA"}
  }
  # --------------------------------------

  # --------------------------------------
  # Calculate the standard effect sizes for observed v. null interactions, then
  #   remove the sd (no longer needed)
  output$SES <- with(output, (Observed - Null) / sd)
  output$SES[!is.finite(output$SES)] <- NA # Replaces Inf and NaN with NA
  output$sd <- NULL
  # --------------------------------------


  # ======================================
  # PART 2: preference plots
  # ======================================
  # --------------------------------------
  # Subset of the sp-level results only containing the predator of interest
  plot.data <- output[output$Consumer == node, ]
  # --------------------------------------

  # --------------------------------------
  # Vector of fill colours for the bars. Set up to allow alternative spelling
  #   of colours/colors
  if(!is.null(bar.colours)) {
    plot.data$Prefs <- rep(bar.colours[2], nrow(plot.data))
    for (i in 1:nrow(plot.data)) {
      if (plot.data$Test[i] == "Stronger") {plot.data$Prefs[i] <- bar.colours[3]}
      if (plot.data$Test[i] == "Weaker") {plot.data$Prefs[i] <- bar.colours[1] }
    }
  } else if (!is.null(bar.colors)) {
    plot.data$Prefs <- rep(bar.colors[2], nrow(plot.data))
    for (i in 1:nrow(plot.data)) {
      if (plot.data$Test[i] == "Stronger") {plot.data$Prefs[i] <- bar.colors[3]}
      if (plot.data$Test[i] == "Weaker") {plot.data$Prefs[i] <- bar.colors[1]}
    }
  } else {plot.data$Prefs <- rep(0, nrow(plot.data))}
  # --------------------------------------

  # --------------------------------------
  # Plot type 1: counts with confidence limits
  if(bar.type == "counts") {
    # Set up maximum y-axis value for ylim. Add an additional 5%
    min.y <- min(plot.data[, 3:6], na.rm = TRUE)
    max.y <- max(plot.data[, 3:6], na.rm = TRUE)
    max.y <- max.y * 1.05
    plot.data$Setup <- seq(min.y, max.y, length.out = nrow(plot.data))
    if (is.null(res.order)) {
      # Plot built up in 3 stages: i) uses min and max values to set the y-axis
      #   range without having to use ylim (so this can be customised by the user)
      #   ii) the main barplot and label, and iii) the error bars using arrows.
      #   Warnings surpressed for arrows: warnings are generated when they are of
      #   'zero length' i.e. no confidence interval
      bp1 <- graphics::barplot(plot.data$Setup, names.arg = plot.data$Resource,
                     col = 0, border = NA, axisnames = FALSE, ...)
      graphics::barplot(plot.data$Observed, names.arg = plot.data$Resource,
                        las = 3, main = node, col = plot.data$Prefs,
                        add = TRUE, ...)
      arrow.cols <- ifelse((plot.data[, 5] - plot.data[, 6]) == 0, NA, "black")
      graphics::arrows(bp1, plot.data[, 5], bp1, plot.data[, 6], lwd = 2,
                       col = arrow.cols, code = 3, angle = 90, length = .05)
    } else {
      colnames(res.order) <- c("Taxon", "Order")
      plot.data <- merge(plot.data, res.order, by.x = "Resource", by.y = "Taxon")
      rank.plot <- plot.data[order(plot.data$Order, decreasing = FALSE), ]
      bp1 <- graphics::barplot(rank.plot$Setup, names.arg = rank.plot$Resource,
                               col = 0, border = NA, axisnames = FALSE, ...)
      graphics::barplot(rank.plot$Observed, names.arg = rank.plot$Resource,
                        las = 3, main = node, col = rank.plot$Prefs,
                        add = TRUE, ...)
      arrow.cols <- ifelse((rank.plot[, 5] - rank.plot[, 6]) == 0, NA, "black")
      graphics::arrows(bp1, rank.plot[, 5], bp1, rank.plot[, 6], lwd = 2,
                       col = arrow.cols, code = 3, angle = 90, length = .05)
    }
  }
  # --------------------------------------

  # --------------------------------------
  # Plot type 2: Standardised effect sizes with 2 and -2 lines
  if(bar.type == "SES") {
    min.y <- min(plot.data$SES, na.rm = TRUE)
    max.y <- max(plot.data$SES, na.rm = TRUE)
    max.y <- max.y * 1.05
    plot.data$Setup <- seq(min.y, max.y, length.out = nrow(plot.data))
    if (is.null(res.order)) {
      bp1 <- graphics::barplot(plot.data$Setup, names.arg = plot.data$Resource,
                               col = 0, border = NA, axisnames = FALSE, ...)
      graphics::barplot(plot.data$SES, names.arg = plot.data$Resource, las = 3,
                        add = TRUE, main = node, col = plot.data$Prefs, ...)
      graphics::abline(h = 2, lty = 2)
      graphics::abline(h = -2, lty = 2)
    } else {
      colnames(res.order) <- c("Taxon", "Order")
      plot.data <- merge(plot.data, res.order, by.x = "Resource", by.y = "Taxon")
      rank.plot <- plot.data[order(plot.data$Order, decreasing = FALSE), ]
      bp1 <- graphics::barplot(rank.plot$Setup, names.arg = rank.plot$Resource,
                               col = 0, border = NA, axisnames = FALSE, ...)
      graphics::barplot(rank.plot$SES, names.arg = rank.plot$Resource, las = 3,
                        add = TRUE, main = node, col = rank.plot$Prefs, ...)
      graphics::abline(h = 2, lty = 2)
      graphics::abline(h = -2, lty = 2)
    }
  }
}


