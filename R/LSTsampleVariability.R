#
# Copyright (C) 2019 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

LSTsampleVariability <- function(jaspResults, dataset, options) {
  errors <- .svCheckErrors(options, jaspResults)
  if (!errors){
    set.seed(options[["cltSampleSeed"]])
    if(options[["svParentSizeType"]] == "svParentInfinite"){
      parentData <- .generateParentData(options)
      samples <- .cltTakeSamples(jaspResults, options = options, data = parentData)
    } else {
      parentData <- .generateParentData(options, finite = options[["svParentSize"]])
      samples <- .cltTakeSamples(jaspResults, options = options, data = parentData, replace = FALSE)
    }
    
    
    if (options[["parentShow"]])
      if(options[["svParentSizeType"]] == "svParentInfinite") {
        jaspResults[["cltParentDistribution"]] <- .cltParentDistribution(jaspResults, options = options)
      } else {
        jaspResults[["cltParentDistribution"]] <- .svPlotFinitePopulation(jaspResults, options, parentData)
      }
    
    if (options[["samplesShow"]])
      jaspResults[["cltSamples"]] <- .cltPlotSamples(jaspResults, options = options, samples = samples)
  }
  
  return()
}


.svCheckErrors <- function(options, jaspResults){
  errors <- FALSE
  errorMessage <- createJaspPlot(title = gettext("Error Message"), width = 700, height = 400)
  if (options[["svParentSizeType"]] == "svParentFinite"){
    n <- options[["cltSampleSize"]]
    k <- options[["cltSampleAmount"]]
    N <- options[["svParentSize"]]
    if ((n * k) > N) {
      errors <- TRUE
      errorMessage$setError(gettextf("Population size too small. Cannot draw %i samples with size %i from a finite population of %i.", k, n, N))
    }
  }
  if (errors) {
    jaspResults[["cltSamples"]] <- errorMessage
  }
  return(errors)
  
  
}

.svPlotFinitePopulation <- function(jaspResults, options, data){
  n <- length(data$x)
  plotSize <- 700 + (n / 50) * 25
  plot <- createJaspPlot(title = gettext("Parent Distribution"), width = plotSize, height = plotSize)
  plot$position <- 1
  
  if (options[["cltParentDistribution"]] == "binomial") {
    dummyData <- sort(runif(n = n, min = -3, max = 3))
    dotPlotData <- data.frame(x = dummyData, group = as.factor(data$x))
    dotSize <- .getDotSize(n)
    xBreaks <- jaspGraphs::getPrettyAxisBreaks(dummyData)
    plotObject <- ggplot2::ggplot() +
      ggplot2::geom_dotplot(data = dotPlotData, mapping = ggplot2::aes(x = x, group = group, fill = group), binaxis = 'x',
                            stackdir = 'up', dotsize = dotSize) +
      ggplot2::scale_fill_manual(values = c("grey20", "grey90")) +
      ggplot2::scale_x_continuous(name = "", breaks = xBreaks, labels = rep("", length(xBreaks))) +
      ggplot2::coord_fixed() 
    
    pData <- ggplot2::ggplot_build(plotObject)$data
    dotWidth <- pData[[1]]$width[1] * dotSize
    yLabels <- unique(as.integer(jaspGraphs::getPrettyAxisBreaks(c(0, max(pData[[1]]$countidx)))))
    yBreaks <- yLabels * dotWidth
    yStep <- yBreaks[2] - yBreaks[1]
    yMax <- ifelse(max(yBreaks) < (10 * dotWidth), (10 * dotWidth) + yStep*2, max(yBreaks) + yStep*2)
    yLimits <-  c(0, yMax)
    
    plotObject <- plotObject + ggplot2::scale_y_continuous(name = "", limits = yLimits, breaks = yBreaks, labels = yLabels) + 
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw() +
      ggplot2::theme(axis.ticks = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_blank())
  } else {
    dotPlotData <- data
    plotObject <- .lstDescCreateDotPlotObject(dotPlotData, options, stats = "none", discrete = FALSE, rugs = FALSE)
  }
  
  plot$plotObject <- plotObject
  return(plot)
}
