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
  if (!errors) {
    set.seed(options[["cltSampleSeed"]])
    if(options[["svParentSizeType"]] == "svParentInfinite"){
      parentData <- .generateParentData(options)
      samples <- .cltTakeSamples(jaspResults, options = options, data = parentData)
      if (options[["parentShow"]])
        jaspResults[["cltParentDistribution"]] <- .cltParentDistribution(jaspResults, options = options)
      if (options[["samplesShow"]])
        jaspResults[["cltSamples"]] <- .cltPlotSamples(jaspResults, options = options, samples = samples)
    } else {
      parentData <- .generateParentData(options, finite = options[["svParentSize"]])
      samplesAndIndices <- .cltTakeSamples(jaspResults, options = options, data = parentData, replace = FALSE)
      if (options[["parentShow"]])
        jaspResults[["cltParentDistribution"]] <- .svPlotFinitePopulation(jaspResults, options, parentData)
      if (options[["samplesShow"]])
        jaspResults[["cltSamples"]] <- .svPlotFiniteSamples(jaspResults, options, samples = samplesAndIndices$samples,
                                                            indices = samplesAndIndices$indices, parentData)
    }
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

.svPlotFinitePopulation <- function(jaspResults, options, data) {
  n <- length(data$x)
  plotSize <- 700 + (n / 50) * 25
  plot <- createJaspPlot(title = gettext("Parent Distribution"), width = plotSize, height = plotSize)
  plot$position <- 1

  if (options[["cltParentDistribution"]] == "binomial") {
    set.seed(123)
    dummyData <- sort(runif(n = n, min = -3, max = 3))
    dotPlotData <- data.frame(x = dummyData, group = as.factor(data$x))
    plotObject <- .dotPlotWithGroups(dotPlotData, options, groupColors = c("orange", "dodgerblue"), groups = TRUE)
  } else {
    dotPlotData <- data
    plotObject <- .dotPlotWithGroups(dotPlotData, options)
  }
  plot$plotObject <- plotObject
  return(plot)
}


.svPlotFiniteSamples <- function(jaspResults, options, samples, indices, parentData) {
  samples <- samples[1:7]
  indices <- indices[1:7]
  plotMat <- matrix(list(), 4, 2)
  n <- length(parentData$x)

  index <- 0
  for (i in 1:4) {
    for (j in 1:2) {
      index <-  index + 1
      if (index == 8) {
        samplePlot <- ggplot2::ggplot() + ggplot2::theme_void() +
          ggplot2::annotate(geom = "text", x = 0, y = 0, label = gettextf("... until Sample Nr. %i",
                                                                          options[["cltSampleAmount"]]), size = 10)
      } else {
        if (options[["cltParentDistribution"]] == "binomial") {
          set.seed(123)
          dummyData <- sort(runif(n = n, min = -3, max = 3))
          dotPlotData <- data.frame(x = dummyData, group = parentData$x)
          dotPlotData$group <- as.factor(dotPlotData$group)
          samplePlot <- .dotPlotWithGroups(dotPlotData, options, groupColors = c("orange", "dodgerblue"),
                                           groups = TRUE, samples = indices[index])
        } else {
          dotPlotData <- parentData
          samplePlot <- .dotPlotWithGroups(dotPlotData, options, samples = indices[index])
        }
      }
      plotMat[[i,j]] <- samplePlot
    }
  }
  sampleMatrixPlot <- createJaspPlot(title = gettext("Samples"), width = 1200, height = 1500)
  sampleMatrixPlot$position <- 2
  #sampleMatrixPlot$dependOn(options = c())

  sampleMatrixPlot$plotObject <- jaspGraphs::ggMatrixPlot(plotMat)

  return(sampleMatrixPlot)
}

.dotPlotWithGroups <- function(data, options, groupColors = "", groups = FALSE, samples = "") {
  n <- length(data$x)
  dotSize <- .getDotSize(n)
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(data$x)
  xLimits <- range(xBreaks)
  if (groups) {
    plotObject <- ggplot2::ggplot() +
      ggplot2::geom_dotplot(data = data, mapping = ggplot2::aes(x = x, group = group, fill = group), binaxis = 'x',
                            stackdir = 'up', dotsize = dotSize, binpositions = "all", stackgroups = TRUE) +
      ggplot2::scale_fill_manual(values = groupColors) +
      ggplot2::coord_fixed()
  } else {
    plotObject <- ggplot2::ggplot() +
      ggplot2::geom_dotplot(data = data, mapping = ggplot2::aes(x = x), binaxis = 'x',
                            stackdir = 'up', dotsize = dotSize, fill = "orange", binpositions = "all") +
      ggplot2::coord_fixed()
  }
  pData <- ggplot2::ggplot_build(plotObject)$data
  dotWidth <- pData[[1]]$width[1] * dotSize
  yLabels <- unique(as.integer(jaspGraphs::getPrettyAxisBreaks(c(0, max(pData[[1]]$countidx)))))
  yBreaks <- yLabels * dotWidth
  yLimits <-  range(yBreaks)
  plotObject <- plotObject + ggplot2::scale_y_continuous(name = "", limits = yLimits, breaks = yBreaks, labels = yLabels) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()


  if (samples != "") {
    for (i in samples){
    sampleDot <- pData[[1]][i,]
    y0 <- ifelse(sampleDot$countidx == 1, dotWidth/2, dotWidth/2 + (sampleDot$countidx - 1) * dotWidth)
    x0 <- sampleDot$x
    circleData <- data.frame(x0 = x0,
                             y0 = y0,
                             r = dotWidth / 2)
    plotObject <- plotObject + ggforce::geom_circle(data = circleData, mapping = ggplot2::aes(x0 = x0, y0 = y0, r = r),
                                                    inherit.aes = FALSE, fill = "red")
    }
  }

  if (options[["cltParentDistribution"]] == "binomial") {
    plotObject <- plotObject +
      ggplot2::scale_x_continuous(name = "", breaks = xBreaks, labels = rep("", length(xBreaks))) +
      ggplot2::theme(axis.ticks = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_blank())
  } else {
    plotObject <- plotObject +
      ggplot2::scale_x_continuous(name = "Value", breaks = xBreaks, limits = xLimits) +
      ggplot2::theme(axis.ticks.y = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_blank())
  }
  return(plotObject)
}
