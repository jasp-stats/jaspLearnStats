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
    if(options[["svParentSizeType"]] == "svParentInfinite") {
      parentData <- .generateParentData(options)
      samples <- .cltTakeSamples(jaspResults, options = options, data = parentData)
      if (options[["parentShow"]])
        jaspResults[["cltParentDistribution"]] <- .cltParentDistribution(jaspResults, options = options)
      if (options[["samplesShow"]]){
        maxSamples <- length(samples)
        fromTo <- .getFromToSampleShow(type = options[["svSampleShowType"]], maxSamples, singleValue = options[["svFirstOrLastSamples"]],
                                       start = options[["svFromSample"]], stop = options[["svToSample"]])
        from <- fromTo[1]
        to <- fromTo[2]
        jaspResults[["cltSamples"]] <- .cltPlotSamples(jaspResults, options = options, samples = samples, from = from, to = to)
      }
    } else {
      parentData <- .generateParentData(options, finite = options[["svParentSize"]])
      samplesAndIndices <-  .cltTakeSamples(jaspResults, options = options, data = parentData, replace = FALSE)
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
  maxSamples <- length(samples)
  fromTo <- .getFromToSampleShow(type = options[["svSampleShowType"]], maxSamples, singleValue = options[["svFirstOrLastSamples"]],
                                 start = options[["svFromSample"]], stop = options[["svToSample"]])
  from <- fromTo[1]
  to <- fromTo[2]
  visibleSamples <- from:to
  samples <- samples[visibleSamples]
  indices <- indices[visibleSamples]
  
  removedDots <- c()
  plotList <- list()
  for (i in 1:length(visibleSamples)) {
    if (options[["cltParentDistribution"]] == "binomial") {
      set.seed(123)
      dummyData <- sort(runif(n = length(parentData$x), min = -3, max = 3))
      dotPlotData <- data.frame(x = dummyData, group = parentData$x)
      dotPlotData$group <- as.factor(dotPlotData$group)
      currentSample <- indices[[i]]
      samplePlot <- .dotPlotWithGroups(dotPlotData, options, groupColors = c("orange", "dodgerblue", "white"),
                                       groups = TRUE, samples = currentSample, alpha = .4,
                                       sampleColors = c("orange", "dodgerblue"), removedDots = removedDots)
    } else {
      dotPlotData <- parentData
      samplePlot <- .dotPlotWithGroups(dotPlotData, options, samples = indices[[i]], alpha = .4, sampleColors = "orange",
                                       removedDots = removedDots)
    }
    removedDots <- unlist(indices[1:i])
    samplePlot <- samplePlot + ggplot2::ggtitle(gettextf("Sample Nr. %i", visibleSamples[i])) 
    plotList[[i]] <- samplePlot
  }
  tbcString <- ifelse(to == maxSamples, "", gettextf("... until Sample Nr. %i", maxSamples))
  plotMat <- .arrangePlotMat(plotList, tbc = tbcString)
  
  sampleMatrixPlot <- createJaspPlot(title = gettext("Samples"), width = 1200, height = 1500)
  sampleMatrixPlot$position <- 2
  #sampleMatrixPlot$dependOn(options = c())
  
  sampleMatrixPlot$plotObject <- jaspGraphs::ggMatrixPlot(plotMat)
  
  return(sampleMatrixPlot)
}

.dotPlotWithGroups <- function(data, options, groupColors = "", groups = FALSE, samples = "", alpha = 1, sampleColors,
                               removedDots = c()) {
  n <- length(data$x)
  dotSize <- .getDotSize(n)
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(data$x)
  xLimits <- range(xBreaks)
  if (groups) {
    plotObject <- ggplot2::ggplot() +
      ggplot2::geom_dotplot(data = data, mapping = ggplot2::aes(x = x, group = group, fill = group), binaxis = 'x',
                            stackdir = 'up', dotsize = dotSize, binpositions = "all", stackgroups = TRUE, alpha = alpha) +
      ggplot2::scale_fill_manual(values = groupColors) +
      ggplot2::coord_fixed()
  } else {
    plotObject <- ggplot2::ggplot() +
      ggplot2::geom_dotplot(data = data, mapping = ggplot2::aes(x = x), binaxis = 'x',
                            stackdir = 'up', dotsize = dotSize, fill = "orange", binpositions = "all", alpha = alpha) +
      ggplot2::coord_fixed()
  }
  pData <- ggplot2::ggplot_build(plotObject)$data
  dotWidth <- pData[[1]]$width[1] * dotSize
  yLabels <- unique(as.integer(jaspGraphs::getPrettyAxisBreaks(c(0, max(pData[[1]]$countidx)))))
  yBreaks <- yLabels * dotWidth
  yStep <- yBreaks[2] - yBreaks[1] 
  yLimits <-  range(yBreaks)
  plotObject <- plotObject + ggplot2::scale_y_continuous(name = "Count", limits = yLimits, breaks = yBreaks, labels = yLabels) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()
  
  if (length(removedDots != 0)) {
    for (rd in removedDots) {
      sampleDot <- pData[[1]][rd,]
      y0 <- ifelse(sampleDot$countidx == 1, dotWidth/2, dotWidth/2 + (sampleDot$countidx - 1) * dotWidth)
      x0 <- sampleDot$x
      circleData <- data.frame(x0 = x0,
                               y0 = y0,
                               r = dotWidth / 2)
      plotObject <- plotObject + ggforce::geom_circle(data = circleData, mapping = ggplot2::aes(x0 = x0, y0 = y0, r = r),
                                                      inherit.aes = FALSE, fill = "white", color = "grey90")
    }
  }
  
  if (samples != "") {
    for (s in samples) {
      sampleDot <- pData[[1]][s,]
      if(length(sampleColors) > 1) {
        colorIndex <- data$group[s]
        fillColor <- sampleColors[colorIndex]
      } else {
        fillColor <- sampleColors
      }
      y0 <- ifelse(sampleDot$countidx == 1, dotWidth/2, dotWidth/2 + (sampleDot$countidx - 1) * dotWidth)
      x0 <- sampleDot$x
      circleData <- data.frame(x0 = x0,
                               y0 = y0,
                               r = dotWidth / 2)
      plotObject <- plotObject + ggforce::geom_circle(data = circleData, mapping = ggplot2::aes(x0 = x0, y0 = y0, r = r),
                                                      inherit.aes = FALSE, fill = fillColor)
    }
  }
  
  
  if (options[["cltParentDistribution"]] == "binomial") {
    plotObject <- plotObject +
      ggplot2::scale_x_continuous(name = "", breaks = xBreaks, labels = rep("", length(xBreaks))) 
  } else {
    plotObject <- plotObject +
      ggplot2::scale_x_continuous(name = "Value", breaks = xBreaks, limits = xLimits)
  }
  return(plotObject)
}


.arrangePlotMat <- function(plotList, col = 2, tbc = ""){
  nPlots <- length(plotList)
  if (nPlots %% 2 == 0) {
    if (tbc != "") {
      nCanvas <- nPlots + 2
      tbcIndex <- nCanvas - 1
      emptyIndex <- nCanvas
    } else {
      nCanvas <- nPlots
      emptyIndex <- 0
    }
  } else {
    nCanvas <- nPlots + 1
    emptyIndex <- nCanvas
    if (tbc != "")
      tbcIndex <- nCanvas
  }
  rows <- nCanvas / col
  plotMat <- matrix(list(), rows, col)
  index <- 0
  
  for (r in 1:rows) {
    for (c in 1:col) {
      index <-  index + 1
      if (tbc != "" && index == tbcIndex) {
        plot <- ggplot2::ggplot() + ggplot2::theme_void() +
          ggplot2::annotate(geom = "text", x = 0, y = 0, label = tbc, size = 10)
      } else if(index == emptyIndex) {
        plot <- ggplot2::ggplot() + ggplot2::theme_void()
      } else {
        plot <- plotList[[index]]
      }
      plotMat[[r, c]] <- plot
    }
  }
  return(plotMat)
}

.getFromToSampleShow <- function(type = c("first", "last", "range", "all"), maxSamples, singleValue = "", start ="", stop = ""){
  if (type == "first") {
    from <- 1
    to <- singleValue
  } else if (type == "last") {
    from <- maxSamples - (singleValue - 1)
    to <- maxSamples
  } else if (type == "range") {
    from <- start
    to <- stop
  } else if (type == "all") {
    from <- 1
    to <- maxSamples
  }
  return(c(from, to))
}
