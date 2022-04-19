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

LSTcentralLimitTheorem <- function(jaspResults, dataset, options) {
  set.seed(options[["cltSampleSeed"]])
  colors <- .getColors(options[["cltColorPalette"]])
  parentData <- .generateParentData(options)
  samples <- .cltTakeSamples(jaspResults, options = options, data = parentData)
  
  if (options[["parentShow"]])
    jaspResults[["cltParentDistribution"]] <- .cltParentDistribution(jaspResults, options = options, colors)
  
  if (options[["samplesShow"]]){
    maxSamples <- length(samples)
    fromTo <- .getFromToSampleShow(options[["cltSampleShowType"]], maxSamples, singleValue = options[["cltFirstOrLastSamples"]],
                                   start = options[["cltFromSample"]], stop = options[["cltToSample"]])
    from <- fromTo[1]
    to <- fromTo[2]
    jaspResults[["cltSamples"]] <- .cltPlotSamples(jaspResults, options, samples, from, to, colors)
  }
  
  if (options[["samplingDistShow"]])
    jaspResults[["cltSamplingDistribution"]] <- .cltSamplingDistribution(jaspResults, options, samples, colors)
  
  return()
}

.cltParentDistribution <- function(jaspResults, options, colors, showMean = TRUE, showSD = FALSE, labelsInCorner = FALSE) {
  distribution <- options[["cltParentDistribution"]]
  mean <- options[["cltMean"]]
  sd <- options[["cltStdDev"]]
  range <- options[["cltRange"]]
  skewDirection <- options[["cltSkewDirection"]]
  if (options[["cltSkewIntensity"]] == "low"){
    skew <- 2
  } else if (options[["cltSkewIntensity"]] == "medium") {
    skew <- 5
  } else if (options[["cltSkewIntensity"]] == "high") {
    skew <- 50
  }
  
  if (distribution == "binomial") {
    prob <- options[["binomProb"]]
    showMean <- FALSE
  } else {
    showMean <- showMean
    prob <- NA
  }
  
  pdPlot <- createJaspPlot(title = gettext("Parent Distribution"), width = 700, height = 400)
  pdPlot$position <- 1
  pdPlot$dependOn(options = c("cltParentDistribution",
                              "parentShow",
                              "cltMean",
                              "cltStdDev",
                              "cltRange",
                              "cltSkewDirection",
                              "cltSkewIntensity"))
  pdPlot$plotObject <- .distributionPlotFunction(distribution = distribution, mean = mean, sd = sd, range = range,
                                                 skew = skew, prob = prob, skewDirection = skewDirection,
                                                 colors, showMean = showMean, showSD = showSD, labelsInCorner = labelsInCorner)
  return(pdPlot)
}


.cltTakeSamples <- function(jaspResults, options, data, replace = TRUE) {
  n <- options[["cltSampleSize"]]
  k <- options[["cltSampleAmount"]]
  sampleList <- list()
  if (replace) {
    for (i in seq_len(k)) {
      sampleList[[i]] <- sample(x = data[["x"]], size = n, replace = TRUE)
    }
    return(sampleList)
  } else {
    population <- data
    if (options[["cltParentDistribution"]] == "binomial") {
      sampleIndiceList <- list()
      indices <- 1:length(population[["x"]])
      for(i in seq_len(k)) {
        sampleIndices <- sample(indices, size = n, replace = FALSE)
        sampleIndiceList[[i]] <- sampleIndices
        indicesWithoutSamples <- indices[-.getIndices(sampleIndices, indices)]
        indices <- indicesWithoutSamples
      }
      sampleList <- .getSampleValuesFromIndices(sampleIndiceList, population)
    } else {
      for(i in seq_len(k)) {
        indices <- 1:length(population[["x"]])
        sampleIndices <- sample(indices, size = n, replace = FALSE)
        sampleList[[i]] <- population[["x"]][sampleIndices]
        populationWithoutSamples <- population[["x"]][-sampleIndices]
        population <- data.frame(x = populationWithoutSamples)
      }
      orderedData <- data.frame(x = sort(data$x))
      sampleIndiceList <- lapply(sampleList, .getIndices, population = orderedData)
    }
    return(list("samples" = sampleList,
                "indices" = sampleIndiceList))
  }
}

.getIndices <- function(sampleVector, population){
  sampleIndices <- c()
  for (i in seq_along(sampleVector)) {
    sampleIndices <- c(sampleIndices, which(sampleVector[i] == population))
  }
  return(sampleIndices)
}

.getSampleValuesFromIndices <- function(sampleIndiceList, population){
  sampleList <- list()
  for (i in seq_along(sampleIndiceList)) {
    sampleList[[i]] <- population$x[sampleIndiceList[[i]]]
  }
  return(sampleList)
}


.cltPlotSamples <- function(jaspResults, options, samples, from, to, colors, showMean = TRUE, showSE = FALSE, showSD = FALSE,
                            labelsInCorner = FALSE) {
  maxSamples <- length(samples)
  visibleSamples <- from:to
  samples <- samples[visibleSamples]
  
  allCounts <- vector()
  for (i in seq_along(visibleSamples)) {
    sample <- samples[[i]]
    binWidthType1 <- .getBinWidth(variable = sample, options = options)
    h1 <- hist(x = sample, plot = FALSE, right = FALSE, breaks = binWidthType1)
    counts <- h1$counts
    allCounts <- c(allCounts, counts)
  }
  
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, allCounts))
  yStep <- yBreaks[2] - yBreaks[1]
  yLimits <- c(min(yBreaks), max(yBreaks) + 2 * yStep)
  
  if (options[["cltParentDistribution"]] == "binomial") {
    xBreaks <- c(1 , 2)
    xLimits <- c(.5, 2.5)
  } else {
    binWidthType2 <- .getBinWidth(unlist(samples), options)
    h2 <- hist(unlist(samples), plot = FALSE, right = FALSE, breaks = binWidthType2)
    xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(h2$breaks))
    xStep <- xBreaks[2] - xBreaks[1]
    xLimits <- c(min(xBreaks) - xStep, max(xBreaks) + 1.5 * xStep)
    binWidth <- (h2$breaks[2] - h2$breaks[1])
  }
  
  plotList <- list()
  for (i in 1:length(visibleSamples)) {
    if (options[["cltParentDistribution"]] == "binomial") {
      data <- factor(samples[[i]], level = c(1, 2))
      countTable <- table(data)
      df <- data.frame("biValues" = c(1, 2), "counts" = as.vector(countTable), "groups" = c("c1", "c2"))
      countLabelData <- data.frame(x = c(1, 2), y = rep(max(yBreaks) + yStep/2, 2),
                                   label = c(gettextf("Count: %i", df$counts[1]), gettextf("Count: %i", df$counts[2])))
      samplePlot <- ggplot2::ggplot() +
        ggplot2::geom_bar(data = df, mapping = ggplot2::aes(x = biValues, y = counts, fill = groups),
                          size = .3, stat = "identity", color = "black") +
        ggplot2::scale_fill_manual(values = colors[1:2]) +
        ggplot2::geom_label(data = countLabelData, mapping = ggplot2::aes(x = x, y = y, label = label),
                            color = colors[1:2], size = 6)
    } else {
      samplePlot <- ggplot2::ggplot(data.frame(x = samples[[i]]), ggplot2::aes(x = x)) +
        ggplot2::geom_histogram(mapping = ggplot2::aes(y =..count..),closed = "left", fill = colors[1], col = "black",
                                size = .7, binwidth = binWidth, center = binWidth/2)
      sampleMean <- mean(samples[[i]])
      if (showMean) {
        meanLineData <- data.frame(x = rep(sampleMean, 2), y = c(0, max(yBreaks) + yStep * 1.5))
        yPosLabel <- ifelse(labelsInCorner, max(yBreaks) - 4 * yStep, max(yBreaks) + yStep * 1.5)
        xPosLabel <- ifelse(labelsInCorner, max(xBreaks) + xStep, sampleMean)
        meanLabelData <- data.frame(x = xPosLabel, y = yPosLabel, label = gettextf("Mean: %.2f", sampleMean))
        samplePlot <- samplePlot +
          ggplot2::geom_path(data = meanLineData, mapping = ggplot2::aes(x = x, y = y), color = colors[4], size = 1,
                             alpha = .7) +
          ggplot2::geom_label(data = meanLabelData, mapping = ggplot2::aes(x = x, y = y, label = label),
                              color = colors[4], size = 6)
      }
      if (showSE) {
        SE <- sd(samples[[i]]) / sqrt(length(samples[[i]]))
        SElineData <- data.frame(x = c(sampleMean - SE, sampleMean + SE), y = rep(max(yBreaks) + yStep/2, 2))
        SElabelData <- data.frame(x = max(xBreaks) + xStep, y = max(yBreaks) - 2 * yStep, label = gettextf("SE: %.2f", SE))
        samplePlot <- samplePlot +
          ggplot2::geom_path(data = SElineData, mapping = ggplot2::aes(x = x, y = y), color = colors[6], size = 2,
                             alpha = 1) +
          ggplot2::geom_label(data = SElabelData, mapping = ggplot2::aes(x = x, y = y, label = label),
                              color = colors[6], size = 6)
      }
      if (showSD) {
        SD <- sd(samples[[i]])
        SDlineData <- data.frame(x = c(sampleMean - SD, sampleMean + SD), y = rep(max(yBreaks) + yStep * 1.5, 2))
        SDlabelData <- data.frame(x = max(xBreaks) + xStep, y = max(yBreaks), label = gettextf("SD: %.2f", SD))
        samplePlot <- samplePlot +
          ggplot2::geom_path(data = SDlineData, mapping = ggplot2::aes(x = x, y = y), color = colors[2], size = 2,
                             alpha = 1) +
          ggplot2::geom_label(data = SDlabelData, mapping = ggplot2::aes(x = x, y = y, label = label),
                              color = colors[2], size = 6)
      }
      if (options[["samplesShowRugs"]])
        samplePlot <- samplePlot + ggplot2::geom_rug()
    }
    samplePlot <- samplePlot +
      ggplot2::scale_y_continuous(breaks = yBreaks, limits = yLimits, name = "Count") +
      ggplot2::scale_x_continuous(breaks = xBreaks, limits = xLimits, name = "") +
      ggplot2::ggtitle(gettextf("Sample Nr. %i", visibleSamples[i])) +
      jaspGraphs::themeJaspRaw() +
      jaspGraphs::geom_rangeframe()
    plotList[[i]] <- samplePlot
  }
  tbcString <- ifelse(to == maxSamples, "", gettextf("... until Sample Nr. %i", maxSamples))
  plotMat <- .arrangePlotMat(plotList, tbc = tbcString)
  rows <- .getPlotMatDetails(length(visibleSamples))$rows
  plotHeight <- rows * 200
  
  sampleMatrixPlot <- createJaspPlot(title = gettext("Samples"), width = 1200, height = plotHeight)
  sampleMatrixPlot$position <- 2
  sampleMatrixPlot$dependOn(options = c("cltParentDistribution",
                                        "cltMean",
                                        "cltStdDev",
                                        "cltRange",
                                        "cltSkewDirection",
                                        "cltSkewIntensity",
                                        "cltSampleSize",
                                        "cltSampleAmount",
                                        "samplesShow",
                                        "samplesShowRugs"))
  sampleMatrixPlot$plotObject <- jaspGraphs::ggMatrixPlot(plotMat)
  
  return(sampleMatrixPlot)
}

.generateParentData <- function(options, finite = FALSE) {
  if (finite) {
    n <- finite
  } else {
    n <- 100000
  }
  distribution <- options[["cltParentDistribution"]]
  mean <- options[["cltMean"]]
  sd <- options[["cltStdDev"]]
  range <- options[["cltRange"]]
  
  if (distribution == "normal") {
    data <- rnorm(n, mean, sd)
    data <- mean + sd * scale(data)
  } else if (distribution == "uniform") {
    min <- mean - range / 2
    max <- mean + range / 2
    data <- runif(n = n, min = min, max = max)
    data <- mean + scale(data, scale = FALSE)
  } else if (distribution == "skewed") {
    if (options[["cltSkewIntensity"]] == "low") {
      skew <- 1.5
    } else if (options[["cltSkewIntensity"]] == "medium") {
      skew <- 3
    } else if (options[["cltSkewIntensity"]] == "high") {
      skew <- 10
    }
    if (options[["cltSkewDirection"]] == "left") {
      skew <- skew * -1
    }
    data <- .scaledSkewedNormal(n, xi = mean, omega = sd,  alpha = skew)
  } else if (distribution == "binomial") {
    prob <- options[["binomProb"]]
    data <- c(rep(1, round(n * prob)), rep(2, round(n * (1 - prob))))
  }
  df <- data.frame(x = data)
  return(df)
}


.cltSamplingDistribution <- function(jaspResults, options, samples, colors, showSD = FALSE, labelsInCorner = FALSE, 
                                     xAxisToScale = FALSE) {
  means <- lapply(X = samples, FUN = mean)
  meanDf <- data.frame(x = unlist(means))
  n <- length(samples)
  meanOfMeans <- mean(meanDf[["x"]])
  sdOfMeans <- sd(meanDf[["x"]])
  binWidthType <- .getBinWidth(variable = meanDf[["x"]], options = options)
  h <- hist(meanDf[["x"]], plot = FALSE, breaks = binWidthType)
  if(xAxisToScale) {
    meanParent <- options[["cltMean"]]
    if (options[["cltParentDistribution"]] != "uniform") {
      sdParent <- options[["cltStdDev"]]
    } else {
      rangeParent <- options[["cltRange"]]
      minParent <- meanParent - rangeParent / 2
      maxParent <- meanParent + rangeParent / 2
      sdParent <- (maxParent - minParent)/sqrt(12)
    }
    xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(meanParent - 3 * sdParent, meanParent + 3 * sdParent))
  } else {
    xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(h$breaks), min.n = 4)
  }
  xStep <- xBreaks[2] - xBreaks[1]
  xLimits <- c(min(xBreaks) - xStep, max(xBreaks) + xStep)
  counts <- h$counts
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, max(counts)))
  yStep <- yBreaks[2] - yBreaks[1]
  yLimits <- c(min(yBreaks), max(yBreaks) + yStep)

  binWidth <- (h$breaks[2] - h$breaks[1])
  
  sdPlotObject <- ggplot2::ggplot(meanDf, ggplot2::aes(x = x)) +
    ggplot2::geom_histogram(mapping = ggplot2::aes(y =..count..),
                            closed = "left", fill = colors[4], col = "black", size = .7,
                            binwidth = binWidth, center = binWidth/2)
  
  if (options[["samplingDistShowRugs"]])
    sdPlotObject <- sdPlotObject + ggplot2::geom_rug()
  
  meanLineData <- data.frame(x = rep(meanOfMeans, 2), y = c(0, max(yBreaks) + yStep / 2))
  sdPlotObject <- sdPlotObject +
    ggplot2::scale_x_continuous(breaks = xBreaks, limits = xLimits, name = "") +
    ggplot2::geom_path(data = meanLineData, mapping = ggplot2::aes(x = x, y = y), color = colors[5], size = 1,
                       alpha = .7)
  
  if (options[["samplingDistShowNormal"]])
    sdPlotObject <- sdPlotObject + ggplot2::stat_function(fun = function(x)dnorm(x, mean = meanOfMeans, sd = sdOfMeans) * binWidth * n,
                                                          size = 1)
  if (showSD) {
    SDLine <- data.frame(x = c(meanOfMeans - sdOfMeans, meanOfMeans + sdOfMeans), y = rep(max(yBreaks) + yStep/2, 2))
    SDLabel <- data.frame(x = max(xBreaks), y = max(yBreaks) + yStep/2, label = gettextf("Std. Dev.: %.2f", sdOfMeans))
    sdPlotObject <- sdPlotObject +
      ggplot2::geom_label(data = SDLabel, mapping = ggplot2::aes(x = x, y = y, label = label), color = colors[6], size = 6) +
      ggplot2::geom_path(data = SDLine, mapping = ggplot2::aes(x = x, y = y), color = colors[6], size = 2)
  }
  
  meanLabelyPos <- ifelse(labelsInCorner, max(yBreaks) - yStep/2, max(yBreaks) + yStep/2)
  meanLabelxPos <- ifelse(labelsInCorner, max(xBreaks), meanOfMeans)
  meanLabelData <- data.frame(x = meanLabelxPos, y = meanLabelyPos, label = gettextf("Mean: %.2f", meanOfMeans))
  sdPlotObject <- sdPlotObject +
    ggplot2::geom_label(data = meanLabelData, mapping = ggplot2::aes(x = x, y = y, label = label), color = colors[5], size = 6) +
    ggplot2::scale_y_continuous(breaks = yBreaks, limits = yLimits, name = "Count") +
    jaspGraphs::themeJaspRaw() + jaspGraphs::geom_rangeframe()
  
  sdPlot <- createJaspPlot(title = gettext("Sampling Distribution of the Mean"), width = 700, height = 400)
  sdPlot$position <- 3
  sdPlot$dependOn(options = c("cltParentDistribution",
                              "cltMean",
                              "cltStdDev",
                              "cltRange",
                              "cltSkewDirection",
                              "cltSkewIntensity",
                              "cltSampleSize",
                              "cltSampleAmount",
                              "samplingDistShow",
                              "samplingDistShowNormal",
                              "samplingDistShowRugs"))
  
  sdPlot$plotObject <- sdPlotObject
  
  return(sdPlot)
}

.scaledSkewedNormal <- function(n, xi= 0 , omega = 1, alpha = 0, tau = 0) {
  y <- xi + omega*scale(sn::rsn(n, xi = xi, omega = omega, alpha = alpha, tau = tau))
  return(y)
}


.distributionPlotFunction <- function(distribution = c("normal", "uniform", "skewed", "binomial"), mean, sd = NA, range = NA,
                                      skew = NA, prob = NA, skewDirection = "right", colors, showMean = TRUE, showSD, labelsInCorner) {
  if (distribution == "normal") {
    xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(mean - 3 * sd, mean + 3 * sd))
    xStep <- xBreaks[2] - xBreaks[1]
    xLimits <- c(min(xBreaks) - xStep, max(xBreaks) + xStep)
    pdPlotObject <-  ggplot2::ggplot(data.frame(x = xBreaks), ggplot2::aes(x = x)) +
      ggplot2::stat_function(fun = dnorm, n = 10000, args = list(mean = mean, sd = sd), geom = "area", fill = colors[1],
                             color = "black")
  } else if (distribution == "uniform") {
    min <- mean - range / 2
    max <- mean + range / 2
    sd <- (max - min)/sqrt(12)
    xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(mean - 3 * sd, mean + 3 * sd ))#jaspGraphs::getPrettyAxisBreaks(c(min - range / 4, max + range / 4))
    xStep <- xBreaks[2] - xBreaks[1]
    xLimits <- c(min(xBreaks) - xStep, max(xBreaks) + xStep)
    pdPlotObject <-  ggplot2::ggplot(data.frame(x = xBreaks), ggplot2::aes(x = x)) +
      ggplot2::stat_function(fun = dunif, n = 100, args = list(min = min, max = max), geom = "area", fill = colors[1]) +
      ggplot2::stat_function(fun = dunif, n = 100, args = list(min = min, max = max))
  } else if (distribution == "skewed") {
    if (skewDirection == "left")
      skew <- skew * -1
    xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(mean - 3 * sd, mean + 3 * sd ))
    xStep <- xBreaks[2] - xBreaks[1]
    xLimits <- c(min(xBreaks) - xStep, max(xBreaks) + xStep)
    df <- data.frame(x = .scaledSkewedNormal(100000, xi = mean, omega = sd, alpha = skew))
    pdPlotObject <-  ggplot2::ggplot(df, ggplot2::aes(x = x)) + ggplot2::geom_density(mapping = ggplot2::aes(y = ..density..),
                                                                                      n = 2^7, bw = sd/3, fill = colors[1])
  } else if (distribution == "binomial") {
    df <- data.frame("biValues" = 1:2, "counts" = c(prob, 1 - prob), "groups" = c("c1", "c2"))
    xBreaks <- c(1, 2)
    xLimits <- c(.5, 2.5)
    pdPlotObject <- ggplot2::ggplot() + ggplot2::geom_bar(data = df, mapping = ggplot2::aes(x = biValues, y = counts, fill = groups),
                                                          size = .3, stat = "identity", color = "black") +
      ggplot2::scale_fill_manual(values = colors[1:2])
  }
  plotData <- ggplot2::ggplot_build(pdPlotObject)$data[[1]]
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, plotData$y))
  yStep <- yBreaks[2] - yBreaks[1]
  yLimits <- c(min(yBreaks), max(yBreaks) + yStep)
  
  if (distribution == "binomial") {
    yName <- "Probability"
  } else {
    yName <- "Density"
  }
  
  if (showMean) {
    yPosMean <- ifelse(labelsInCorner, max(yBreaks) + yStep/2, plotData$y[which.min(abs(mean - plotData$x))])
    meanLineData <- data.frame(x = rep(mean, 2), y = c(0, yPosMean))
    yPosLabel <- ifelse(labelsInCorner, max(yBreaks) - yStep/2, yPosMean/2)
    xPosLabel <- ifelse(labelsInCorner, max(xBreaks), mean)
    meanLabelData <- data.frame(x = xPosLabel, y = yPosLabel, label = gettextf("Mean: %.2f", mean))
    pdPlotObject <- pdPlotObject +
      ggplot2::geom_path(data = meanLineData, mapping = ggplot2::aes(x = x, y = y), color = colors[3], size = 1,
                         alpha = .7) +
      ggplot2::geom_label(meanLabelData, mapping = ggplot2::aes(x = x, y = y, label = label), color = colors[3], size = 6)
  }
  
  if (showSD) {
    sdLineData <- data.frame(x = c(mean - sd, mean + sd), y = rep(max(yBreaks) + yStep/2, 2))
    sdLabelData <- data.frame(x = max(xBreaks), y = max(yBreaks) + yStep/2, label = gettextf("Std. Dev: %.2f", sd))
    pdPlotObject <- pdPlotObject +
      ggplot2::geom_path(data = sdLineData, mapping = ggplot2::aes(x = x, y = y), size = 2, color = colors[2]) +
      ggplot2::geom_label(sdLabelData, mapping = ggplot2::aes(x = x, y = y, label = label), color = colors[2], size = 6)
  }
  
  pdPlotObject <- pdPlotObject +
    ggplot2::scale_x_continuous(breaks = xBreaks, limits = xLimits, name = "") +
    ggplot2::scale_y_continuous(breaks = yBreaks, limits = yLimits, name = yName) +
    jaspGraphs::themeJaspRaw() +
    jaspGraphs::geom_rangeframe()
  
  return(pdPlotObject)
}

.getBinWidth <- function(variable, options) {
  binWidthType <- options[["cltBinWidthType"]]
  if (binWidthType == "doane") {  # https://en.wikipedia.org/wiki/Histogram#Doane's_formula
    sigma.g1 <- sqrt((6*(length(variable) - 2)) / ((length(variable) + 1)*(length(variable) + 3)))
    g1 <- mean(abs(variable)^3)
    k <- 1 + log2(length(variable)) + log2(1 + (g1 / sigma.g1))
    binWidthType <- k
  } else if (binWidthType == "fd" && nclass.FD(variable) > 10000) { # FD-method will produce extreme number of bins and crash ggplot, mention this in footnote
    binWidthType <- 10000
  } else if (binWidthType == "manual") {
    binWidthType <- options[["cltNumberOfBins"]]
  }
  return(binWidthType)
}
