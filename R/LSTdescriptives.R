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

LSTdescriptives <- function(jaspResults, dataset, options, state = NULL) {
  inputType <- options[["lstDescDataType"]]
  ready <- (inputType == "dataRandom" |
              (inputType == "dataSequence" && options[["lstDescDataSequenceInput"]] != "") | 
              (inputType == "dataVariable" && options[["selectedVariable"]] != ""))
  data <- .getDataLSTdesc(jaspResults, options, inputType)
  
  #checking whether data is discrete or continuous, whereas only integers are treated as discrete
  discrete <- ifelse(all(data$x == as.integer(data$x)), TRUE, FALSE)
  
  
  if(options[["LSdescCentralOrSpread"]] == "LSdescCentralTendency"){
    if (options[["LSdescExplanationC"]])
      .descExplanationCT(jaspResults, options)
    if (options[["LSdescHistBar"]])
      .lstDescCreateHistogramOrBarplot(jaspResults, options, data, ready, discrete, stats = "ct")
    if (options[["LSdescDotPlot"]])
      .lstDescCreateDotplot(jaspResults, options, data, ready, discrete, stats = "ct")
  }
  
  if(options[["LSdescCentralOrSpread"]] == "LSdescSpread"){
    if (options[["LSdescExplanationS"]])
      .descExplanationS(jaspResults, options)
    if (options[["LSdescHistBar"]])
      .lstDescCreateHistogramOrBarplot(jaspResults, options, data, ready, discrete, stats = "spread")
    if (options[["LSdescDotPlot"]])
      .lstDescCreateDotplot(jaspResults, options, data, ready, discrete, stats = "spread")
  }
}


.descExplanationS <- function(jaspResults, options){
  jaspResults[["descExplanationS"]] <- createJaspContainer(gettext("Explanation"))
  jaspResults[["descExplanationS"]]$position <- 1
  
  plot <- createJaspPlot(title = gettext("Example distribution"), width = 600, height = 600)
  plot$position <- 1
  
  set.seed(123)
  data <- data.frame(index = 1:21, x = rnorm(21, 10, 2.5))   #sample(0:20, 21)
  
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(data$x)
  yBreaks <- c(1, 6, 11, 16, 21)
  
  if (options[["LSdescS"]] == "LSdescRange") {
    sortedDf <- data[order(data$x),]
    sortedDf <- cbind(sortedDf, list("extreme" = c("min", rep("normal", 19), "max")))
    
    plotObject <- ggplot2::ggplot() +
      ggplot2::geom_point(data = sortedDf, mapping = ggplot2::aes(x = x, y = index, fill = extreme),
                          size = 6, color = "black", shape = 21) +
      ggplot2::scale_fill_manual(values = c("min" = "blue", "normal" = "gray", "max" = "red"))
    
    minY <- which.min(data$x)
    minX <- min(data$x)
    minLineData <- data.frame(x = rep(minX, 2), y = c(minY, -.5))
    maxY <- which.max(data$x)
    maxX <- max(data$x)
    maxLineData <- data.frame(x = rep(maxX, 2), y = c(maxY, -.5))
    rangeLineData <- data.frame(x = c(minX, maxX), y = rep(-.5, 2))
    arrowHeadData1 <- data.frame(x = c(minX, minX + .3, minX + .3), y = c(-.5, 0, -1))
    arrowHeadData2 <- data.frame(x = c(maxX, maxX - .3, maxX - .3), y = c(-.5, 0, -1))
    rangeLabelData <- data.frame(x = (maxX + minX) / 2, y = -.5, label = gettext("Range"))
    minLabelData <- data.frame(x = minX, y = (minY - .5) / 2, label = gettext("Min."))
    maxLabelData <- data.frame(x = maxX, y = (maxY - .5) / 2, label = gettext("Max."))
    
    plotObject <- plotObject +
      ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = minLineData, color = "blue", size = 1) + 
      ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = maxLineData, color = "red", size = 1) +
      ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = rangeLineData, color = "orange", size = 1.5) + 
      ggplot2::geom_polygon(mapping = ggplot2::aes(x = x, y = y), data = arrowHeadData1, fill = "orange") + 
      ggplot2::geom_polygon(mapping = ggplot2::aes(x = x, y = y), data = arrowHeadData2, fill = "orange") +
      ggplot2::geom_label(mapping = ggplot2::aes(x = x, y = y, label = label), data = rangeLabelData, color = "orange", size = 6) +
      ggplot2::geom_label(mapping = ggplot2::aes(x = x, y = y, label = label), data = minLabelData, color = "blue", size = 6) +
      ggplot2::geom_label(mapping = ggplot2::aes(x = x, y = y, label = label), data = maxLabelData, color = "red", size = 6) +
      ggplot2::scale_y_continuous(name = "Observation No.", breaks = yBreaks, limits = c(-1, 21)) +
      ggplot2::scale_x_continuous(name = "Value", breaks = xBreaks, limits = range(xBreaks))
  }else if (options[["LSdescS"]] == "LSdescQR") {
    quartiles <- quantile(data$x)
    twentyfivePercentLabels <- data.frame(x = c(sum(quartiles[1:2])/2, sum(quartiles[2:3])/2,
                                                sum(quartiles[3:4])/2, sum(quartiles[4:5])/2),
                                          y = rep(10, 4), label = rep("25%", 4))
    q1LineData <- data.frame(x = rep(quartiles[2], 2), y = c(21, -.5))
    q2LineData <- data.frame(x = rep(quartiles[3], 2), y = c(21, 1))
    q3LineData <- data.frame(x = rep(quartiles[4], 2), y = c(21, -.5))
    iqrLineData <- data.frame(x = c(quartiles[2], quartiles[4]), y = rep(-.5, 2))
    arrowHeadData1 <- data.frame(x = c(quartiles[2], quartiles[2] + .3, quartiles[2] + .3), y = c(-.5, 0, -1))
    arrowHeadData2 <- data.frame(x = c(quartiles[4], quartiles[4] - .3, quartiles[4] - .3), y = c(-.5, 0, -1))
    labelData <- data.frame(x = c(rep(max(xBreaks)+2, 3), mean(data$x)), y = c(20, 18, 16, -.5),
                            label = gettext("1st quartile", "2nd quartile / \n Median", "3rd quartile", "IQR"))
    
    plotObject <- ggplot2::ggplot(data = data, mapping = ggplot2::aes(x = x, y = index)) +
      ggplot2::geom_ribbon(mapping = ggplot2::aes(xmin = min(data$x), xmax = quartiles[2]), fill = "salmon4") +
      ggplot2::geom_ribbon(mapping = ggplot2::aes(xmin = quartiles[2], xmax = quartiles[3]), fill = "salmon3") +
      ggplot2::geom_ribbon(mapping = ggplot2::aes(xmin = quartiles[3], xmax = quartiles[4]), fill = "salmon2") +
      ggplot2::geom_ribbon(mapping = ggplot2::aes(xmin = quartiles[4], xmax = quartiles[5]), fill = "salmon") +
      ggplot2::geom_text(mapping = ggplot2::aes(x = x, y = y, label = label), data = twentyfivePercentLabels, size = 5) +
      ggplot2::geom_point(size = 6, fill = "grey", color = "black", shape = 21) +
      ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = q1LineData, color = "purple", size = 1) +
      ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = q2LineData, color = "green", size = 1) +
      ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = q3LineData, color = "dodgerblue", size = 1) +
      ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = iqrLineData, color = "orange", size = 1) +
      ggplot2::geom_polygon(mapping = ggplot2::aes(x = x, y = y), data = arrowHeadData1, fill = "orange") + 
      ggplot2::geom_polygon(mapping = ggplot2::aes(x = x, y = y), data = arrowHeadData2, fill = "orange") +
      ggplot2::geom_label(mapping = ggplot2::aes(x = x, y = y, label = label), data = labelData,
                          color = c("purple", "green", "dodgerblue", "orange"), size = 5) +
      ggplot2::scale_y_continuous(name = "Observation No.", breaks = yBreaks, limits = c(-1, 21)) +
      ggplot2::scale_x_continuous(name = "Value", breaks = xBreaks, limits = c(min(xBreaks), max(xBreaks) + 5))
  }else if (options[["LSdescS"]] == "LSdescVar") {
    meanPoint <- mean(data$x)
    meanLineData <- data.frame(x = rep(meanPoint, 2), y = c(21, 1))
    labelData <- data.frame(x = meanPoint, y = 11, label = gettext("Mean"))
    
    plotObject <- ggplot2::ggplot(data = data, mapping = ggplot2::aes(x = x, y = index)) +
      ggplot2::geom_point(size = 6, fill = "grey", color = "black", shape = 21) 
    for (i in 1:length(data$x)) {
      devLineData <- data.frame(x = c(data$x[i], meanPoint), y = rep(data$index[i], 2))
      plotObject <- plotObject +
        ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = devLineData, size = 1, color = "dodgerblue")
    }
    plotObject <- plotObject +
      ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = meanLineData, size = 1, color = "red") +
      ggplot2::geom_label(mapping = ggplot2::aes(x = x, y = y, label = label), data = labelData, size = 5, color = "red") +
      ggplot2::scale_y_continuous(name = "Observation No.", breaks = yBreaks, limits = c(0, 21)) +
      ggplot2::scale_x_continuous(name = "Value", breaks = xBreaks, limits = range(xBreaks)) 
  }else if (options[["LSdescS"]] == "LSdescSD") {
    stdDev <- sd(data$x)
    meanPoint <- mean(data$x)
    meanLineData <- data.frame(x = rep(meanPoint, 2), y = c(21, 1))
    labelData <- data.frame(x = meanPoint, y = 11, label = gettext("Mean"))
    sdLabels <- data.frame(x = c(sum(min(xBreaks), meanPoint - 2*stdDev)/2, sum(meanPoint - stdDev, meanPoint - 2*stdDev)/2,
                                 sum(meanPoint, meanPoint - stdDev)/2, sum(meanPoint, meanPoint + stdDev)/2, 
                                 sum(meanPoint + stdDev, meanPoint + 2*stdDev)/2, sum(meanPoint + 2*stdDev, meanPoint + 3*stdDev)/2),
                           y = rep(11, 6), label = gettext("-3 SD", "-2 SD", "-1 SD", "+1 SD", "+2 SD", "+3 SD"))
    plotObject <- ggplot2::ggplot(data = data, mapping = ggplot2::aes(x = x, y = index)) +
      ggplot2::geom_ribbon(mapping = ggplot2::aes(xmin = meanPoint, xmax = min(xBreaks)), fill = "salmon") +
      ggplot2::geom_ribbon(mapping = ggplot2::aes(xmin = meanPoint, xmax = max(xBreaks)), fill = "salmon") +
      ggplot2::geom_ribbon(mapping = ggplot2::aes(xmin = meanPoint, xmax = meanPoint + 2*stdDev), fill = "salmon2") +
      ggplot2::geom_ribbon(mapping = ggplot2::aes(xmin = meanPoint, xmax = meanPoint - 2*stdDev), fill = "salmon2") +
      ggplot2::geom_ribbon(mapping = ggplot2::aes(xmin = meanPoint, xmax = meanPoint + stdDev), fill = "salmon4") +
      ggplot2::geom_ribbon(mapping = ggplot2::aes(xmin = meanPoint, xmax = meanPoint - stdDev), fill = "salmon4") +
      ggplot2::geom_text(mapping = ggplot2::aes(x = x, y = y, label = label), data = sdLabels, size = 4) +
      ggplot2::geom_point(size = 6, fill = "grey", color = "black", shape = 21) +
      ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = meanLineData, size = 1, color = "red") +
      ggplot2::geom_label(mapping = ggplot2::aes(x = x, y = y, label = label), data = labelData, size = 5, color = "red") +
      ggplot2::scale_y_continuous(name = "Observation No.", breaks = yBreaks, limits = c(0, 21)) +
      ggplot2::scale_x_continuous(name = "Value", breaks = xBreaks, limits = range(xBreaks))
  }
  
  
  
  plotObject <- plotObject +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()
  
  plotData <- ggplot2::ggplot_build(plotObject)$data[[1]]
  
  
  text <- gettext("Text for comparison:  Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")
  
  plot$plotObject <- plotObject
  
  jaspResults[["descExplanationS"]][["Plot"]] <- plot
  jaspResults[["descExplanationS"]][["Text"]] <- createJaspHtml(text, "p")
}

.descExplanationCT <- function(jaspResults, options) {
  jaspResults[["descExplanationCT"]] <- createJaspContainer(gettext("Explanation"))
  jaspResults[["descExplanationCT"]]$position <- 1
  
  mean <- 0
  sd <- 1
  skew <- 1000
  
  pdPlot <- createJaspPlot(title = gettext("Theoretical example distribution"), width = 700, height = 400)
  pdPlot$position <- 1
  
  
  xLimits <- c(mean - 5 * sd, mean + 5 * sd)
  yLimits <- c(-.1 , .5)
  distLimits <- c(mean - 4 * sd, mean + 4 * sd)
  hLineData <- data.frame(x = distLimits, y = rep(0, 2))
  
  df <- data.frame(x = .scaledSkewedNormal(100000, xi = mean, omega = sd, alpha = skew))
  df <- subset(df, df$x > distLimits[1] & df$x < distLimits[2]) # remove values outside limits
  pdPlotObject <-  ggplot2::ggplot(df, ggplot2::aes(x = x)) +
    ggplot2::geom_density(mapping = ggplot2::aes(y = ..density..), n = 2^10, bw = sd/3, size = 1) +
    ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = hLineData, size = 1) +
    ggplot2::ylim(yLimits) +
    ggplot2::xlim(xLimits) +
    jaspGraphs::themeJaspRaw() +
    ggplot2::theme(axis.line = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(), axis.title.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank())
  
  #to make density line stop a dist limits
  densityOverlayData1 <- data.frame(x = c(xLimits[1], distLimits[1]), y = rep(0, 2))
  densityOverlayData2 <- data.frame(x = c(xLimits[2], distLimits[2]), y = rep(0, 2))
  
  pdPlotObject <- pdPlotObject +
    ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = densityOverlayData1, color = "white", size = 4) +
    ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = densityOverlayData2, color = "white", size = 4)
  
  plotData <- ggplot2::ggplot_build(pdPlotObject)$data[[1]]
  
  allCTs <- options[["LSdescCT"]] == "LSdescMMM"
  if(options[["LSdescCT"]] == "LSdescMedian"| allCTs){
    median <- median(df$x)
    medianLineHeight <- plotData$y[which.min(abs(plotData$x - median))]
    medianLineData <- data.frame(x = c(rep(median, 2)), y = c(0, medianLineHeight))
    fiftyPercentLabels <- data.frame(x = c(median + .85, median - .85), y = rep(.1, 2), label = rep("50%", 2))
    labelXPos <- ifelse(allCTs, 4, median)
    labelYPos <- ifelse(allCTs, max(yLimits) * .85, max(yLimits) * .55)
    
    pdPlotObject <- pdPlotObject +
      ggplot2::geom_ribbon(mapping = ggplot2::aes(ymin = 0, ymax = y), data = subset(plotData, plotData$x > median),
                           fill = "grey") +
      ggplot2::geom_ribbon(mapping = ggplot2::aes(ymin = 0, ymax = y), data = subset(plotData, plotData$x < median),
                           fill = "grey", alpha = .5) +
      ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = medianLineData, size = 1, color = "green") +
      ggplot2::geom_label(data = data.frame(x = labelXPos, y = labelYPos, label = gettext("Median")), 
                          mapping = ggplot2::aes(x = x, y = y, label = label), color = "green", size = 6) +
      ggplot2::geom_text(data = fiftyPercentLabels, mapping = ggplot2::aes(x = x, y = y, label = label), size = 7)
    text <- gettext("Text for Median:  Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")
  }
  
  if(options[["LSdescCT"]] == "LSdescMode"| allCTs){
    mode <- plotData$x[plotData$y == max(plotData$y)]
    modeVLineData <- data.frame(x = c(rep(mode, 2)), y = c(0, max(plotData$y)))
    modeHLineData <- data.frame(x = c(mode - .7, mode + .7), y = rep(max(plotData$y) + 0.003, 2))
    labelXPos <- ifelse(allCTs, 4, mode)
    labelYPos <- ifelse(allCTs, max(yLimits) * .75, max(yLimits) * .45)
    pdPlotObject <- pdPlotObject +
      ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = modeVLineData, size = 1, color = "blue") +
      ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = modeHLineData, size = 1, color = "lightblue") +
      ggplot2::geom_label(data = data.frame(x = labelXPos, y = labelYPos, label = gettext("Mode")), 
                          mapping = ggplot2::aes(x = x, y = y, label = label), color = "blue", size = 6)
    text <- gettext("Text for Mode:  Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")
  }
  
  if(options[["LSdescCT"]] == "LSdescMean" | allCTs){
    meanLineHeight <- plotData$y[which.min(abs(plotData$x - mean))]
    meanLineData <- data.frame(x = c(rep(mean, 2)), y = c(0, meanLineHeight))
    triangleData <- data.frame(x = c(mean, mean + .4, mean - .4), y = c(0, -.05, -.05))
    balanceBaseData <- data.frame(x = rep(c(-2, 2), each = 2), y = c(-.05, -.1, -.1, -.05))
    balanceBaseLineData <- data.frame(x = c(-2, 2), y = rep(-.05, 2))
    balanceLineData1 <- data.frame(x = c(seq(-3.8, -4.2, length.out = 40), seq(-4.2, -4.1, length.out = 60)),
                                   y = seq(-.04, .04, length.out = 100))
    balanceLineData2 <- data.frame(x = c(seq(-4, -4.3, length.out = 40), seq(-4.3, -4.25, length.out = 60)),
                                   y = seq(-.04, .02, length.out = 100))
    balanceLineData3 <- data.frame(x = balanceLineData1$x * -1, y = balanceLineData1$y)
    balanceLineData4 <- data.frame(x = balanceLineData2$x * -1, y = balanceLineData2$y)
    labelXPos <- ifelse(allCTs, 4, mean)
    labelYPos <- ifelse(allCTs, max(yLimits) * .95, max(yLimits) * .2)
    
    pdPlotObject <- pdPlotObject + 
      ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = meanLineData, size = 1, color = "red") +
      ggplot2::geom_polygon(mapping = ggplot2::aes(x = x, y = y), data = triangleData, fill = "red") +
      ggplot2::geom_polygon(mapping = ggplot2::aes(x = x, y = y), data = balanceBaseData, fill = "rosybrown2", alpha = .3) +
      ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = balanceBaseLineData, size = 1.2, color = "rosybrown2") +
      ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = balanceLineData1, size = 1.2, color = "rosybrown2") +
      ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = balanceLineData2, size = 1, color = "rosybrown2") +
      ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = balanceLineData3, size = 1.2, color = "rosybrown2") +
      ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = balanceLineData4, size = 1, color = "rosybrown2") +
      ggplot2::geom_label(data = data.frame(x = labelXPos, y = labelYPos, label = gettext("Mean")), 
                          mapping = ggplot2::aes(x = x, y = y, label = label), color = "red", size = 6)
    text <- gettext("Text for Mean : Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")
  }
  
  if (options[["LSdescCT"]] == "LSdescMMM"){
    text <- gettext("Text for comparison:  Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")
  }
  
  pdPlot$plotObject <- pdPlotObject
  
  jaspResults[["descExplanationCT"]][["Plot"]] <- pdPlot
  jaspResults[["descExplanationCT"]][["Text"]] <- createJaspHtml(text, "p")
}


.getDataLSTdesc <- function(jaspResults, options, inputType) {
  if (inputType == "dataRandom") {
    data <- .sampleRandomDataForLSTdesc(jaspResults, options)
  } else if (inputType == "dataSequence") {
    data <- .readInputSequenceForLSTdesc(jaspResults, options)
  } else if (inputType == "dataVariable") {
    data <- .readDataLSTdesc(jaspResults, options)
  }
  return(data)
}

.sampleRandomDataForLSTdesc <- function(jaspResults, options){
  set.seed(options[["lstDescSampleSeed"]])
  n <- options[["lstDescSampleN"]]
  if (options[["lstDescSampleDistType"]] == "lstSampleDistDiscrete") {
    if (options[["LSdescDiscreteDistributions"]] == "binomialDist") {
      data <- rbinom(n, 10, prob = .5)
    } else if (options[["LSdescDiscreteDistributions"]] == "poissonDist") {
      data <- rpois(n, 1)
    }
  } else if (options[["lstDescSampleDistType"]] == "lstSampleDistCont") {
    if (options[["LSdescContinuousDistributions"]] == "skewedNormal") {
      data <- sn::rsn(n, alpha = 100)
    } else if (options[["LSdescContinuousDistributions"]] == "uniform") {
      data <- runif(n = n, min = 0, max = 5)
    } else if (options[["LSdescContinuousDistributions"]] == "normal"){
      data <- rnorm(n, 0, 10)
    }
  }
  df <- data.frame(x = data)
  return(df)
}


.readInputSequenceForLSTdesc <- function(jaspResults, options){
  inputSequence <- options[["lstDescDataSequenceInput"]]
  inputSequence <- unlist(strsplit(inputSequence, split = ","))
  inputSequence <- gsub(" ", "", inputSequence, fixed = TRUE)
  df <- data.frame(x = as.numeric(inputSequence))
  return(df)
}


.readDataLSTdesc <- function(jaspResults, options){
  variable <- unlist(options[["selectedVariable"]])
  variable <- variable[variable != ""]
  dataset <- .readDataSetToEnd(columns.as.numeric = variable)
  df <- data.frame(x = unlist(dataset))
  return(df)
}

.drawSpreadVisualization <- function(jaspResults, options, data, plotObject, yMax) {
  if (options[["LSdescS"]] == "LSdescRange") {
    minX <- min(data$x)
    maxX <- max(data$x)
    range <- maxX - minX
    minLineData <- data.frame(x = rep(minX, 2), y = c(0, yMax))
    maxLineData <- data.frame(x = rep(maxX, 2), y = c(0, yMax))
    rangeLineData <- data.frame(x = c(minX, maxX), y = rep(yMax * .95, 2))
    labelData <- data.frame(x = c(minX, maxX, (minX + maxX)/2),
                            y = c(yMax * .9, yMax * .9, yMax * .95),
                            label = c(gettextf("Min.: %.2f", minX), gettextf("Max.: %.2f", maxX), gettextf("Range: %.2f", range)))
    plotObject <- plotObject + 
      ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = minLineData, size = 1, color = "blue") +
      ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = maxLineData, size = 1, color = "red") +
      ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = rangeLineData, size = 1, color = "orange") +
      ggplot2::geom_label(mapping = ggplot2::aes(x = x, y = y, label = label), data = labelData, size = 5, 
                          color = c("blue", "red", "orange"))
  } else if (options[["LSdescS"]] == "LSdescQR") {
    quartiles <- quantile(data$x)
    iqr <- IQR(data$x)
    maxX <- max(data$x)
    q1LineData <- data.frame(x = rep(quartiles[2], 2), y = c(0, yMax))
    q2LineData <- data.frame(x = rep(quartiles[3], 2), y = c(0, yMax * .95))
    q3LineData <- data.frame(x = rep(quartiles[4], 2), y = c(0, yMax))
    iqrLineData <- data.frame(x = c(quartiles[2], quartiles[4]), y = rep(yMax * .95, 2))
    labelData <- data.frame(x = c(rep(maxX, 3), sum(quartiles[2], quartiles[4])/2),
                            y = c(yMax * c(.95, .85, .75), yMax * .95),
                            label = c(gettextf("1st quar. = %.2f", quartiles[2]), gettextf("2nd quar. / \n Median = %.2f", quartiles[3]),
                                      gettextf("3rd quar. = %.2f", quartiles[4]), gettextf("IQR = %.2f", iqr)))
    
    plotObject <- plotObject +
      ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = q1LineData, color = "purple", size = 1) +
      ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = q2LineData, color = "green", size = 1) +
      ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = q3LineData, color = "dodgerblue", size = 1) +
      ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = iqrLineData, color = "orange", size = 1) +
      ggplot2::geom_label(mapping = ggplot2::aes(x = x, y = y, label = label), data = labelData,
                          color = c("purple", "green", "dodgerblue", "orange"), size = 4)
  }else if (options[["LSdescS"]] == "LSdescSD") {
    stdDev <- sd(data$x)
    meanPoint <- mean(data$x)
    meanLineData <- data.frame(x = rep(meanPoint, 2), y = c(0, yMax * .95))
    labelData <- data.frame(x = rep(meanPoint, 2), y = yMax * c(.95, .85),
                            label = c(gettextf("Mean = %2.f", meanPoint), gettextf("SD = %2.f", stdDev)))
    
    minX <- min(data$x)
    maxX <- max(data$x)
    sdsMin <- round((abs(meanPoint - minX) / stdDev) + .5)
    sdsMax <- round((abs(meanPoint - maxX) / stdDev) + .5)
    colorPalette <- c("salmon4", "olivedrab4", "salmon2", "olivedrab2", "salmon", "olivedrab")
    for(i in 1:sdsMin){
      sdLineMax <- meanPoint - stdDev * (i-1)
      if (i != max(sdsMin)){
        sdLineMin <- meanPoint - stdDev * (i)
        sdLineData <- data.frame(x = c(sdLineMax, sdLineMin, sdLineMin),
                                 y = c(yMax * .85, yMax * .85, yMax * .8))
        sdLabelData <- data.frame(x = sdLineMin, y = yMax * .88, label = gettextf("-%i SD", i))
        plotObject <- plotObject +
          ggplot2::geom_label(mapping = ggplot2::aes(x = x, y = y, label = label), data = sdLabelData, size = 3,
                              color = colorPalette[i])
      } else {
        sdLineMin <- minX
        sdLineData <- data.frame(x = c(sdLineMax, sdLineMin), 
                                 y = rep(yMax * .85, 2))
      }
      plotObject <- plotObject +
        ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = sdLineData, color = colorPalette[i], size = 1)
    }
    for(i in 1:sdsMax){
      sdLineMin <- meanPoint + stdDev * (i-1)
      if (i != max(sdsMax)){
        sdLineMax <- meanPoint + stdDev * (i)
        sdLineData <- data.frame(x = c(sdLineMin, sdLineMax, sdLineMax),
                                 y = c(yMax * .85, yMax * .85, yMax * .8))
        sdLabelData <- data.frame(x = sdLineMax, y = yMax * .88, label = gettextf("+%i SD", i))
        plotObject <- plotObject +
          ggplot2::geom_label(mapping = ggplot2::aes(x = x, y = y, label = label), data = sdLabelData, size = 3,
                              color = colorPalette[i])
      } else {
        sdLineMin <- maxX
        sdLineData <- data.frame(x = c(sdLineMax, sdLineMin), 
                                 y = rep(yMax * .85, 2))
      }
      plotObject <- plotObject +
        ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = sdLineData, color = colorPalette[i], size = 1)
    }
    plotObject <- plotObject +
      ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = meanLineData, size = 1, color = "red") +
      ggplot2::geom_label(mapping = ggplot2::aes(x = x, y = y, label = label), data = labelData, size = 4,
                          color = c("red", "dodgerblue"))
  }
  return(plotObject)
}

.lstDescCreateHistogramOrBarplot <- function(jaspResults, options, data, ready, discrete, stats = c("ct", "spread")){
  title <- ifelse(discrete, "Barplot", "Histogram")
  jaspResults[["descHistogramOrBarplot"]] <- createJaspContainer(gettext(title))
  p <- createJaspPlot(title = gettext(title), width = 700, height = 400)
  p$position <- 2
  
  if (stats == "ct")
    allCTs <- options[["LSdescCT"]] == "LSdescMMM"
  
  if (ready) {
    if (discrete){
      xBreaks <- unique(as.integer(jaspGraphs::getPrettyAxisBreaks(data$x)))
      xStep <- xBreaks[2] - xBreaks[1]
      if (stats == "ct" && allCTs){
        xLimits <- c(xBreaks[1] - xStep/2, max(xBreaks) * 1.4)
      } else {
        xLimits <- c(min(xBreaks) - xStep/2, max(xBreaks) + xStep/2)
      }
      yBreaks <- unique(round(jaspGraphs::getPrettyAxisBreaks(c(0, table(data$x)))))
      yLimits <- range(yBreaks) * 1.3
      yMax <- max(yLimits)
      plotObject <- ggplot2::ggplot(data, ggplot2::aes(x = x)) + ggplot2::geom_bar(fill = "grey",
                                                                                   col = "black", size = .3)
      if (options[["LSdescHistBarRugs"]])
        plotObject <- plotObject + ggplot2::geom_rug(data = data, mapping = ggplot2::aes(x = x), sides = "b")
      plotObject <- plotObject + 
        ggplot2::scale_y_continuous(name = "Counts", breaks = yBreaks, limits = yLimits) + 
        ggplot2::scale_x_continuous(name = "Observations", breaks = xBreaks, limits = xLimits) + 
        jaspGraphs::geom_rangeframe() + jaspGraphs::themeJaspRaw()
    } else{
      displayDensity <- options[["LSdescHistCountOrDens"]] == "LSdescHistDens"
      xBreaks <- jaspGraphs::getPrettyAxisBreaks(data$x)
      xStep <- xBreaks[2] - xBreaks[1]
      if (stats == "ct" && allCTs) {
        xLimits <- c(min(xBreaks) - xStep/2, xBreaks[length(xBreaks)] * 1.4)
      } else {
        xLimits <- c(min(xBreaks) - xStep/2, max(xBreaks) + xStep/2)
      }
      plotObject <- jaspDescriptives:::.plotMarginal(data$x, variableName = "Observations", displayDensity = displayDensity,
                                                     binWidthType = "sturges", rugs = options[["LSdescHistBarRugs"]]) 
      yMax <- max(ggplot2::ggplot_build(plotObject)$data[[1]]$y) * 1.3
      yBreaks <- jaspGraphs::getPrettyAxisBreaks(ggplot2::ggplot_build(plotObject)$data[[1]]$y)
      yLimits <- c(0, yMax)
      plotObject <- plotObject + ggplot2::scale_y_continuous(limits = yLimits, breaks = yBreaks) +
        ggplot2::scale_x_continuous(name = "Observations", breaks = xBreaks, limits = xLimits) 
    }
    if (stats == "ct") {
      plotObject <- .drawMeanMedianOrModeLine(jaspResults, options, data, plotObject, yMax = yMax, discrete = discrete)
    } else if (stats == "spread") {
      plotObject <- .drawSpreadVisualization(jaspResults, options, data, plotObject, yMax = yMax)
    }
    p$plotObject <- plotObject
  }
  jaspResults[["descHistogramOrBarplot"]] <- p
}

.lstDescCreateDotplot <- function(jaspResults, options, data, ready, discrete, stats = c("ct", "spread")){
  jaspResults[["descDotplot"]] <- createJaspContainer(gettext("Dotplot"))
  height <- 400 + (length(data$x) / 50) * 25
  dp <- createJaspPlot(title = gettext("Dotplot"), width = 700, height = height)
  dp$position <- 3
  
  if (ready){
    dpPlotObjectList <- .lstDescCreateDotPlotObject(data, options, stats = stats)
    dpPlotObject <- dpPlotObjectList$p
    if (stats == "ct"){
      if ((options[["LSdescCT"]] == "LSdescMean" | options[["LSdescCT"]] == "LSdescMode" | options[["LSdescCT"]] == "LSdescMMM"))
        dpPlotObject <- .drawMeanMedianOrModeLine(jaspResults, options, data, dpPlotObject, yMax = dpPlotObjectList$yMax,
                                                  lines = FALSE, discrete = discrete)
    }
    dp$plotObject <- dpPlotObject
  }
  
  jaspResults[["descDotplot"]] <- dp
  
}

.drawMeanMedianOrModeLine <- function(jaspResults, options, data, plot, yMax, lines = TRUE, discrete){
  xBreaks <- pretty(data$x)
  n <- length(data$x)
  labelSize <- 3 + 300 / (100 + n * 2)
  if(options[["LSdescCT"]] == "LSdescMode"| options[["LSdescCT"]] == "LSdescMMM"){
    xPos <- median(xBreaks)
    modeLines <- lines
    noMode <- length(unique(data$x)) == length(data$x)
    if (noMode) {
      modeLabelData <- data.frame(x = xPos, y = yMax * .9, label = gettext("All values unique, \n no mode defined."))
      modeLines <- FALSE # also plot no line then
    } else {
      tableData <- table(data$x)
      modeCol <- tableData[tableData == max(tableData)]
      mode <- as.numeric(names(modeCol))
      modeHeight <- as.numeric(modeCol)
      plotData <- ggplot2::ggplot_build(plot)$data
      if (length(mode) == 1) {
        modeLabelText <- ifelse(discrete, gettextf("Mode = %i (Count = %i)", mode, modeHeight),
                                gettextf("Mode = %.2f (Count = %i)", mode, modeHeight))
        modeLabelData <- data.frame(x = xPos, y = yMax, label = modeLabelText)
      } else {
        firstMode <- ifelse(discrete, gettextf("Mode = {%i", mode[1]), gettextf("Mode = {%.2f", mode[1]))
        if (discrete) {
          otherModes <- gettextf(rep(", %i", length(mode) - 1), mode[2:length(mode)])
        } else {
          otherModes <- gettextf(rep(", %.2f", length(mode) - 1), mode[2:length(mode)])
        }
        otherModes <- paste(otherModes, collapse = "")
        modeHeightString <- gettextf( "} (Count = %i)", modeHeight)
        modeLabelText <- paste(firstMode, otherModes, modeHeightString, sep = "")
        modeLabelData <- data.frame(x = xPos, y = yMax, label = modeLabelText)
      }
    }
    if (modeLines){
      if (discrete) {
        modeLineYPos <- max(plotData[[1]]$ymax)
        for (i in 1:length(mode)){
          modeLineXPos <- plotData[[1]]$x[plotData[[1]]$xmin < mode[i] & plotData[[1]]$xmax > mode[i]][1]
          lineData <- data.frame(x = modeLineXPos, y = c(modeLineYPos, modeLineYPos * 1.05))
          plot <- plot + ggplot2::geom_path(data = lineData, mapping = ggplot2::aes(x = x, y = y), color = "blue", size = 3)
        }
      } else {
        modeLineYPos <- max(plotData[[2]]$ymax)
        for (i in 1:length(mode)){
          modeLineXPos <- plotData[[2]]$x[plotData[[2]]$xmin < mode[i] & plotData[[2]]$xmax > mode[i]][1]
          lineData <- data.frame(x = modeLineXPos, y = c(modeLineYPos, modeLineYPos * 1.05))
          plot <- plot + ggplot2::geom_path(data = lineData, mapping = ggplot2::aes(x = x, y = y), color = "blue", size = 3)
        }
      }
      plot <- plot + ggplot2::geom_hline(yintercept = modeLineYPos, size = 1, color = "blue") 
    }
    if (options[["LSdescCT"]] == "LSdescMode") {
      plot <- plot + ggplot2::geom_label(data = modeLabelData, 
                                         mapping = ggplot2::aes(x = x, y = y, label = label), color = "blue", size = labelSize)
    } else if (options[["LSdescCT"]] == "LSdescMMM") {
      modeLabelData$x <- max(xBreaks) * 1.2
      modeLabelData$y <- ifelse(noMode, yMax * 0.75, yMax * .8)
      plot <- plot + ggplot2::geom_label(data = modeLabelData, 
                                         mapping = ggplot2::aes(x = x, y = y, label = label), color = "blue", size = labelSize)
    }
  }
  if (options[["LSdescCT"]] == "LSdescMean" | options[["LSdescCT"]] == "LSdescMMM") {
    mean <- mean(data$x)
    if (lines)
      plot <- plot + ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y),
                                        data = data.frame(x = rep(mean, 2), y = c(0, yMax)), size = 1, color = "red")
    if (options[["LSdescCT"]] == "LSdescMean") {
      plot <- plot + ggplot2::geom_label(data = data.frame(x = mean, y = yMax, label = gettextf("Mean = %.2f", mean)), 
                                         mapping = ggplot2::aes(x = x, y = y, label = label), color = "red", size = labelSize)
    } else if (options[["LSdescCT"]] == "LSdescMMM") {
      plot <- plot + ggplot2::geom_label(data = data.frame(x = max(xBreaks) * 1.2, y = yMax, label = gettextf("Mean = %.2f", mean)), 
                                         mapping = ggplot2::aes(x = x, y = y, label = label), color = "red", size = labelSize)
    }
  }
  if (options[["LSdescCT"]] == "LSdescMedian"| options[["LSdescCT"]] == "LSdescMMM") {
    median <- median(data$x)
    if (lines)
      plot <- plot + ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y),
                                        data = data.frame(x = rep(median, 2), y = c(0, yMax)), size = 1, color = "green")
    if (options[["LSdescCT"]] == "LSdescMedian") {
      plot <- plot +  ggplot2::geom_label(data = data.frame(x = median, y = yMax, label = gettextf("Median = %.2f", median)), 
                                          mapping = ggplot2::aes(x = x, y = y, label = label), color = "green", size = labelSize)
    } else if (options[["LSdescCT"]] == "LSdescMMM") {
      plot <- plot + ggplot2::geom_label(data = data.frame(x = max(xBreaks) * 1.2, y = yMax * .9, label = gettextf("Median = %.2f", median)), 
                                         mapping = ggplot2::aes(x = x, y = y, label = label), color = "green", size = labelSize)
    }
  }
  return(plot)
}


.lstDescCreateDotPlotObject <- function(data, options, stats = c("ct", "spread")){
  n <- length(data$x)
  if (length(unique(data$x)) == 1){ 
    dotsize <- .0333
  } else if (n > 50){
    dotsize <- 1 - (log(n)) / 30
  } else {
    dotsize <- 1
  }
  
  labelSize <- 3 + 300 / (100 + n * 2)
  
  if (stats == "ct") {
    allCTs <- options[["LSdescCT"]] == "LSdescMMM"
  } else {
    allCTs <- FALSE
  }
  
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(data$x)
  xStep <- xBreaks[2] - xBreaks[1]
  xBuffer <- ifelse(allCTs, xStep * 2, xStep/2)
  xLimits <- c(min(xBreaks) - xStep/2, max(xBreaks) + xBuffer)
  
  p <- ggplot2::ggplot(data = data, ggplot2::aes(x = x)) +
    ggplot2::geom_dotplot(binaxis = 'x', stackdir = 'up', dotsize = dotsize, fill = "grey") +
    ggplot2::scale_x_continuous(name = "Value", breaks = xBreaks, limits = xLimits) +
    ggplot2::coord_fixed() 
  
  if (options[["LSdescDotPlotRugs"]])
    p <- p + ggplot2::geom_rug(data = data, mapping = ggplot2::aes(x = x), sides = "b")
  
  pData <- ggplot2::ggplot_build(p)$data
  dotWidth <- pData[[1]]$width[1] * dotsize
  yLabels <- unique(as.integer(jaspGraphs::getPrettyAxisBreaks(c(0, max(pData[[1]]$countidx)))))
  yBreaks <- yLabels * dotWidth
  yMax <- ifelse(max(yBreaks) < (10 * dotWidth), (10 * dotWidth), max(yBreaks))
  yLimits <-  c(0, yMax + (2 * dotWidth))
  
  p <- p + ggplot2::scale_y_continuous(name = "Counts", limits = yLimits, breaks = yBreaks, labels = yLabels) + 
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()
  if (stats == "ct"){
    if (options[["LSdescCT"]] == "LSdescMedian"| options[["LSdescCT"]] == "LSdescMMM") {
      p <- .dotPlotVisualizeMedian(data, p, dotsize, labelSize, yLimits, allCTs, labelText = gettext("Median"), quartile = 2)
    }
    if (options[["LSdescCT"]] == "LSdescMean" || options[["LSdescCT"]] == "LSdescMMM"){
      mean <- mean(data$x)
      y0 <- dotWidth / 2
      circleData <- data.frame(x0 = mean, 
                               y0 = y0,
                               r = dotWidth / 2)
      meanLineData <- data.frame(x = c(mean, mean),
                                 y = c(y0 + dotWidth / 2,  max(yLimits) * .95))
      p <- p + ggforce::geom_circle(data = circleData, mapping = ggplot2::aes(x0 = x0, y0 = y0, r = r),
                                    inherit.aes = FALSE, fill = "red", alpha = .3, color = "red", n = 4)
      if (!allCTs)
        p <- p + ggplot2::geom_path(data = meanLineData, mapping = ggplot2::aes(x = x, y = y), color = "red", size = .8)
    }
    if (options[["LSdescCT"]] == "LSdescMode" || options[["LSdescCT"]] == "LSdescMMM"){
      if(length(unique(data$x)) != n){
        modeTable <- table(data$x)
        mode <- as.numeric(names(modeTable[modeTable == max(modeTable)]))
        modeLineYPos <- dotWidth * max(pData[[1]]$count)
        for (i in 1:length(mode)){
          modeLineXPos <- pData[[1]]$x[pData[[1]]$xmin < mode[i] & pData[[1]]$xmax > mode[i]][1]
          lineData <- data.frame(x = modeLineXPos, y = c(modeLineYPos, modeLineYPos + dotWidth / 2))
          p <- p + ggplot2::geom_path(data = lineData, mapping = ggplot2::aes(x = x, y = y), color = "blue", size = 3)
        }
        p <- p + ggplot2::geom_hline(yintercept = modeLineYPos, color = "blue", size = 1)
      }
    }
  } else if (stats == "spread"){
    if (options[["LSdescS"]] == "LSdescRange") {
      p <- .dotPlotVisualizeRange(data, p, dotsize, yLimits)
    } else if (options[["LSdescS"]] == "LSdescQR"){
      p <- .dotPlotVisualizeMedian(data, p, dotsize, labelSize, yLimits, labelText = gettext("2nd quar. / \n Median"),
                                   quartile = 2)
      p <- .dotPlotVisualizeMedian(data, p, dotsize, labelSize, yLimits, labelText = gettext("1st quar."), quartile = 1)
      p <- .dotPlotVisualizeMedian(data, p, dotsize, labelSize, yLimits, labelText = gettext("3rd quar."), quartile = 3)
    }
  }
  return(list("p" = p, "yMax" = max(yLimits) * .95))
}

.dotPlotVisualizeRange <- function(data, plotObject, dotsize, yLimits){
  pData <- ggplot2::ggplot_build(plotObject)$data
  dotWidth <- pData[[1]]$width[1] * dotsize
  range <- max(data$x) - min(data$x)
  minDot <- pData[[1]][1,]
  maxDot <- pData[[1]][length(data$x),]
  minY0 <- dotWidth/2
  maxY0 <- ifelse(maxDot$countidx == 1, dotWidth/2, dotWidth/2 + (maxDot$countidx - 1) * dotWidth)
  circleData  <- data.frame(x0 = c(minDot$x, maxDot$x),
                            y0 = c(minY0, maxY0),
                            r = dotWidth/2)
  minLineData <- data.frame(x = rep(minDot$x, 2), y = c(0, max(yLimits)))
  maxLineData <-  data.frame(x = rep(maxDot$x, 2), y = c(0, max(yLimits)))
  rangeLineData <- data.frame(x = c(minDot$x, maxDot$x), y = rep(max(yLimits) * .95, 2))
  labelData <- data.frame(x = c(minDot$x, maxDot$x, range/2),
                          y = c(max(yLimits) * .9, max(yLimits) * .9, max(yLimits) * .95),
                          label = c(gettextf("Min.: %.2f", min(data$x)), gettextf("Max.: %.2f", max(data$x)), 
                                    gettextf("Range: %.2f", range)))
  plotObject <- plotObject +
    ggforce::geom_circle(data = circleData[1,], mapping = ggplot2::aes(x0 = x0, y0 = y0, r = r),
                         inherit.aes = FALSE, fill = "blue") +
    ggforce::geom_circle(data = circleData[2,], mapping = ggplot2::aes(x0 = x0, y0 = y0, r = r),
                         inherit.aes = FALSE, fill = "red") +
    ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = minLineData, color = "blue", size = 1) +
    ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = maxLineData, color = "red", size = 1) +
    ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = rangeLineData, color = "orange", size = 1) +
    ggplot2::geom_label(mapping = ggplot2::aes(x = x, y = y, label = label), data = labelData, size = 5,
                        color = c("blue", "red", "orange"))
  return(plotObject)
}

.dotPlotVisualizeMedian <- function(data, plotObject, dotsize, labelSize, yLimits, allCTs = FALSE, labelText, quartile) {
  n <- length(data$x)
  pData <- ggplot2::ggplot_build(plotObject)$data
  dotWidth <- pData[[1]]$width[1] * dotsize
  sortedDf <- data.frame(x = sort(data$x))
  location <- as.integer(quantile(as.numeric(rownames(sortedDf)))*2) / 2 # round to nearest half
  location <- location[quartile + 1]
  quartileValue <- quantile(data$x)[quartile + 1]
  if(location == as.integer(location)){   # if median is a single datapoint
    halfwayDot <- pData[[1]][location,]
    y0 <- ifelse(halfwayDot$countidx == 1, dotWidth/2, dotWidth/2 + (halfwayDot$countidx - 1) * dotWidth)
    x0 <- halfwayDot$x
    circleData <- data.frame(x0 = x0, 
                             y0 = y0,
                             r = dotWidth / 2)
    medianLineData <- data.frame(x = c(x0, quartileValue),
                                 y = c(y0, max(yLimits) * .9))
    chairData <- data.frame(x = c(x0 - dotWidth * .65, x0 - dotWidth * .65,
                                  x0 + dotWidth * .65, x0 + dotWidth * .65),
                            y = c(y0 + dotWidth/2, y0, y0, y0 - dotWidth/2))
    plotObject <- plotObject + ggforce::geom_circle(data = circleData, mapping = ggplot2::aes(x0 = x0, y0 = y0, r = r),
                                                    inherit.aes = FALSE, fill = "green") +
      ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = chairData, size = 1, color = "springgreen4")
    if (!allCTs)
      plotObject <- plotObject + ggplot2::geom_path(data = medianLineData, mapping = ggplot2::aes(x = x, y = y), color = "green",
                                                    size = 1)
  } else {  # if median is the average of two points
    halfwayDots <- list("lowerDot" = pData[[1]][location - .5,],
                        "upperDot" = pData[[1]][location + .5,])
    y0lower <- ifelse(halfwayDots$lowerDot$countidx == 1, dotWidth / 2, dotWidth / 2 + (halfwayDots$lowerDot$countidx - 1) * dotWidth)
    y0upper <- ifelse(halfwayDots$upperDot$countidx == 1, dotWidth / 2, dotWidth / 2 + (halfwayDots$upperDot$countidx - 1) * dotWidth)
    if (all(c(halfwayDots$lowerDot$count, halfwayDots$upperDot$count) == 1)) {
      start <- c(pi * 2, pi * 2)
      end <-c(pi * 3, pi)
    } else {
      start <- c(pi * 1.5, pi / 2)
      end <- c(pi * 2.5, pi * 1.5)
    }
    circleData <- data.frame(x0 = c(halfwayDots$lowerDot$x, halfwayDots$upperDot$x),
                             y0 = c(y0lower, y0upper),
                             r = rep(dotWidth/2, 2),
                             r0 = rep(0, 2),
                             start = start,
                             end = end)
    chairData <- data.frame(x = c(halfwayDots$lowerDot$x - dotWidth * .65, halfwayDots$lowerDot$x - dotWidth * .65,
                                  halfwayDots$lowerDot$x + dotWidth * .65, halfwayDots$lowerDot$x + dotWidth * .65),
                            y = c(y0lower + dotWidth, y0lower + dotWidth/2, y0lower + dotWidth/2, y0lower))
    medianLineData1 <- data.frame(x = c(halfwayDots$lowerDot$x, quartileValue),
                                  y = c(y0lower, max(yLimits) * .95))
    medianLineData2 <- data.frame(x = c(halfwayDots$upperDot$x, quartileValue),
                                  y = c(y0upper, max(yLimits) * .95))
    
    plotObject <- plotObject + 
      ggforce::geom_arc_bar(data = circleData,
                            mapping = ggplot2::aes(x0 = x0, y0 = y0, r0 = r0, r = r,  start = start, end = end),
                            inherit.aes = FALSE, fill = "green") +
      ggplot2::geom_path(mapping = ggplot2::aes(x = x, y = y), data = chairData, size = 1, color = "springgreen4")
    if(!allCTs){
      plotObject <- plotObject + ggplot2::geom_path(data = medianLineData1, mapping = ggplot2::aes(x = x, y = y), color = "green", size = 1) +
        ggplot2::geom_path(data = medianLineData2, mapping = ggplot2::aes(x = x, y = y), color = "green", size = 1)
    }
  }
  label <- paste(labelText, "= %.2f")
  if(!allCTs)
    plotObject <- plotObject + ggplot2::geom_label(data = data.frame(x = quartileValue, y = max(yLimits) * .95, label = gettextf(label, quartileValue)), 
                                                   mapping = ggplot2::aes(x = x, y = y, label = label), color = "green", size = labelSize)
  return(plotObject)
}
