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

.svPlotFinitePopulation <- function(jaspResults, options, data){
  n <- length(data$x)
  plotSize <- 700 + (n / 50) * 25
  plot <- createJaspPlot(title = gettext("Parent Distribution"), width = plotSize, height = plotSize)
  plot$position <- 1
  
  if (options[["cltParentDistribution"]] == "binomial") {
    set.seed(123)
    dummyData <- sort(runif(n = n, min = -3, max = 3))
    dotPlotData <- data.frame(x = dummyData, group = as.factor(data$x))
    dotSize <- .getDotSize(n)
    xBreaks <- jaspGraphs::getPrettyAxisBreaks(dummyData)
    plotObject <- ggplot2::ggplot() +
      ggplot2::geom_dotplot(data = dotPlotData, mapping = ggplot2::aes(x = x, group = group, fill = group), binaxis = 'x',
                            stackdir = 'up', dotsize = dotSize) +
      ggplot2::scale_fill_manual(values = c("grey20", "grey90")) +
      ggplot2::scale_x_continuous(name = "", breaks = xBreaks, labels = rep("", length(xBreaks))) #+
    #ggplot2::coord_fixed() 
    
    pData <- ggplot2::ggplot_build(plotObject)$data
    dotWidth <- pData[[1]]$width[1] * dotSize
    yLabels <- unique(as.integer(jaspGraphs::getPrettyAxisBreaks(c(0, max(pData[[1]]$countidx)))))
    yBreaks <- yLabels * dotWidth
    yLimits <-  range(yBreaks)
    
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


.svPlotFiniteSamples <- function(jaspResults, options, samples, indices, parentData) {
  samples <- samples[1:7]
  indices <- indices[1:7]
  plotMat <- matrix(list(), 4, 2)
  
  parentPopulationPlot <- .svPlotFinitePopulation(jaspResults, options, parentData)
  
  index <- 0
  for (i in 1:4) {
    for (j in 1:2) {
      index <-  index + 1
      if (index == 8) {
        samplePlot <- ggplot2::ggplot() + ggplot2::theme_void() + 
          ggplot2::annotate(geom = "text", x = 0, y = 0, label = gettextf("... until Sample Nr. %i",
                                                                          options[["cltSampleAmount"]]), size = 10)
      } else {
        samplePlot <- parentPopulationPlot$plotObject
      }
      plotMat[[i,j]] <- samplePlot
    }
  }
  sampleMatrixPlot <- createJaspPlot(title = gettext("Samples"), width = 1200, height = 1400)
  sampleMatrixPlot$position <- 2
  #sampleMatrixPlot$dependOn(options = c())
  
  sampleMatrixPlot$plotObject <- jaspGraphs::ggMatrixPlot(plotMat)
  
  return(sampleMatrixPlot)
}