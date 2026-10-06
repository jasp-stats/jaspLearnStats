#
# Copyright (C) 2013-2025 University of Amsterdam
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

# This is a generated file. Don't change it!

#' Descriptive Statistics
#'
LSTdescriptives <- function(
          data = NULL,
          version = "0.97.1",
          LSdescContinuousDistributions = "skewedNormal",
          LSdescDiscreteDistributions = "binomialDist",
          LSdescDotPlot = TRUE,
          LSdescDotPlotRugs = TRUE,
          LSdescExplanation = FALSE,
          LSdescHistBar = FALSE,
          LSdescHistBarRugs = TRUE,
          LSdescHistCountOrDens = "LSdescHistCount",
          LSdescStatistics = "none",
          binomialDistributionNumberOfTrials = 10,
          binomialDistributionSuccessProbability = 0.5,
          descBinWidthType = "sturges",
          descColorPalette = "colorblind",
          descNumberOfBins = 30,
          lstDescDataSequenceInput = "",
          lstDescDataType = "dataSequence",
          lstDescSampleDistType = "lstSampleDistDiscrete",
          lstDescSampleN = 100,
          lstDescSampleSeed = 123,
          normalDistributionMean = 0,
          normalDistributionStdDev = 10,
          plotHeight = 320,
          plotWidth = 480,
          poissonDistributionLambda = 1,
          selectedVariable = list(types = list(), value = ""),
          skewedNormalDistributionLocation = 0,
          skewedNormalDistributionScale = 1,
          skewedNormalDistributionShape = 100,
          uniformDistributionLowerBound = 0,
          uniformDistributionUpperBound = 5) {

   defaultArgCalls <- formals(jaspLearnStats::LSTdescriptives)
   defaultArgs <- lapply(defaultArgCalls, eval)
   options <- as.list(match.call())[-1L]
   options <- lapply(options, eval)
   defaults <- setdiff(names(defaultArgs), names(options))
   options[defaults] <- defaultArgs[defaults]
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("LSdescContinuousDistributions", "LSdescDiscreteDistributions", "descBinWidthType", "descColorPalette", "lstDescDataSequenceInput", "selectedVariable")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspLearnStats", "LSTdescriptives", "LSTdescriptives.qml", options, version, TRUE))
}