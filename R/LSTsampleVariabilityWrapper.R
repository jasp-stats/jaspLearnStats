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

#' Sample Variability
#'
LSTsampleVariability <- function(
          data = NULL,
          version = "0.97.1",
          binomProb = 0.5,
          cltBinWidthType = "sturges",
          cltColorPalette = "colorblind",
          cltMean = 0,
          cltNumberOfBins = 30,
          cltParentDistribution = "normal",
          cltRange = 1,
          cltSampleAmount = 10,
          cltSampleSeed = 1,
          cltSampleSize = 10,
          cltSkewDirection = "left",
          cltSkewIntensity = "low",
          cltStdDev = 1,
          parentExplain = FALSE,
          parentShow = TRUE,
          plotHeight = 320,
          plotWidth = 480,
          samplesExplain = FALSE,
          samplesShow = TRUE,
          samplesShowRugs = TRUE,
          svFirstOrLastSamples = 7,
          svFromSample = 1,
          svParentSize = 100,
          svParentSizeType = "svParentInfinite",
          svSampleShowType = "first",
          svToSample = 7) {

   defaultArgCalls <- formals(jaspLearnStats::LSTsampleVariability)
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

   optionsWithFormula <- c("cltBinWidthType", "cltColorPalette", "cltParentDistribution", "cltSkewDirection", "cltSkewIntensity", "svSampleShowType")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspLearnStats", "LSTsampleVariability", "LSTsampleVariability.qml", options, version, TRUE))
}