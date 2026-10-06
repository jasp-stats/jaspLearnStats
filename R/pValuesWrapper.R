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

#' P Values
#'
pValues <- function(
          data = NULL,
          version = "0.97.1",
          alpha = 0.05,
          alternative = "twoSided",
          alternativeHypothesisFrequencyTable = FALSE,
          alternativeHypothesisPlotCriticalRegion = FALSE,
          alternativeHypothesisPlotPValues = FALSE,
          alternativeHypothesisPlotTestStatistics = FALSE,
          alternativeHypothesisPlotTestStatisticsOverlayAlternative = FALSE,
          alternativeHypothesisPlotTestStatisticsOverlayNull = FALSE,
          alternativeHypothesisReset = FALSE,
          alternativeHypothesisSimulate = FALSE,
          alternativeHypothesisStudiesToSimulate = 1,
          distribution = "normal",
          introText = FALSE,
          normalMean = 0,
          nullHypothesisFrequencyTable = FALSE,
          nullHypothesisPlotCriticalRegion = FALSE,
          nullHypothesisPlotPValues = FALSE,
          nullHypothesisPlotPValuesOverlayUniform = FALSE,
          nullHypothesisPlotTestStatistics = FALSE,
          nullHypothesisPlotTestStatisticsOverlayTheoretical = FALSE,
          nullHypothesisReset = FALSE,
          nullHypothesisSimulate = FALSE,
          nullHypothesisStudiesToSimulate = 1,
          plotHeight = 320,
          plotTheoretical = TRUE,
          plotTheoreticalCriticalRegion = FALSE,
          plotTheoreticalPValue = 0.3,
          plotTheoreticalStatistic = FALSE,
          plotTheoreticalTestStatistic = 1,
          plotWidth = 480,
          tDf = 30,
          tNcp = 0,
          testStatisticSpecificationType = "testStatistic") {

   defaultArgCalls <- formals(jaspLearnStats::pValues)
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

   optionsWithFormula <- c("alternative", "distribution")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspLearnStats", "pValues", "LSTPValues.qml", options, version, TRUE))
}