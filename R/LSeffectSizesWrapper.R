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

#' Effect Sizes
#'
LSeffectSizes <- function(
          data = NULL,
          version = "0.97.1",
          deltaCohensU3 = FALSE,
          deltaNumberNeededToTreat = FALSE,
          deltaOverlap = FALSE,
          deltaProbabilityOfSuperiority = FALSE,
          effectSize = "delta",
          effectSizeValueDelta = 0.5,
          effectSizeValuePhi = 0.5,
          effectSizeValueRho = 0.5,
          eventRate = 0.5,
          explanatoryTexts = FALSE,
          inputPopulation = FALSE,
          mu1 = 0,
          mu2 = 0,
          muC = 0,
          muE = 0,
          pX = 0.5,
          pX0Y0 = 0.25,
          pX0Y1 = 0.25,
          pX1Y0 = 0.25,
          pX1Y1 = 0.25,
          pY = 0.5,
          phiOR = FALSE,
          phiRD = FALSE,
          phiRR = FALSE,
          plotCombine = FALSE,
          plotDeltaRaincloud = FALSE,
          plotHeight = 320,
          plotPhiMosaic = FALSE,
          plotPhiProportions = FALSE,
          plotRhoRegression = FALSE,
          plotWidth = 480,
          rhoSharedVariance = FALSE,
          seed = 1,
          setSeed = FALSE,
          sigma = 1,
          sigma1 = 1,
          sigma2 = 1,
          simulateData = FALSE,
          simulateDataN = 100) {

   defaultArgCalls <- formals(jaspLearnStats::LSeffectSizes)
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


   return(jaspBase::runWrappedAnalysis("jaspLearnStats", "LSeffectSizes", "LSeffectSizes.qml", options, version, TRUE))
}