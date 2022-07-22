//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//
import QtQuick 2.8
import QtQuick.Layouts 1.3
import JASP.Controls 1.0
import JASP.Widgets 1.0
import JASP.Theme 1.0

Form 
{
	columns: 1
	
	RadioButtonGroup
	{
		name:                                   "nOutcomeVariables"
		title:			qsTr("How many outcome variables?")
		
		RadioButton
		{
			name:                              "nOutcomeOne"
			label:                              qsTr("One")		
			checked:                            true		
		}

		RadioButton
		{
			name:                               "nOutcomeMany"
			label:                              qsTr("Two or more")
		}
	}
	
		RadioButtonGroup
	{
		name:                                   "typeOutcomeVariables"
		title:			qsTr("What type of outcome?")
		
		RadioButton
		{
			name:                              "typeOutcomeCont"
			label:                              qsTr("Continuous")		
			checked:                            true		
		}

		RadioButton
		{
			name:                               "typeOutcomeCat"
			label:                              qsTr("Categorical")
		}
	}
	
	RadioButtonGroup
	{
		name:                                   "nPredictor"
		title:			qsTr("How many predictor variables?")
		id:				nPredictor
		
		RadioButton
		{
			name:                              "onePredictor"
			label:                              qsTr("One")		
			checked:                            true		
		}

		RadioButton
		{
			name:                               "twoPlusPredictors"
			label:                              qsTr("Two or more")
		}
	}
	
	RadioButtonGroup
	{
		name:                                   "typePredictor"
		id:										typePredictor
		title:						qsTr("What type of predictor?")
		
		RadioButton
		{
			name:                              "typePredictorCont"
			label:                              qsTr("Continuous")		
			checked:                            true		
		}

		RadioButton
		{
			name:                               "typePredictorCat"
			label:                              qsTr("Categorical")
		}
		
		RadioButton
		{
			name:                               "typePredictorBoth"
			label:                              qsTr("Both")
			visible:							nPredictor.value == "twoPlusPredictors"
		}
	}
	
	RadioButtonGroup
	{
		name:                                   "nCatPredictor"
		title:						qsTr("How many categories in predictor variable?")
		visible:					typePredictor.value == "typePredictorCat"
		
		RadioButton
		{
			name:                              "nCatPredictorTwo"
			label:                              qsTr("Two")		
			checked:                            true		
		}

		RadioButton
		{
			name:                              "nCatPredictorTwoPlus"
			label:                              qsTr("More than two")		
		}
	}
	
	RadioButtonGroup
	{
		name:                                   "repCatPredictor"
		title:						qsTr("Same or different entities in categories of predictor?")
		visible:					typePredictor.value == "typePredictorCat"
		
		RadioButton
		{
			name:                              "repCatPredictor"
			label:                              qsTr("Same")		
			checked:                            true		
		}

		RadioButton
		{
			name:                              "noRepCatPredictor"
			label:                              qsTr("Different")		
		}
	}
	
	RadioButtonGroup
	{
		name:                                   "parametricTest"
		title:						qsTr("Are the assumptions for a parametric test met?")
		
		RadioButton
		{
			name:                              "parametric"
			label:                              qsTr("Assumptions met")		
			checked:                            true		
		}

		RadioButton
		{
			name:                              "nonparametric"
			label:                              qsTr("Assumptions not met")		
		}
	}
}
