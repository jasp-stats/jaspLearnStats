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
	
	Section
	{
		title: qsTr("Data options")
		expanded: true
		columns:	1
	
		RadioButtonGroup
		{
			columns:	3
			name:		"lstDescDataType"
			title:		qsTr("Data input type")
			id:			lstDescDataType

			RadioButton
			{
				value:		"dataSequence"
				label:		qsTr("Enter sequence")
				id:			dataTypeB
				checked:	true
			}
			
			RadioButton
			{
				value:		"dataRandom"
				label:		qsTr("Random sample")
				id:			dataTypeA
			}
			
			RadioButton
			{
				value:		"dataVariable"
				label:		qsTr("Select variable")
				id:			dataTypeC
				enabled:	dataSetInfo.dataAvailable
			}
		}
		
		Group
		{
			columns: 2
	
			DoubleField
			{
				name:			"lstDescSampleN"
				visible:		dataTypeA.checked
				label:			qsTr("Sample size")
				fieldWidth:		60
				defaultValue:	100
				decimals:		0
			}
			
			DoubleField
			{
				name:			"lstDescSampleSeed"
				visible:	dataTypeA.checked
				label:			qsTr("Set seed")
				fieldWidth:		60
				defaultValue:	123
				decimals:		0
			}
		}
		
		Group
		{
		
			RadioButtonGroup
			{
				columns:	3
				name:		"lstDescSampleDistType"
				visible:	dataTypeA.checked
				title:		qsTr("Distribution type")
				id:			distributionType
		
				RadioButton
				{
					value:		"lstSampleDistDiscrete"
					label:		qsTr("Discrete")
					id:			distTypeDisc
					checked:	true
				}
				
				RadioButton
				{
					value:		"lstSampleDistCont"
					label:		qsTr("Continuous")
					id:			distTypeCont
				}	
			}
			
			DropDown
			{
				name: "LSdescDiscreteDistributions"
				visible:	dataTypeA.checked && distTypeDisc.checked
				label: qsTr("Distribution")
				indexDefaultValue: 0
				id: lsDescDiscreteDistributions
				values:
				[
					{label: qsTr("Binomial distribution"),		value: "binomialDist"},
					{label: qsTr("Poisson distribution"),		value: "poissonDist"}
				]
			}
		
			DropDown
			{
				name: "LSdescContinuousDistributions"
				visible:	dataTypeA.checked && distTypeCont.checked
				label: qsTr("Distribution")
				indexDefaultValue: 0
				id: lsDescContinuousDistributions
				values:
				[
					{label: qsTr("Skewed normal distribution"),		value: "skewedNormal"},
					{label: qsTr("Uniform distribution"),		value: "uniform"},
					{label: qsTr("Normal distribution"),		value: "normal"}
				]
			}
		}
		
		TextArea
		{
			title:		qsTr("Comma-separated sequence of observations")
			visible:	dataTypeB.checked
			height:		100
			name:		"lstDescDataSequenceInput"
			textType:	JASP.TextTypeSource
			separators:	[",",";","\n"]
		}
		
		Group
		{
			visible: dataTypeC.checked
			
			VariablesForm
			{
				preferredHeight:	150
				
				AvailableVariablesList
				{
					name:	"allVariables"
					title:	qsTr("Available")
				}
				
				AssignedVariablesList
				{
					name:				"selectedVariable"
					title:				qsTr("Selected")
					singleVariable:		true
					allowedColumns:		["ordinal", "scale"]
				}
			}
		}


		RadioButtonGroup
		{
			title:									qsTr("Statistics to display")
			name:									"LSdescStatistics"
			columns:								3

			Group
			{	
				title:								qsTr("Central tendency measures")

				RadioButton
				{
					name:								"LSdescMean"
					label:								qsTr("Mean")
				}
				
				RadioButton
				{
					name:								"LSdescMedian"
					label:								qsTr("Median")
				}
				
				RadioButton
				{
				name:									"LSdescMode"
				label:									qsTr("Mode")
				}
				
				RadioButton
				{
					name:								"LSdescMMM"
					label:								qsTr("All")
				}

			}

			Group
			{	
				title:								qsTr("Spread measures")

				RadioButton
				{
					name:							"LSdescRange"
					label:							qsTr("Range")
				}
				
				RadioButton
				{
					name:							"LSdescQR"
					label:							qsTr("Quartiles")
				}
			
				RadioButton
				{
					name:							"LSdescSD"
					label:							qsTr("Std. dev.")
				}

			}

			Group
			{
				title:		" "

				RadioButton
				{
					name:							"none"
					label:							qsTr("None")
					checked:						true
				}
			}

			CheckBox
			{
				name:		"LSdescExplanation"
				label:		qsTr("Show explanation")
				checked:	false
				enabled:	false
			}	
		}
	}
	
	Section
	{
		title: qsTr("Plots")
		
		CheckBox
		{
			name:		"LSdescHistBar"
			label:		qsTr("Histogram / Barplot")
		
			RadioButtonGroup
			{
				name:	"LSdescHistCountOrDens"
			
				RadioButton
				{
					name:		"LSdescHistCount"
					label:		qsTr("Show counts")
					checked:	true
				}
			
				RadioButton
				{
					name:	"LSdescHistDens"
					label:	qsTr("Show density (histogram only)")
				}
			}
			
			CheckBox
			{
				name:		"LSdescHistBarRugs"
				label:		qsTr("Display rug marks")
				checked:	true
			}

			Group
			{
	
				DropDown
				{
					name:				"descBinWidthType"
					label:				qsTr("Histogram bin width type")
					indexDefaultValue:	0
					id:					binWidthType
					values:
					[
					{label: qsTr("Sturges"),				value: "sturges"},
					{label: qsTr("Scott"),					value: "scott"},
					{label: qsTr("Doane"),					value: "doane"},
					{label: qsTr("Freedman-Diaconis"),		value: "fd"	},
					{label: qsTr("Manual"),					value: "manual"	}
					]
				}
					
				DoubleField
				{
					name:			"descNumberOfBins"
					label:			qsTr("Number of bins")
					defaultValue:	30
					min:			3;
					max:			10000;
					enabled:		binWidthType.currentValue === "manual"
				}
			}
		}
		
		CheckBox
		{
			name:		"LSdescDotPlot"
			label:		qsTr("Dot plot")
			checked:	true

			CheckBox
			{
				name:		"LSdescDotPlotRugs"
				label:		qsTr("Display rug marks")
				checked:	true
			}
		}
		
		DropDown
		{
			name:				"descColorPalette"
			label:				qsTr("Color palette")
			indexDefaultValue:	0
			values:
			[
				{ label: qsTr("Colorblind"),		value: "colorblind"		},
				{ label: qsTr("Colorblind Alt."),	value: "colorblind3"	},
				{ label: qsTr("Viridis"),			value: "viridis"		},
				{ label: qsTr("ggplot2"),			value: "ggplot2"		},
				{ label: qsTr("Gray"),				value: "gray"			}
			]
		}
	}

	Section
	{
		title:		qsTr("Distribution options")
		visible:	dataTypeA.checked

		Group
		{
			title:		qsTr("Binomial distribution parameters")
			visible:	lsDescDiscreteDistributions.currentValue == "binomialDist" && distTypeDisc.checked

			DoubleField
			{
				name:			"binomialDistributionSuccessProbability"
				label:			qsTr("Probability of success (p)")
				defaultValue:	.5
				min:			0
				max:			1
			}

			DoubleField
			{
				name:			"binomialDistributionNumberOfTrials"
				label:			qsTr("Number of trials (k)")
				defaultValue:	10
				min:			1
			}
		}

		Group
		{
			title:		qsTr("Poisson distribution parameters")
			visible:	lsDescDiscreteDistributions.currentValue == "poissonDist" && distTypeDisc.checked

			DoubleField
			{
				name:			"poissonDistributionLambda"
				label:			qsTr("Rate (λ)")
				defaultValue:	1
				min:			0
			}
		}

		Group
		{
			title:		qsTr("Skewed normal distribution parameters")
			visible:	lsDescContinuousDistributions.currentValue == "skewedNormal" && distTypeCont.checked

			DoubleField
			{
				name:			"skewedNormalDistributionLocation"
				label:			qsTr("Location (ξ)")
				defaultValue:	0
				negativeValues:	true
			}

			DoubleField
			{
				name:			"skewedNormalDistributionScale"
				label:			qsTr("Scale (ω)")
				defaultValue:	1
				min:			0
			}

			DoubleField
			{
				name:			"skewedNormalDistributionShape"
				label:			qsTr("Shape (α)")
				defaultValue:	100
				negativeValues:	true
			}
		}

		Group
		{
			title:		qsTr("Uniform distribution parameters")
			visible:	lsDescContinuousDistributions.currentValue == "uniform" && distTypeCont.checked

			DoubleField
			{
				name:			"uniformDistributionLowerBound"
				label:			qsTr("Lower bound")
				defaultValue:	0
				id:				uniformDistributionLowerBound
				max: 			parseFloat(uniformDistributionUpperBound.value)
				negativeValues:	true
			}

			DoubleField
			{
				name:			"uniformDistributionUpperBound"
				label:			qsTr("Upper bound")
				defaultValue:	5
				id:				uniformDistributionUpperBound
				min: 			parseFloat(uniformDistributionLowerBound.value)
				negativeValues:	true
			}

		}

		Group
		{
			title:		qsTr("Normal distribution parameters")
			visible:	lsDescContinuousDistributions.currentValue == "normal" && distTypeCont.checked

			DoubleField
			{
				name:			"normalDistributionMean"
				label:			qsTr("Mean (μ)")
				defaultValue:	0
				negativeValues:	true
			}

			DoubleField
			{
				name:			"normalDistributionStdDev"
				label:			qsTr("Std. dev. (σ)")
				defaultValue:	10
				min:			0
			}
		}
	}
}
