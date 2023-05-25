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
		title: qsTr("Parent Distribution")
		expanded: true

		Group
		{
		
		
			RadioButtonGroup
			{
				columns:	2
				name:		"svParentSizeType"
				title:		qsTr("Parent distribution size")
				id:			svParentSizeType
			
				RadioButton
				{
					value:		"svParentInfinite"
					label:		qsTr("Infinite")
					id:			sizeInfinite
					checked:	true
				}
			
				RadioButton
				{
					value:		"svParentFinite"
					label:		qsTr("Finite")
					id:			sizeFinite
				
					DoubleField
					{
						name:			"svParentSize"
						id:				svParentSize
						label:			qsTr("Size")
						fieldWidth:		60
						defaultValue:	100
						min: 1
						decimals:		0
					}
				}
			}
		
			DropDown
			{
				name:				"cltParentDistribution"
				label:				qsTr("Parent distribution shape")
				indexDefaultValue:	0
				id:					cltParentDistribution
				values:
				[
					{label: qsTr("Normal"),		value: "normal"},
					{label: qsTr("Uniform"),	value: "uniform"},
					{label: qsTr("Skewed"),		value: "skewed"},
					{label: qsTr("Binomial"),	value: "binomial"}
				]
			}
			
			CheckBox
			{
				name:		"parentShow"
				label:		qsTr("Show parent distribution")
				checked:	true
			}
			
			CheckBox
			{
				name:		"parentExplain"
				label:		qsTr("Show explanatory text")
				checked:	true
			}
		}
		
		Group
		{
			DoubleField
			{
				name:			"cltMean"
				label:			qsTr("Mean")
				fieldWidth:		60
				defaultValue:	0
				decimals:		2
				visible:		cltParentDistribution.currentValue != "binomial"

			}
	
			DoubleField
			{
				name:			"cltStdDev"
				label:			qsTr("Std. dev.")
				fieldWidth:		60
				defaultValue:	1
				decimals:		2
				min:			0.01
				visible:		cltParentDistribution.currentValue != "uniform" & cltParentDistribution.currentValue != "binomial"
			}
			
			DoubleField
			{
				name:			"cltRange"
				label:			qsTr("Range")
				fieldWidth:		60
				defaultValue:	1
				decimals:		2
				min:			0.01
				visible:		cltParentDistribution.currentValue == "uniform"
			}
	
			DropDown
			{
				name:				"cltSkewDirection"
				label:				qsTr("Skew direction")
				indexDefaultValue:	0
				visible:			cltParentDistribution.currentValue == "skewed"
				id:					cltSkewDirection
				values:
				[
					{label: qsTr("Left"),		value: "left"},
					{label: qsTr("Right"),		value: "right"}
				]
			}
	
			DropDown
			{
				name:				"cltSkewIntensity"
				label:				qsTr("Skew intensity")
				indexDefaultValue:	0
				visible:			cltParentDistribution.currentValue == "skewed"
				id:					cltSkewIntensity
				values:
				[
					{label: qsTr("Low skew"),		value: "low"},
					{label: qsTr("Medium skew"),		value: "medium"},
					{label: qsTr("High skew"),		value: "high"}
				]
			}
			
			DoubleField
			{
				name:			"binomProb"
				label:			qsTr("Probability")
				fieldWidth:		60
				defaultValue:	.5
				decimals:		2
				min:			0.01
				max:			1
				visible:		cltParentDistribution.currentValue == "binomial"
			}
		}
	}
	
	
	function getMaxSamples()
	{
		var max = 99999
		if(sizeFinite.checked)
		{
			max = svParentSize.value
		}
		return max
	}
	
	function getMinSamplesSV()
	{
		var min = 99999
		if(svSampleShowType.currentValue == "first" | svSampleShowType.currentValue == "last")
		{
			min = svFirstOrLastSamples.value
		} else
		{
			min = svToSample.value
		}
		return min
	}
	
	Section
	{
		title: qsTr("Sample Options")
	
		Group
		{
	
			IntegerField
			{
				name:			"cltSampleSize"
				label:			qsTr("Number of observations per sample")
				fieldWidth:		60
				defaultValue:	10
				max: 			99999
			}
		
			IntegerField
			{
				name:			"cltSampleAmount"
				id:				svSampleAmount
				label:			qsTr("Number of total samples")
				fieldWidth:		60
				defaultValue:	10
				max:			getMaxSamples()
				min:			getMinSamplesSV()
			}
		}
		
		Group
		{
		
			CheckBox
			{
				name:		"samplesShow";
				label:		qsTr("Show samples");
				checked:	true
	
				CheckBox
				{
					name:		"samplesShowRugs"
					label:		qsTr("Show rug marks")
					checked:	true
				}
			}
			
			CheckBox
			{
				name:		"samplesExplain"
				label:		qsTr("Show explanatory text")
				checked:	true
			}
		
			DoubleField
			{
				name:			"cltSampleSeed"
				label:			qsTr("Set seed")
				fieldWidth:		60
				defaultValue:	1
				decimals:		0
			}
		}
		
		Group
		{
		
		columns:	3
		
		DropDown
			{
				name:				"svSampleShowType"
				label:				qsTr("Show samples")
				id:					svSampleShowType
				indexDefaultValue:	0
				values:
				[
					{ label: qsTr("First"),		value: "first"},
					{ label: qsTr("Last"),		value: "last"	},
					{ label: qsTr("Range"),			value: "range"		},
					{ label: qsTr("All"),			value: "all"		}
				]
			}

				IntegerField
				{
					name:			"svFirstOrLastSamples"
					label:			""
					fieldWidth:		60
					defaultValue:	7
					visible:		svSampleShowType.currentValue == "first" | svSampleShowType.currentValue == "last"
					min: 			1
					max:			svSampleAmount.value
				}
				
				
				IntegerField
				{
					name:			"svFromSample"
					id:				svFromSample
					label:			qsTr("From")
					fieldWidth:		60
					defaultValue:	1
					visible:		svSampleShowType.currentValue == "range"
					max: 			svToSample.value
					min:			1
				}
				
				IntegerField
				{
					name:			"svToSample"
					id:				svToSample
					label:			qsTr("to")
					fieldWidth:		60
					defaultValue:	7
					visible:		svSampleShowType.currentValue == "range"
					max:			svSampleAmount.value
					min:			svFromSample.value
				}
		}
	}
	
	Section
	{
		title: qsTr("Plot Options")
	
		DropDown
		{
			name:				"cltColorPalette"
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
			
		Group
		{
	
			DropDown
			{
				name:				"cltBinWidthType"
				label:				qsTr("Bin width type")
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
				name:			"cltNumberOfBins"
				label:			qsTr("Number of bins")
				defaultValue:	30
				min:			3;
				max:			10000;
				enabled:		binWidthType.currentValue === "manual"
			}			
		}
	}
}
