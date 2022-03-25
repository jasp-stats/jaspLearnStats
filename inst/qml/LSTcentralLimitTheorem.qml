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
import "../qml/qml_components" as LS

Form 
{
	columns: 1
	
	Section
	{
		title: qsTr("Parent Distribution")

		Group
		{
		
			DropDown
			{
				name:				"cltParentDistribution"
				label:				qsTr("Parent Distribution")
				indexDefaultValue:	0
				id:					cltParentDistribution
				values:
				[
					{label: qsTr("Normal"),		value: "normal"},
					{label: qsTr("Uniform"),	value: "uniform"},
					{label: qsTr("Skewed"),		value: "skewed"}
				]
			}
			
			CheckBox
			{
				name:		"parentShow"
				label:		qsTr("Show parent distribution")
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
			}
	
			DoubleField
			{
				name:			"cltStdDev"
				label:			qsTr("Std. Dev.")
				fieldWidth:		60
				defaultValue:	1
				decimals:		2
				min:			0.01
				visible:		cltParentDistribution.currentValue != "uniform"
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
				label:				qsTr("Skew Direction")
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
				label:				qsTr("Skew Intensity")
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
		}
	}
	
	Section
	{
		title: qsTr("Sample Options")
	
		Group
		{
	
			DoubleField
			{
				name:			"cltSampleSize"
				label:			qsTr("Number of observations per sample")
				fieldWidth:		60
				defaultValue:	30
				decimals:		0
			}
		
			DoubleField
			{
				name:			"cltSampleAmount"
				label:			qsTr("Number of total samples")
				fieldWidth:		60
				defaultValue:	100
				decimals:		0
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
				name:				"cltSampleShowType"
				label:				qsTr("Show Samples")
				id:					cltSampleShowType
				indexDefaultValue:	0
				values:
				[
					{ label: qsTr("First"),		value: "first"},
					{ label: qsTr("Last"),		value: "last"	},
					{ label: qsTr("Range"),			value: "range"		},
					{ label: qsTr("All"),			value: "all"		}
				]
			}

			DoubleField
			{
				name:			"cltFirstOrLastSamples"
				label:			qsTr("")
				fieldWidth:		60
				defaultValue:	7
				decimals:		0
				visible:		cltSampleShowType.currentValue == "first" | cltSampleShowType.currentValue == "last"
				max:			999
				}
				
				
			DoubleField
			{
				name:			"cltFromSample"
				label:			qsTr("From")
				fieldWidth:		60
				defaultValue:	1
				decimals:		0
				visible:		cltSampleShowType.currentValue == "range"
				min: 			1
			}
				
			DoubleField
			{
				name:			"cltToSample"
				label:			qsTr("To")
				fieldWidth:		60
				defaultValue:	7
				decimals:		0
				visible:		cltSampleShowType.currentValue == "range"
				max:			999
			}
		}
	}
	
	Section
	{
		title: qsTr("Sampling Distribution Options")
	
		CheckBox
		{
			name:		"samplingDistShow";
			label:		qsTr("Show sampling distribution");
			checked:	true
	
			CheckBox
			{
				name:		"samplingDistShowNormal";
				label:		qsTr("Superimpose normal distribution");
				checked:	true
			}
			
			CheckBox
			{
				name: "samplingDistShowRugs"
				label: qsTr("Show rug marks")
				checked: true
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
