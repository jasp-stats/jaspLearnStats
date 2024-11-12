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


Form
{
	Group
	{
		DoubleField
		{
			name: "mu"
			label: qsTr("μ")
			negativeValues: true
		}
		DoubleField
		{
			name: "sigma"
			label: qsTr("σ")
			negativeValues: false
			max: 100
			defaultValue: 1
		}
		IntegerField
		{
			name: "n"
			label: qsTr("Sample size (n)")
			min: 2
			max: 10000
			defaultValue: 10
		}
		CIField
		{
			name: "confidenceIntervalInterval"
			label: qsTr("Confidence level")
		}
		IntegerField
		{
			name: "nReps"
			label: qsTr("Repetitions")
			min: 1
			max: 10000
			defaultValue: 100
			id:nReps
		}
		SetSeed{}
	}
	Group
	{
		Slider
		{
			name: "dataPlotShowN"
			label: qsTr("Show last")
			min: 1
			max: nReps.value
			value: 3
			decimals: 0
			vertical: true
			id: dataSlider
			Layout.leftMargin: (increaseSampleTen.x + increaseSampleTen.width) / 1.9  - textField.width
			onValueChanged: moved()
		}

		RowLayout
		{
			Layout.topMargin: 10 * preferencesModel.uiScale
			spacing: jaspTheme.generalAnchorMargin
			Button
			{
				id: 								decreaseSampleTen
				text:								qsTr("<b>- 10</b>")
				onClicked:							dataSlider.value -= 10
			}
			Button
			{
				id: 								decreaseSample
				text:								qsTr("<b>- 1</b>")
				onClicked:							dataSlider.value -= 1
			}
			Button
			{
				Layout.leftMargin:						preferencesModel.uiScale
				id: 								increaseSample
				text:								qsTr("<b>+ 1</b>")
				onClicked:							dataSlider.value += 1
			}

			Button
			{
				id: 								increaseSampleTen
				text:								qsTr("<b>+ 10</b>")
				onClicked:							dataSlider.value += 10
			}
		}
	}

	Divider{}

	Group
	{
		Layout.columnSpan:2
		title: qsTr("Plots")
		CheckBox
		{
			name: "treePlot"
			label: qsTr("Tree plot")
			checked: true
			CheckBox
			{
				name: "tableTreePlot"
				label: qsTr("Display table")
				checked: false
			}
			CheckBox
			{
				name: "fixAxisTreePlot"
				label: qsTr("Fix x-axis from ")
				checked: false
				childrenOnSameRow: true
				DoubleField
				{
					name: "fixAxisLower"
					label: ""
					negativeValues: true
					defaultValue: -2
					afterLabel: qsTr(" to ")
				}
				DoubleField
				{
					name: "fixAxisUpper";
					label: ""
					negativeValues: true
					defaultValue: 2
				}
			}
		}

		CheckBox
		{
			name:	"dataPlot"
			label:	qsTr("Data distribution plots")

			Group
			{

				columns:	3

				DropDown
				{
					name:				"ciSampleShowType"
					label:				qsTr("Show samples")
					id:					ciSampleShowType
					indexDefaultValue:	0
					values:
					[
						{ label: qsTr("First"),		value: "first"	},
						{ label: qsTr("Last"),		value: "last"	},
						{ label: qsTr("Range"),		value: "range"	},
						{ label: qsTr("All"),		value: "all"	}
					]
				}

				IntegerField
				{
					name:			"ciFirstOrLastSamples"
					label:			""
					fieldWidth:		60
					defaultValue:	7
					visible:		ciSampleShowType.currentValue == "first" | ciSampleShowType.currentValue == "last"
					max:			ciSampleAmount.value
					min:			1
					}


				IntegerField
				{
					name:			"ciFromSample"
					id:				ciFromSample
					label:			qsTr("From")
					fieldWidth:		60
					defaultValue:	1
					visible:		ciSampleShowType.currentValue == "range"
					min: 			1
					max:			ciToSample.value
				}

				IntegerField
				{
					name:			"ciToSample"
					id:				ciToSample
					label:			qsTr("To")
					fieldWidth:		60
					defaultValue:	7
					visible:		ciSampleShowType.currentValue == "range"
					max:			ciSampleAmount.value
					min:			ciFromSample.value
				}
			}
		}

		CheckBox
		{
			name: "convergencePlot"
			label: qsTr("Convergence plot")
			CheckBox
			{
				name: "convergenceZoomIn"
				label: qsTr("Zoom in with margin")
				childrenOnSameRow: true
				DoubleField
				{
					name: "convergenceZoomMargin"
					label: ""
					negativeValues: false
					defaultValue: 0.2
					min: 0.01
					max: 0.4
				}
			}

		}
	}
}

