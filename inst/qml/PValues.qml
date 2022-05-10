//
// Copyright (C) 2013-2021 University of Amsterdam
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

import QtQuick			2.12
import QtQuick.Layouts	1.3
import JASP.Controls	1.0
import JASP.Widgets		1.0
import JASP				1.0

Form
{
	columns: 2

	Group
	{
		title: qsTr("Theoretical distribution")
		DropDown
		{
			id:		distribution;
			name:	"distribution";
			values:
			[
				{ value: "normal",			label: qsTr("Normal") },
				{ value: "tDistribution",	label: qsTr("T distribution") }
			]
		}
		DoubleField { name: "tDf"; label: qsTr("df"); visible: distribution.currentValue === "tDistribution"; defaultValue: 1; negativeValues: false; inclusive: JASP.None	}
	}

	Group
	{
		title: qsTr("Test settings")
		PercentField
		{
			label:	qsTr("Type-I error rate α")
			name:	"alpha"
			defaultValue: 5
		}

		DropDown
		{
			label:	qsTr("Alt. Hypothesis")
			name:	"alternative"
			values:
			[
				{ value: "twoSided",	label: qsTr("Two sided")	},
				{ value: "greater",		label: qsTr("Greater")		},
				{ value: "less",		label: qsTr("Less")			}
			]
		}
	}

	CheckBox
	{
		label:		qsTr("Plot theoretical distribution")
		name:		"plotTheoretical"
		checked:	 true

		CheckBox
		{
			label:	qsTr("Highlight critical region")
			name:	"plotTheoreticalCriticalRegion"
		}

		CheckBox
		{
			label:	qsTr("Highlight specified test statistic")
			name:	"plotTheoreticalStatistic"

			DoubleField
			{
				label:	qsTr("Test statistic")
				name:	"plotTheoreticalTestStatistic"
				defaultValue:	1
				negativeValues: true
			}
		}
	}

	Section
	{
		title:	qsTr("Simulation under null hypothesis")
		expanded: true

		Group
		{
			title: qsTr("Simulation")
			columns: 1
			IntegerField
			{
				label:			qsTr("Number of studies to simulate")
				name:			"nullHypothesisStudiesToSimulate"
				defaultValue:	1
				max:			100000000
			}

			Button
			{
				label: qsTr("Simulate")
				onClicked: nullHypothesisSimulate.checked = !nullHypothesisSimulate.checked
				CheckBox
				{
					name: "nullHypothesisSimulate"; visible: false; id: nullHypothesisSimulate
				}
			}

			Button
			{
				label: qsTr("Reset")
				onClicked: nullHypothesisReset.checked = !nullHypothesisReset.checked
				CheckBox
				{
					name: "nullHypothesisReset"; visible: false; id: nullHypothesisReset
				}
			}
		}

		Group
		{
			title: qsTr("Output")
			CheckBox
			{
				name: "nullHypothesisPlotTestStatistics"
				label: qsTr("Plot test statistics")

				CheckBox
				{
					name: "nullHypothesisPlotTestStatisticsOverlayTheoretical"
					label: qsTr("Overlay theoretical distribution")
				}
			}

			CheckBox
			{
				name: "nullHypothesisPlotPValues"
				label: qsTr("Plot p-values")

				CheckBox
				{
					name: "nullHypothesisPlotPValuesOverlayUniform"
					label: qsTr("Overlay uniform distribution")
				}
			}

			CheckBox
			{
				name: "nullHypothesisFrequencyTable"
				label: qsTr("Frequency table")
			}
		}
	}

	Section
	{
		title:	qsTr("Simulation under alternative hypothesis")
		Group
		{
			Layout.columnSpan:	2
			title:				qsTr("Specify alternative sampling distribution")

			DoubleField { name: "normalMean";	label: qsTr("Mean");			visible: distribution.currentValue === "normal";		defaultValue: 0; negativeValues: true	}
			DoubleField { name: "tNcp";			label: qsTr("Non-centrality");	visible: distribution.currentValue === "tDistribution"; defaultValue: 0; negativeValues: true	}
		}

		Group
		{
			title: qsTr("Simulation")
			columns: 1
			IntegerField
			{
				label:			qsTr("Number of studies to simulate")
				name:			"alternativeHypothesisStudiesToSimulate"
				defaultValue:	1
				max:			100000000
			}

			Button
			{
				label: qsTr("Simulate")
				onClicked: alternativeHypothesisSimulate.checked = !alternativeHypothesisSimulate.checked
				CheckBox
				{
					name: "alternativeHypothesisSimulate"; visible: false; id: alternativeHypothesisSimulate
				}
			}

			Button
			{
				label: qsTr("Reset")
				onClicked: alternativeHypothesisReset.checked = !alternativeHypothesisReset.checked
				CheckBox
				{
					name: "alternativeHypothesisReset"; visible: false; id: alternativeHypothesisReset
				}
			}
		}

		Group
		{
			title: qsTr("Output")
			CheckBox
			{
				name: "alternativeHypothesisPlotTestStatistics"
				label: qsTr("Plot test statistics")

				CheckBox
				{
					name: "alternativeHypothesisPlotTestStatisticsOverlayNull"
					label: qsTr("Overlay theoretical null distribution")
				}

				CheckBox
				{
					name: "alternativeHypothesisPlotTestStatisticsOverlayAlternative"
					label: qsTr("Overlay theoretical alternative distribution")
				}
			}

			CheckBox
			{
				name: "alternativeHypothesisPlotPValues"
				label: qsTr("Plot p-values")
			}

			CheckBox
			{
				name: "alternativeHypothesisFrequencyTable"
				label: qsTr("Frequency table")
			}
		}
	}

	Section
	{
		title:	qsTr("Options")
		CheckBox	{	name: "introText"; label:	qsTr("Introductory text")	}
	}
}
