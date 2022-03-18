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


Form {

	RadioButtonGroup
	{
		name: 	"effectSize"
		id:		effectSize
		title:	qsTr("Effect Size")
  		
		RadioButton
		{
			value:		"delta"
			label:		qsTr("Cohen's δ")
			checked:	true
		}

		RadioButton
		{
			value:		"rho"
			label:		qsTr("Pearson correlation coefficient ρ")
			checked:	false
		}

		RadioButton
		{
			value:		"phi"
			label:		qsTr("Contingency coefficient φ")
			checked:	false
		}
	}


	Group
	{

		Group
		{
			visible:		effectSize.value == "delta"

			DoubleField
			{
				name:			"effectSizeValueDelta"
				label:			"δ"
				defaultValue:	0
			}

			DoubleField
			{
				name:			"mu"
				label:			"μ"
			}

			DoubleField
			{
				name:			"sigma"
				label:			"σ"
				min:			0
				defaultValue:	1
				inclusive: 		JASP.None
			}

			PercentField
			{
				name:			"eventRate"
				label:			qsTr("Event rate")
			}
		}

		Group
		{
			visible:		effectSize.value == "rho"
				
			DoubleField
			{
				
				name:			"effectSizeValueRho"
				label:			"ρ"
				defaultValue:	0
				min: 			-1
				max:			1
			}
			
/*
			Slider
			{
				name:			"effectSizeValueRho"
				visible:		effectSize.value == "rho"
				label:			"ρ"
				vertical:		false
				value:			0.3
				min:			-1
				max:			 1
			}
*/

			DoubleField
			{
				name:			"mu1"
				label:			"μ₁"
			}

			DoubleField
			{
				name:			"mu2"
				label:			"μ₂"
			}

			DoubleField
			{
				name:			"sigma1"
				label:			"σ₁"
				min:			0
				defaultValue:	1
				inclusive: 		JASP.None
			}

			DoubleField
			{
				name:			"sigma2"
				label:			"σ₂"
				min:			0
				defaultValue:	1
				inclusive: 		JASP.None
			}
		}

		Group
		{
			visible: 		effectSize.value == "phi"

			DoubleField
			{
				name:			"effectSizeValuePhi"
				label:			"φ"
				defaultValue:	0
				min: 			-1
				max:			1
			}
		

			DoubleField
			{
				name:			"pX"
				label:			"P(X=1)"
				min:			0
				max:			1
				defaultValue:	.5
				inclusive: 		JASP.None
			}

			DoubleField
			{
				name:			"pY"
				label:			"P(Y=1)"
				min:			0
				max:			1
				defaultValue:	.5
				inclusive: 		JASP.None
			}
		}



	}

	Divider { }

	Group
	{
		CheckBox
		{
			name:		"simulateData"
			label:		qsTr("Simulate data")
			checked:	false

			IntegerField
			{
				name:			"simulateDataN"
				label:			qsTr("No. observations")
				defaultValue:	100
				fieldWidth:		60
			}
		}
	}

}
