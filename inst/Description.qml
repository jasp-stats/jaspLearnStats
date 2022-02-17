import QtQuick 		2.12
import JASP.Module 	1.0

Description
{
	title :			qsTr("Learn Stats")
	name: 			"jaspLearnStats"
	icon:			"learning-stats.png"
	description:	qsTr("Learn classical statistics with simple examples and supporting text")
	version:		"0.15"
	requiresData:	false
	author:			"JASP Team"
	maintainer:		"JASP Team <info@jasp-stats.org>"
	website:		"www.jasp-stats.org"
	license:		"GPL (>= 2)"


	Analysis
	{
		title:	qsTr("Central Limit Theorem")
		qml:	"LSTcentralLimitTheorem.qml"
		func:	"LSTcentralLimitTheorem"
	}
	
		Analysis
	{
		title:	qsTr("Descriptive Statistics")
		qml:	"LSTdescriptives.qml"
		func:	"LSTdescriptives"
	}


}
