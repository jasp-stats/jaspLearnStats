import QtQuick 		2.12
import JASP.Module 	1.0

Description
{
	title :			qsTr("Learn Stats")
	name: 			"jaspLearnStats"
	icon:			"learning-stats.svg"
	description:	qsTr("Learn classical statistics with simple examples and supporting text")
	version			: "0.18.0"
	requiresData:	false
	author:			"JASP Team"
	maintainer:		"JASP Team <info@jasp-stats.org>"
	website:		"www.jasp-stats.org"
	license:		"GPL (>= 2)"
	
	
	Analysis
	{
		title:	qsTr("Normal Distribution")
		qml:	"LSTnormDist.qml"
		func:	"LSTnormDist"
	}
	
	Analysis
	{
		title:	qsTr("Binomial Distribution")
		qml:	"LSTbinomDist.qml"
		func:	"LSTbinomDist"
	}

	Analysis
	{
		title:	qsTr("Central Limit Theorem")
		qml:	"LSTcentralLimitTheorem.qml"
		func:	"LSTcentralLimitTheorem"
	}

	Analysis
	{
		title:	qsTr("Standard Error")
		qml:	"LSTstandardError.qml"
		func:	"LSTstandardError"
	}
	
	Analysis
	{
		title:	qsTr("Descriptive Statistics")
		qml:	"LSTdescriptives.qml"
		func:	"LSTdescriptives"
	}

	Analysis
	{
		title:	qsTr("Sample Variability")
		qml:	"LSTsampleVariability.qml"
		func:	"LSTsampleVariability"
	}

	Analysis
	{
		title:	qsTr("P Values")
		qml:	"LSTPValues.qml"
		func:	"pValues"
	}

	Analysis
	{
		title:	qsTr("Confidence Intervals")
		qml:	"LSTconfidenceIntervals.qml"
		func:	"LSTconfidenceIntervals"
	}
	
	Analysis
	{
		title:	qsTr("Effect Sizes")
		qml:	"LSeffectSizes.qml"
		func:	"LSeffectSizes"
	}
	
		Analysis
	{
		title:	qsTr("Statistical Test Decision Tree")
		qml:	"LSTdecisionTree.qml"
		func:	"LSTdecisionTree"
	}
}
