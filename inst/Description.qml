import QtQuick
import JASP.Module

Description
{
	title:			qsTr("Learn Stats")
	icon:			"learning-stats.svg"
	description:	qsTr("Learn classical statistics with simple examples and supporting text")
	requiresData:	false
	hasWrappers: 	false
	
	
	
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
