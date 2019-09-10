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

#include "boundqmltextarea.h"
#include "../analysis/analysisform.h"
#include "qmllistviewtermsavailable.h"
#include "r_functionwhitelist.h"
#include <QQmlProperty>
#include <QQuickItem>
#include <QQuickTextDocument>
#include <QFontDatabase>
#include "log.h"


BoundQMLTextArea::BoundQMLTextArea(QQuickItem* item, AnalysisForm* form) 
	: QMLItem(item, form), QObject(form), BoundQMLItem()
{

	QString textType = _item->property("textType").toString();

	if (textType == "lavaan")
	{
		connect(_form, &AnalysisForm::dataSetChanged,	this, &BoundQMLTextArea::dataSetChangedHandler,	Qt::QueuedConnection	);

		_textType = TextType::Lavaan;
		QMLListViewTermsAvailable* listView = new QMLListViewTermsAvailable(item, form); // Hack to get allVariablesModel
		_allVariablesModel = dynamic_cast<ListModelTermsAvailable*>(listView->model());
#ifdef __APPLE__
		_applyScriptInfo = "\u2318 + Enter to apply";
#else
		_applyScriptInfo = "Ctrl + Enter to apply";
#endif
		_item->setProperty("applyScriptInfo", _applyScriptInfo);

		int id = QFontDatabase::addApplicationFont(":/fonts/FiraCode-Retina.ttf");
		if(QFontDatabase::applicationFontFamilies(id).size() > 0)
		{
			QString family = QFontDatabase::applicationFontFamilies(id).at(0);

			QFont font(family);
			font.setStyleHint(QFont::Monospace);
			font.setPointSize(10);
			_item->setProperty("font", font);
		}

		QVariant textDocumentVariant = _item->property("textDocument");
		QQuickTextDocument* textDocumentQQuick = textDocumentVariant.value<QQuickTextDocument *>();
		if (textDocumentQQuick)
		{
			QTextDocument* doc = textDocumentQQuick->textDocument();
			_lavaanHighlighter = new LavaanSyntaxHighlighter(doc);
			//connect(doc, &QTextDocument::contentsChanged, this, &BoundQMLTextArea::contentsChangedHandler);

		}
		else
			Log::log()  << "No document object found!" << std::endl;
	}
	else if (textType == "model")
	{
		_textType = TextType::Model;

#ifdef __APPLE__
		_applyScriptInfo = "\u2318 + Enter to apply";
#else
		_applyScriptInfo = "Ctrl + Enter to apply";
#endif
		_item->setProperty("applyScriptInfo", _applyScriptInfo);

		int id = QFontDatabase::addApplicationFont(":/fonts/FiraCode-Retina.ttf");
		QString family = QFontDatabase::applicationFontFamilies(id).at(0);

		QFont font(family);
		font.setStyleHint(QFont::Monospace);
		font.setPointSize(10);
		_item->setProperty("font", font);
	}
	else if (textType == "Rcode")
	{
		_textType = TextType::Rcode;

#ifdef __APPLE__
		_applyScriptInfo = "\u2318 + Enter to apply";
#else
		_applyScriptInfo = "Ctrl + Enter to apply";
#endif
		_item->setProperty("applyScriptInfo", _applyScriptInfo);

		int id = QFontDatabase::addApplicationFont(":/fonts/FiraCode-Retina.ttf");
		QString family = QFontDatabase::applicationFontFamilies(id).at(0);

		QFont font(family);
		font.setStyleHint(QFont::Monospace);
		font.setPointSize(10);
		_item->setProperty("font", font);
				
	}
	else
		_textType = TextType::Default;

	QQuickItem::connect(item, SIGNAL(applyRequest()), this, SLOT(checkSyntax()));
	
}

void BoundQMLTextArea::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionString *>(option);

	if (_boundTo != nullptr)
	{
		_text = QString::fromStdString(_boundTo->value());
		_item->setProperty("text", _text);
	}
	else
		Log::log()  << "could not bind to OptionBoolean in BoundQuickCheckBox.cpp" << std::endl;
}

Option *BoundQMLTextArea::createOption()
{
	std::string text = _item->property("text").toString().toStdString();
	return new OptionString(text);
}

bool BoundQMLTextArea::isOptionValid(Option *option)
{
	return dynamic_cast<OptionString*>(option) != nullptr;
}

bool BoundQMLTextArea::isJsonValid(const Json::Value &optionValue)
{
	return optionValue.type() == Json::stringValue;
}

void BoundQMLTextArea::resetQMLItem(QQuickItem *item)
{
	BoundQMLItem::resetQMLItem(item);
	_item->setProperty("text", _text);
	QQuickItem::connect(item, SIGNAL(applyRequest()), this, SLOT(checkSyntax()));	
}

void BoundQMLTextArea::checkSyntax()
{
	_text = _item->property("text").toString();
	if (_textType == TextType::Lavaan)
	{
		// create an R vector of available column names
		// TODO: Proper handling of end-of-string characters and funny colnames
		QString colNames = "c(";
		bool firstCol = true;
		if (_allVariablesModel)
		{
			QList<QString> vars = _allVariablesModel->allTerms().asQList();
			for (QString &var : vars)
			{
				if (!firstCol)
					colNames.append(',');
				colNames.append('\'')
						.append(var.replace("\'", "\\u0027")
								   .replace("\"", "\\u0022")
								   .replace("\\", "\\\\"))
						.append('\'');
				firstCol = false;
			}
		}
		colNames.append(')');
		
		// replace ' and " with their unicode counterparts
		// This protects against arbitrary code being run through string escaping.
		_text.replace("\'", "\\u0027").replace("\"", "\\u0022");
		// This protects against crashes due to backslashes
		_text.replace("\\", "\\\\");
		
		// Create R code string	
		QString checkCode = "checkLavaanModel('";
		checkCode
			.append(_text)
			.append("', ")
			.append(colNames)
			.append(")");
		
		runRScript(checkCode, false);
	}
	else if (_textType == TextType::Rcode)
	{
		try							
		{ 
			R_FunctionWhiteList::scriptIsSafe(_text.toStdString()); 
			_item->setProperty("hasScriptError", false);
			_item->setProperty("infoText", "valid R code");
			if (_boundTo != nullptr)
				_boundTo->setValue(_text.toStdString());			
		}
		catch(filterException & e)
		{
			_item->setProperty("hasScriptError", true);
			std::string errorMessage(e.what());
			_item->setProperty("infoText", errorMessage.c_str());
		}		
		
	}
	else
	{
		if (_boundTo != nullptr)
			_boundTo->setValue(_text.toStdString());
	}

}

void BoundQMLTextArea::dataSetChangedHandler()
{
	form()->refreshAnalysis();
}

void BoundQMLTextArea::rScriptDoneHandler(const QString & result)
{
    if (result.length() == 0) {
		_item->setProperty("hasScriptError", false);
		_item->setProperty("infoText", "Model applied");
		if (_boundTo != nullptr)
			_boundTo->setValue(_text.toStdString());
		
	} else {
		_item->setProperty("hasScriptError", true);
		_item->setProperty("infoText", result);
	}
}


