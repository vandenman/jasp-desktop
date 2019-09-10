﻿//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

#include "analysis.h"

#include <boost/bind.hpp>


#include "options/options.h"
#include "tempfiles.h"
#include "appinfo.h"
#include "dirs.h"
#include "analyses.h"
#include "analysisform.h"
#include "utilities/qutils.h"
#include "log.h"


Analysis::Analysis(Analyses* analyses, size_t id, std::string module, std::string name, std::string title, const Version &version, Json::Value *data) :
	QObject(analyses),
	_options(new Options()),
	_id(id),
	_module(module),
	_name(name),
	_titleDefault(title),
	_title(title),
	_version(version),
	_analyses(analyses)
{
	if (data)
		// We cannot set the options now because it sometimes needs more information from the QML file
		// (especially with OptionsTable that needs a template information).
		_optionsDotJASP = *data;

	setHelpFile("analyses/" + nameQ());
	bindOptionHandlers();
}

Analysis::Analysis(Analyses* analyses, size_t id, Modules::AnalysisEntry * analysisEntry, std::string title, Json::Value *data) :
	  QObject(analyses),
	  _options(new Options()),
	  _id(id),
	  _name(analysisEntry->title()),
	  _titleDefault(analysisEntry->title()),
	  _title(title == "" ? _titleDefault : title),
	  _version(AppInfo::version),
	  _moduleData(analysisEntry),
	  _dynamicModule(_moduleData->dynamicModule()),
	  _analyses(analyses)
{
	if (data)
		_optionsDotJASP = *data; //Same story as other constructor

	_codedReferenceToAnalysisEntry = analysisEntry->codedReference(); //We need to store this to be able to find the right analysisEntry after reloading the entries of a dynamic module (destroys analysisEntries)
	setHelpFile(dynamicModule()->helpFolderPath() + nameQ());
	bindOptionHandlers();
}

Analysis::~Analysis()
{
	const auto & cols = columnsCreated();

	if(cols.size() > 0)
		for(const std::string & col : cols)
			emit requestComputedColumnDestruction(tq(col));

	delete _options;
}

void Analysis::clearOptions()
{
	_options->clear();
}

bool Analysis::checkAnalysisEntry()
{
	try
	{
		if(_codedReferenceToAnalysisEntry != "" && _dynamicModule != nullptr)
			_moduleData = _dynamicModule->retrieveCorrespondingAnalysisEntry(_codedReferenceToAnalysisEntry);
		return true;
	}
	catch (Modules::ModuleException & e)
	{
		return false;
	}
}

void Analysis::bindOptionHandlers()
{
	_options->changed.connect(							boost::bind( &Analysis::optionsChangedHandler,						this, _1));
	_options->requestComputedColumnCreation.connect(	boost::bind( &Analysis::requestComputedColumnCreationHandler,		this, _1));
	_options->requestColumnCreation.connect(			boost::bind( &Analysis::requestColumnCreationHandler,				this, _1, _2));
	_options->requestComputedColumnDestruction.connect(	boost::bind( &Analysis::requestComputedColumnDestructionHandler,	this, _1));
}

void Analysis::abort()
{
	setStatus(Aborting);
	emit optionsChanged(this);
}


void Analysis::setResults(const Json::Value & results, const Json::Value & progress)
{
	_results = results;
	_progress = progress;
	if (_analysisForm)
		_analysisForm->clearErrors();
	emit resultsChangedSignal(this);
}

void Analysis::imageSaved(const Json::Value & results)
{
	_imgResults = results;
	emit imageSavedSignal(this);
}

void Analysis::imageEdited(const Json::Value & results)
{
    _imgResults = results;
	emit imageEditedSignal(this);
}


void Analysis::reload()
{
	_analyses->reload(this, true);
}

void Analysis::exportResults()
{
    emit _analyses->analysesExportResults();
}

void Analysis::refresh()
{
	setStatus(Empty);
	_revision++;
	TempFiles::deleteAll(_id);
	emit toRefreshSignal(this);

	if(_analysisForm)
		_analysisForm->refreshTableViewModels();
}

void Analysis::saveImage(const Json::Value &options)
{
	setStatus(Analysis::SaveImg);
	_saveImgOptions = options;
	emit saveImageSignal(this);
}

void Analysis::editImage(const Json::Value &options)
{
	setStatus(Analysis::EditImg);
	_saveImgOptions = options;
	emit editImageSignal(this);
}

void Analysis::rewriteImages()
{
	setStatus(Analysis::RewriteImgs);
	emit rewriteImagesSignal(this);
}

void Analysis::imagesRewritten()
{
	emit resultsChangedSignal(this);
}

Analysis::Status Analysis::parseStatus(std::string name)
{
	if		(name == "empty")			return Analysis::Empty;
	else if (name == "waiting")			return Analysis::Inited;
	else if (name == "running")			return Analysis::Running;
	else if (name == "complete")		return Analysis::Complete;
	else if (name == "initializing")	return Analysis::Initializing;
	else if (name == "RewriteImgs")		return Analysis::RewriteImgs;
	else if (name == "validationError")	return Analysis::ValidationError;
	else if (name == "aborted")			return Analysis::Aborted;
	else if (name == "SaveImg")			return Analysis::SaveImg;
	else if (name == "EditImg")			return Analysis::EditImg;
	else								return Analysis::FatalError;
}

void Analysis::initialized(AnalysisForm* form, bool isNewAnalysis)
{
	_analysisForm	= form;
	_status			= isNewAnalysis ? Empty : Complete;
	
	connect(_analyses, &Analyses::dataSetChanged,			_analysisForm, &AnalysisForm::dataSetChangedHandler);
	connect(_analyses, &Analyses::dataSetColumnsChanged,	_analysisForm, &AnalysisForm::dataSetChangedHandler); //Really should be renamed
}

Json::Value Analysis::asJSON() const
{
	Json::Value analysisAsJson = Json::objectValue;

	analysisAsJson["id"]			= int(_id);
	analysisAsJson["name"]			= _name;
	analysisAsJson["title"]			= _title;
	analysisAsJson["titleDef"]		= _titleDefault;
	analysisAsJson["rfile"]			= _rfile;
	analysisAsJson["module"]		= _module;
	analysisAsJson["progress"]		= _progress;
	analysisAsJson["version"]		= _version.asString();
	analysisAsJson["results"]		= _results;

	std::string status;

	switch (_status)
	{
	case Analysis::Empty:			status = "empty";			break;
	case Analysis::Inited:			status = "waiting";			break;
	case Analysis::Running:			status = "running";			break;
	case Analysis::Complete:		status = "complete";		break;
	case Analysis::Aborted:			status = "aborted";			break;
	case Analysis::SaveImg:			status = "SaveImg";			break;
	case Analysis::EditImg:			status = "EditImg";			break;
	case Analysis::RewriteImgs:		status = "RewriteImgs";		break;
	case Analysis::ValidationError:	status = "validationError";	break;
	case Analysis::Initializing:	status = "initializing";	break;
	default:						status = "fatalError";	break;
	}

	analysisAsJson["status"]	= status;
	analysisAsJson["options"]	= options()->asJSON();
	analysisAsJson["userdata"]	= userData();

	if(_moduleData != nullptr)
		analysisAsJson["dynamicModule"] = _moduleData->asJsonForJaspFile();

	return analysisAsJson;
}

void Analysis::loadExtraFromJSON(Json::Value & options)
{
	_titleDefault = options.get("titleDef", _titleDefault).asString();
	//The rest is already taken in from Analyses::createFromJaspFileEntry
}

void Analysis::setStatus(Analysis::Status status)
{
	if ((status == Analysis::Running || status == Analysis::Initing) && _version != AppInfo::version)
	{
		TempFiles::deleteList(TempFiles::retrieveList(_id));
		_version = AppInfo::version;
	}
	_status = status;
}

DataSet *Analysis::getDataSet() const
{
	return _analyses->getDataSet();
}

void Analysis::optionsChangedHandler(Option *option)
{
	if (_refreshBlocked)
		return;

	_status = Empty;
	_revision++;
	optionsChanged(this);
}


int Analysis::callback(Json::Value results)
{
	if (_status != Empty && _status != Aborted)
	{
		if (results != Json::nullValue)
		{
			_results = results;
			resultsChangedSignal(this);
		}
		return 0;
	}
	else
	{
		return 1;
	}
}

performType Analysis::desiredPerformTypeFromAnalysisStatus() const
{
	switch(status())
	{
	case Analysis::Empty:		return(usesJaspResults() ? performType::run : performType::init);
	case Analysis::SaveImg:		return(performType::saveImg);
	case Analysis::EditImg:		return(performType::editImg);
	case Analysis::RewriteImgs:	return(performType::rewriteImgs);
	case Analysis::Aborting:	return(performType::abort);
	default:					return(performType::run);
	}
}

std::string Analysis::qmlFormPath() const
{
	return "file:" + (_moduleData != nullptr	?
				_moduleData->qmlFilePath()	:
				Dirs::resourcesDir() + "/" + module() + "/qml/"  + name() + ".qml");
}

void Analysis::runScriptRequestDone(const QString& result, const QString& controlName)
{
	_analysisForm->runScriptRequestDone(result, controlName);
}

Json::Value Analysis::createAnalysisRequestJson(int ppi, std::string imageBackground)
{
	performType perform = desiredPerformTypeFromAnalysisStatus();

	switch(perform)
	{
	case performType::init:			setStatus(Analysis::Initing);	break;
	case performType::abort:		setStatus(Analysis::Aborted);	break;
	case performType::run:
	case performType::saveImg:
	case performType::editImg:
	case performType::rewriteImgs:	setStatus(Analysis::Running);	break;
	default:														break;
	}

	Json::Value json = Json::Value(Json::objectValue);

	json["typeRequest"]			= engineStateToString(engineState::analysis);
	json["id"]					= int(id());
	json["perform"]				= performTypeToString(perform);
	json["revision"]			= revision();
	json["rfile"]				= _moduleData == nullptr ? rfile() : "";
	json["jaspResults"]			= usesJaspResults();
	json["dynamicModuleCall"]	= _moduleData == nullptr ? "" : _moduleData->getFullRCall();


	if (!isAborted())
	{
		json["name"]			= name();
		json["title"]			= title();
		json["ppi"]				= ppi;
		json["imageBackground"] = imageBackground; //comes from engine representation!

		if (perform == performType::saveImg || perform == performType::editImg)
			json["image"] = getSaveImgOptions();
		else
		{
			json["options"]		= options()->size() == 0 ? optionsFromJASPFile() : options()->asJSON();
		}
	}

	return json;
}

void Analysis::setName(std::string name)
{
	if (_name == name)
		return;

	_name = name;
	emit nameChanged();
}

void Analysis::setHelpFile(QString helpFile)
{
	if (_helpFile == helpFile)
		return;

	_helpFile = helpFile;
	emit helpFileChanged(_helpFile);
}

void Analysis::setTitleQ(QString title)
{
	std::string strippedTitle	= title.simplified().toStdString();

	if(strippedTitle == "")
		strippedTitle = _titleDefault;

	if (_title == strippedTitle && strippedTitle == title.toStdString())
		return;

	_results["title"] = strippedTitle;
	_title = strippedTitle;
	
	emit titleChanged();
}

void Analysis::refreshAvailableVariablesModels()
{
	if(form() != nullptr)
		form()->refreshAvailableVariablesModels();
}

QString	Analysis::fullHelpPath(QString helpFileName)
{
	if(isDynamicModule())	return dynamicModule()->helpFolderPath() + helpFileName;
	else					return "analyses/" + helpFileName;
}

ComputedColumn * Analysis::requestComputedColumnCreationHandler(std::string columnName)
{
	Log::log() << "Analysis::requestComputedColumnCreationHandler(" << columnName << ")" << std::endl;

	return requestComputedColumnCreation(QString::fromStdString(columnName), this);
}

void Analysis::requestColumnCreationHandler(std::string columnName, int colType)
{
	Log::log() << "Analysis::requestColumnCreationHandler(" << columnName << ", " << colType << ")" << std::endl;

	return requestColumnCreation(QString::fromStdString(columnName), this, colType);
}

void Analysis::requestComputedColumnDestructionHandler(std::string columnName)
{
	Log::log() << "Analysis::requestComputedColumnDestructionHandler(" << columnName << ")" << std::endl;

	requestComputedColumnDestruction(QString::fromStdString(columnName));
}
