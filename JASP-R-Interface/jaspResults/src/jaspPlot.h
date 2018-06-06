#pragma once
#include "jaspObject.h"

class jaspPlot : public jaspObject
{
public:
	jaspPlot(std::string title = "") : jaspObject(jaspObjectType::plot, title) {}

	~jaspPlot();

	void addFootnote(std::string message, std::string symbol = "");

	float _aspectRatio;
	int _width, _height;
	std::string _error = "", _errorMessage = "", _filePathPng;

	///For safekeeping (aka state replacement?)
	void setPlotObjSerialized(Rcpp::Vector<RAWSXP> plotSerialized);
	Rcpp::Vector<RAWSXP> getPlotObjSerialized() { return plotObjSerialized; }

	std::string dataToString(std::string prefix) override;

	Json::Value	metaEntry() override { return constructMetaEntry("image"); }
	Json::Value	dataEntry() override;

	Json::Value convertToJSON() override;
	void		convertFromJSON_SetFields(Json::Value in) override;

private:
	Rcpp::Vector<RAWSXP> plotObjSerialized;
	Json::Value _footnotes = Json::arrayValue;
};


class jaspPlot_Interface : public jaspObject_Interface
{
public:
	jaspPlot_Interface(jaspObject * dataObj) : jaspObject_Interface(dataObj) {}

	void addFootnote(std::string message, std::string symbol = "")	{ ((jaspPlot*)myJaspObject)->addFootnote(message, symbol); }
	void setPlotObjSerialized(Rcpp::Vector<RAWSXP> plotSerialized)	{ ((jaspPlot*)myJaspObject)->setPlotObjSerialized(plotSerialized); }

	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(jaspPlot, std::string,	_error,			Error)
	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(jaspPlot, std::string,	_errorMessage,	ErrorMessage)
	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(jaspPlot, std::string,	_filePathPng,	FilePathPng)

	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(jaspPlot, float,			_aspectRatio,	AspectRatio)

	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(jaspPlot, int,			_width,			Width)
	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(jaspPlot, int,			_height,			Height)
};

RCPP_EXPOSED_CLASS_NODECL(jaspPlot_Interface)
