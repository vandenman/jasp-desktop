#include "jaspPlot.h"

jaspPlot::~jaspPlot()
{
#ifdef JASP_RESULTS_DEBUG_TRACES
	JASPprint("Destructor of JASPplot("+title+") is called! ");
#endif

	finalizedHandler();
}

std::string jaspPlot::dataToString(std::string prefix)
{
	std::stringstream out;

	out << "{\n" <<
		prefix << "aspectRatio: "	<< _aspectRatio << "\n" <<
		prefix << "dims: "			<< _width << "X" << _height << "\n" <<
		prefix << "error: '"		<< _errorMessage << "'\n" <<
		prefix << "filePath: "		<< _filePathPng << "\n" <<
		"}";

	return out.str();
}

Json::Value jaspPlot::dataEntry()
{
	Json::Value data(Json::objectValue);

	data["title"]		= _title;
	data["convertible"]	= true;
	data["data"]		= _filePathPng;
	data["height"]		= _height;
	data["width"]		= _width;
	data["aspectRatio"]	= _aspectRatio;
	data["status"]		= _error == "" ? "complete" : "error";
	if(_error != "")
    {
		data["error"]					= Json::objectValue;
		data["error"]["type"]			= _error;
		data["error"]["errorMessage"]	= _errorMessage;
    }
	data["name"]		= getUniqueNestedName();

	return data;
}

void jaspPlot::addFootnote(std::string message, std::string symbol)
{
	Json::Value footnote(Json::objectValue);

	footnote["text"]	= message;
	footnote["symbol"]	= symbol;
	footnote["cols"]	= Json::nullValue;
	footnote["rows"]	= Json::nullValue;

	_footnotes.append(footnote);
}

void jaspPlot::setPlotObjSerialized(Rcpp::Vector<RAWSXP> plotSerialized)
{
	plotObjSerialized = plotSerialized;

	/*
	std::cout << "plotSerialized: ";
	for(char byte : plotSerialized)
		std::cout << byte;
	std::cout << std::endl << std::flush;*/
}




Json::Value jaspPlot::convertToJSON()
{
	Json::Value obj		= jaspObject::convertToJSON();

	obj["aspectRatio"]	= _aspectRatio;
	obj["width"]		= _width;
	obj["height"]		= _height;
	obj["error"]		= _error;
	obj["errorMessage"]	= _errorMessage;
	obj["filePathPng"]	= _filePathPng;
	obj["footnotes"]	= _footnotes;

	std::string serializedPlotObjString;
	serializedPlotObjString.reserve(plotObjSerialized.size());

	for(int i=0; i<plotObjSerialized.size(); i++)
		serializedPlotObjString.push_back(plotObjSerialized[i]);

	obj["plotObjSerialized"] = serializedPlotObjString;

	return obj;
}

void jaspPlot::convertFromJSON_SetFields(Json::Value in)
{
	jaspObject::convertFromJSON_SetFields(in);

	_aspectRatio	= in.get("aspectRatio", 0.0f).asDouble();
	_width			= in.get("width", -1).asInt();
	_height			= in.get("height", -1).asInt();
	_error			= in.get("error", "null").asString();
	_errorMessage	= in.get("errorMessage", "null").asString();
	_filePathPng	= in.get("filePathPng", "null").asString();
	_footnotes		= in.get("footnotes", Json::arrayValue);

	std::string jsonPlotObjStr = in.get("plotObjSerialized", "").asString();

	plotObjSerialized = Rcpp::Vector<RAWSXP>(jsonPlotObjStr.size());
	for(int i=0; i<plotObjSerialized.size(); i++)
		plotObjSerialized[i] = jsonPlotObjStr[i];

}
