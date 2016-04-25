
QT += core gui webkit webkitwidgets svg network printsupport

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

windows:CONFIG += c++11
linux:CONFIG += c++11


DESTDIR = ..

windows:TARGET = JASP
   macx:TARGET = JASP
  linux:TARGET = jasp

TEMPLATE = app

DEPENDPATH = ..

CONFIG -= app_bundle

INCLUDEPATH += ../JASP-Common/

   macx:INCLUDEPATH += ../../boost_1_54_0
windows:INCLUDEPATH += ../../boost_1_54_0

PRE_TARGETDEPS += ../libJASP-Common.a
PRE_TARGETDEPS += ../libicu-connector.a

LIBS += -L.. -lJASP-Common

#
# For code page conversion.
  linux:LIBS += -licu-connector -licuuc -licudata
   macx:LIBS += -licu-connector -licucore

windows:LIBS += -lboost_filesystem-mt -lboost_system-mt -larchive.dll
   macx:LIBS += -lboost_filesystem-mt -lboost_system-mt -larchive -lz
  linux:LIBS += -lboost_filesystem    -lboost_system    -larchive

windows:LIBS += -lole32 -loleaut32
  linux:LIBS += -lrt

QMAKE_CXXFLAGS += -Wno-c++11-extensions
QMAKE_CXXFLAGS += -Wno-unused-parameter
QMAKE_CXXFLAGS += -Wno-c++11-long-long
QMAKE_CXXFLAGS += -Wno-c++11-extra-semi

windows:QMAKE_CXXFLAGS += -DBOOST_USE_WINDOWS_H

linux {
        _R_HOME = $$(R_HOME)
        isEmpty(_R_HOME):_R_HOME = /usr/lib/R
        QMAKE_CXXFLAGS += -D\'R_HOME=\"$$_R_HOME\"\'
}

include(JASP-Desktop.pri)
