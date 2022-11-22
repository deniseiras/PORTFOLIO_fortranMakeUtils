#!/usr/bin/python
# -*- coding: utf8 -*-

##########################################
# Refactor Unused Methods - removeUnusedMethods.py
# Author: Denis Eiras - denis.eiras@gmail.com
#
# Generates files:
#  ...
#
##########################################
import re
from shutil import copyfile, rmtree
import os



def isRoutine(candidate):
    return candidate.lower() in ["subroutine", "function"]


def isFunction(candidate):
    return candidate.lower() == "function"


def isDependency(candidate):
    return candidate.lower() in ["subroutine", "function", "interface"]


def isEndString(candidate):
    return candidate.lower() == "end"


def isComment(candidate):
    return candidate[0] == "!"


def isInterfaceRoutine(candidate):
    return candidate.lower() == "procedure"


def createAndOpenFileForWriting(fileSearchedName):

    if not os.path.exists(os.path.dirname(fileSearchedName)):
        try:
            os.makedirs(os.path.dirname(fileSearchedName))
        except OSError as exc: # Guard against race condition
            raise

    file = open(fileSearchedName, "w+")
    return file


# start program

fileName = "methodsNotinCallerTree.txt"
print("Opening objects file: {0} ...".format(fileName))

newFilesDir = "./refactored"
if os.path.exists(newFilesDir):
    rmtree(newFilesDir)
os.mkdir(newFilesDir)

with open(fileName) as file:
    dictMethodsFile = {}
    for lineMethods in file:
        strLineMethods = re.split("\t|\n", lineMethods)
        strLineMethods = filter(None, strLineMethods)
        if len(strLineMethods) > 0:
            methodSearched = strLineMethods[2]
            fileName = strLineMethods[0]
            methods = dictMethodsFile.get(fileName, set())
            methods.add(methodSearched)
            dictMethodsFile[fileName] = methods


    for fileSrcName in dictMethodsFile.keys():
        fileSrcNameNew = "{0}/{1}".format(newFilesDir, fileSrcName)
        print("Creating {0}".format(fileSrcNameNew))

        fileSrcNew = createAndOpenFileForWriting(fileSrcNameNew)
        isCopyNextLine = True
        with open(fileSrcName) as fileSrc:
            for lineSrc in fileSrc:
                strLineWords = lineSrc.split()

                if len(strLineWords) > 1 and isDependency(strLineWords[0]):
                    methodLine = strLineWords[1].split("(")[0].lower()
                    if methodLine in dictMethodsFile[fileSrcName]:
                        isCopyNextLine = False
                    else:
                        fileSrcNew.write(lineSrc)
                        isCopyNextLine = True

                elif len(strLineWords) > 2 and not isEndString(strLineWords[0]) and isFunction(strLineWords[1]):
                    methodLine = strLineWords[2].split("(")[0].lower()
                    if methodLine in dictMethodsFile[fileSrcName]:
                        isCopyNextLine = False
                    else:
                        fileSrcNew.write(lineSrc)
                        isCopyNextLine = True

                elif ( len(strLineWords) == 1 and isEndString(strLineWords[0]) ) or \
                        ( len(strLineWords) >=2 and isEndString(strLineWords[0]) and isDependency(strLineWords[1]) ) :
                    if isCopyNextLine:
                        fileSrcNew.write(lineSrc)

                    isCopyNextLine = True

                elif isCopyNextLine:
                    fileSrcNew.write(lineSrc)






















