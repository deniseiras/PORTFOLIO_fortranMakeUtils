#!/usr/bin/python
# -*- coding: utf8 -*-

##########################################
# Fortran Beautifier - fortranBeautifier.py
# Author: Denis Eiras - denis.eiras@gmail.com
# Github: https://github.com/deniseiras/fortranMakeUtils
#
# Objective: Fortran Utility written in python for beatifier Fortran code.
#
# Usage:
#
#    just run to beautify every fortran file recursively in 
# Enter the parameters:
#
# 1 - Filename
# 2 - Filename of the interested routine - if you want to restrict the search to this file and subroutine.
# 3 - Interested routine to be searched - if you want to restrict the search to this file and subroutine.

# Generates files:
#  - allMethodsCalled.txt: Displays all methods called for all source files.
#  - allMethodsNotCalled.txt: Displays all methods not called for all source files.
#  - methodsInCallerTree.txt: Displays all methods displayed in caller tree.
#  - methodsNotInCallerTree.txt: Displays all methods minus the methodsInCallerTree.
#       This file is useful to work with the removeUnusedMethods.py script.
#  - calleeTree.txt: Displays a tree containing which methods calls a method.
#  - callerTree.txt: Displays a tree containing methods called from a method.
#  - objects.mk: File to include in Makefile: Contains source ".f90" and objects ".o" files.
#       This file is also usefull for use in removeUnusedFiles.py script.
#  - depend.mk: File to include in Makefile: Shows the dependency between object files.
#
##########################################

import re
import shutil

from fortranUtils import getFiles, getMethodsParameter, isFortran95Keyword, isComment

# start program ==================
methodsParameter = getMethodsParameter()
if len(methodsParameter) != 1:
    print(" Please enter the parameters:")
    print(" 1 - Initial Directory (mandatory)")
    exit()

filesInitialDir = getFiles(methodsParameter[0])
print("Searching for fortran keywords ...")
for filename in filesInitialDir:
    oldFileName = filename + '.bak'
    shutil.copy(filename, oldFileName)
    with open(oldFileName) as oldFile:
        with open(filename, 'w') as newFile:
            stringContinues = False
            strings = []
            for line in oldFile:
                newLine = line
                stringsLine = re.split("[\n ,.():*]", line)
                stringsLine = filter(None, stringsLine)
                if len(stringsLine) > 0:
                    if stringContinues:
                        strings += stringsLine
                    else:
                        strings = stringsLine

                    if stringsLine[len(stringsLine) - 1] == "\&":
                        del strings[-1]
                        stringContinues = True
                        continue
                    else:
                        stringContinues = False
                    if not isComment(strings[0]):
                        for str in strings:
                            if isFortran95Keyword(str):
                                print('replacing ', str, ' in ', newLine)
                                strRegex = r"\b" + str + r"\b"
                                newLine = re.sub(strRegex, str.lower(), newLine)
                newFile.write(newLine)
