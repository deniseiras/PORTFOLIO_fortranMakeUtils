#!/usr/bin/python
# -*- coding: utf8 -*-

##########################################
# Remove unused files - removeUnusedFiles.py
# Author: Denis Eiras - denis.eiras@gmail.com
#
# Generates files:
#   - filesNotInSource.txt - Contains all files is current dir that are not listed in the SRC
#       section of the objects.mk files
#
##########################################
import re
import os


def getFiles():
    files = set()
    for root, dirs, filez in os.walk("."):
        for filename in filez:
            if filename.endswith('.f90') or filename.endswith('.F90') or filename.endswith('.c'):
                files.add(root + "/" + filename)
    return files


files = getFiles()
filesSources = set()
fileObjsName = "objects.mk"
print("Opening objects file: " + fileObjsName + " ...")

with open(fileObjsName) as file:
    for line in file:
        stringsLine = re.split("\n| ", line)
        stringsLine = filter(None, stringsLine)
        if len(stringsLine) > 0:
            for eachStr in stringsLine:

                if any(eachStr in s for s in [ "SRCS" "="]):
                    continue
                if eachStr == "OBJS":
                    break

                filesSources.add(eachStr)

fileInSources = set()
for eachSource in filesSources:
    e = [file for file in files if eachSource in file]
    if len(e) == 1:
        fileInSources.add(e[0])

filesNotInSources = sorted(files - fileInSources)

fileFilesNotInSourceFileName = "filesNotInSource.txt"
print("Writing file " + fileFilesNotInSourceFileName)
fileFilesNotInSource = open(fileFilesNotInSourceFileName, "w")
for eachFile in filesNotInSources:
    fileFilesNotInSource.write("rm -fv " + eachFile + "\n")
