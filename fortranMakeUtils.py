#!/usr/bin/python2.7
# -*- coding: utf8 -*-

##########################################
# Fortran Make Utils - fortranMakeUtils.py
# Author: Denis Eiras - denis.eiras@gmail.com
# Github: https://github.com/deniseiras/fortranMakeUtils
#
# Objective: Fortran Utility written in python for generating dependency tree and generating makefiles.
#
# Usage:
#
# Enter the parameters:
#
# 1 - Max depth for search callers and callees (mandatory)
# 2 - Filename of the interested routine - if you want to restrict the search to this file and subroutine.
# 3 - Interested routine to be searched - if you want to restrict the search to this file and subroutine.
#
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
#
# BUG:
# * stackoverflow when a program.f90 call a methods - must fix when using programs
# * allMethodsNotCalled.txt does not regards the father calller. calleTree.txt and callerTree.txt regards.
#
##########################################

import re
import sys
from fortranUtils import *


# start program ==================

print('Using python {}.{}'.format(sys.version_info[0], sys.version_info[1]))
if sys.version_info[0] < 3:
    raise Exception("Must be using Python 3")

methods = set()
allMethods = set()
methodsCalled = {}
methodsNotCalled = set()
interfaceMethods = set()
callees = set()
callers = set()
methodsInCallerTree = set()
methodsNotInCallerTree = set()
modules = set()
modulesDependents = set()
modulesCalled = set()

methodsParameter = getMethodsParameter()
if len(methodsParameter) != 2 and len(methodsParameter) != 4:
    print(" Please enter the parameters:")
    print(" 1 - Initial Directory (mandatory)")
    print(" 2 - Max depth for search callers and callees (mandatory)")
    print(" 3 - Filename of the interested routine.")
    print(" 4 - Interested routine to be searched.")
    exit()

MAX_LEVEL = getMaxLevel()
sys.setrecursionlimit(999)

filesInitialDir = getFiles(methodsParameter[0])
print("Searching for modules and routines ( subroutines, functions, interfaces ) ...")
for filename in filesInitialDir:
    stringContinues = False
    strings = []
    with open(filename) as file:
        for line in file:
            stringsLine = re.split("\n| |,", line)
            stringsLine = list(filter(None, stringsLine))
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

                if len(strings) > 1 and isDependency(strings[0]):
                    # TODO interface block (interface in 1 line, subroutine in second line)
                    method = strings[1].split("(")[0].lower()
                    mod = Module(filename, None)
                    met = Method(method, mod, strings[0].lower())
                    allMethods.add(met)
                if len(strings) > 1 and isRecursive(strings[0]) and isRoutine(strings[1]):
                    method = strings[2].split("(")[0].lower()
                    mod = Module(filename, None)
                    met = Method(method, mod, strings[1].lower())
                    allMethods.add(met)
                # logical function ... etc
                elif len(strings) > 2 and not isEndString(strings[0]) and isFunction(strings[1]):
                    method = strings[2].split("(")[0].lower()
                    mod = Module(filename, None)
                    met = Method(method, mod, strings[1].lower())
                    allMethods.add(met)
                elif len(strings) > 2 and isInterfaceRoutine(strings[1]):
                    for idx in range(2, len(strings)):
                        method = strings[idx].lower()
                        mod = Module(filename, None)
                        met = Method(method, mod, "procedure")
                        interfaceMethods.add(met)
                elif len(strings) > 1 and isModule(strings[0]):
                    mod = Module(filename, strings[1].lower())
                    modules.add(mod)

# removes subroutines and functions from interfaces ...
methods = allMethods - interfaceMethods

print("Searching for uses of modules and callers of routines ...")
for filename in filesInitialDir:
    with open(filename) as file:
        insideModule = None
        for line in file:
            strings = line.split()
            if len(strings) > 0:
                if len(strings) > 1 and isRoutine(strings[0]):
                    method = strings[1].split("(")[0].lower()
                    mod = Module(filename, None)
                    insideMethod = Method(method, mod, strings[0].lower())
                    continue
                # real function , recursive subroutine ...
                if len(strings) > 1 and not isEndString(strings[0]) and isRoutine(strings[1]):
                    method = strings[2].split("(")[0].lower()
                    mod = Module(filename, None)
                    insideMethod = Method(method, mod, strings[2].lower())
                    continue
                elif len(strings) == 2 and isModule(strings[0]):
                    insideModule = Module(filename, strings[1])
                    continue

                # not a module
                if insideModule is None:
                    insideModule = Module(filename, None)
                existentInsideModule = [eMod for eMod in modulesDependents if eMod == insideModule]
                if len(existentInsideModule) == 1:
                    insideModule = existentInsideModule[0]

                calledMethod = None
                strMethodCalled = None

                # using module
                if isUsedModule(strings[0]):
                    strUsedModule = strings[1].split(",")[0].lower()
                    modUsed = [mod for mod in modules if mod.name == strUsedModule]
                    if len(modUsed) > 0:  # maybe exists one module with same name in diferent files?
                        for eachModUsed in modUsed:
                            if insideModule != eachModUsed:

                                existentUsedMod = [eMod for eMod in modulesDependents if eMod == eachModUsed]
                                if len(existentUsedMod) == 0:
                                    existentUsedMod = eachModUsed
                                    modulesDependents.add(existentUsedMod)
                                else:
                                    existentUsedMod = existentUsedMod[0]

                                insideModule.dependsOn.add(existentUsedMod)
                                modulesDependents.add(insideModule)

                # calling subroutine
                if strings[0] == "call":
                    strMethodCalled = strings[1].split("(")[0].lower()
                    # TODO método chamado de qual módulo? insideModule está errado
                    calledMethod = Method(strMethodCalled, insideModule, "subroutine or interface")
                else:
                    # calling function or interface function
                    if not isDependency(strings[0]) and (len(strings) > 1 and not isFunction(strings[1])) and \
                            not isEndString(strings[0]) and not isComment(strings[0]):
                        for eachString in strings:
                            if "(" in eachString:
                                strMethodCalled = eachString.split("(")[0].lower()
                                for eachMethod in methods:
                                    if eachMethod.name == strMethodCalled: # TODO e método chamado está sendo usado !
                                        # TODO método chamado de qual módulo? insideModule está errado
                                        calledMethod = Method(strMethodCalled, insideModule, "function or interface")
                                        break

                if calledMethod is not None and calledMethod != insideMethod: # dont includes calls of self (Recursive)
                    for eachMethod in methods:
                        if eachMethod.name == calledMethod.name:
                            # TODO método chamado de qual módulo? pode também não ser módulo
                            # for eachMet in methods:
                            #     if eachMet.module in insideModule.dependsOn:
                            #         calledMethod = eachMet.module
                            if eachMethod in methodsCalled.keys():
                                methodsCalled[eachMethod] += 1
                            else:
                                methodsCalled[eachMethod] = 1

                            addCalleAndCaller(eachMethod, insideMethod, callees, callers)

methodsNotCalled = methods - set(list(methodsCalled.keys()))

createCalleeTree(callees)
createCallerTree(callers)
print("Creating modules dependencies ...")
if len(methodsParameter) == 3:
    rootFileName = methodsParameter[1]
    rootMethodName = methodsParameter[2].lower()
    rootMethod = [m for m in methods if rootFileName == m.module.filename and m.name == rootMethodName]
    if len(rootMethod) == 0:
        print("File " + rootFileName + " or/and method " + rootMethodName + " not found")
        exit()
    rootMethod = rootMethod[0]
    callees = [callee for callee in callees if callee.method == rootMethod]
    callers = [caller for caller in callers if caller.method == rootMethod]
    createModulesAndDependentsOfCaller(modulesCalled, callers[0])

else:
    for caller in callers:
        createModulesAndDependentsOfCaller(modulesCalled, caller)

writeCalledFile(methodsCalled)
writeNotCalledFile(methodsNotCalled)
writeCallees(callees)
writeCallers(callers, methodsInCallerTree)
writeMethodsInCallerTreeFile(methodsInCallerTree)


methodsNotInCallerTree = methods - methodsInCallerTree
writeMethodsNotInCallerTreeFile(methodsNotInCallerTree)

# add "use" dependents modules of "called" dependents modules
modulesUsedOrCalled = set()
for modCalled in modulesCalled:
    modDependent = [modDep for modDep in modulesDependents if modDep == modCalled]
    if len(modDependent) > 0:
        modDependent = modDependent[0]
        modCalled.dependsOn = modCalled.dependsOn.union(modDependent.dependsOn)
        createModulesAndDependentsOfDependents(modCalled, modulesUsedOrCalled, modulesDependents)
    else:
        modulesUsedOrCalled.add(modCalled)

modulesUsedOrCalled = sorted(modulesUsedOrCalled, key=lambda x: x.getOnlyFileName().lower())
writeObjectsFile(modulesUsedOrCalled)
writeObjectDependeciesFile(modulesUsedOrCalled)