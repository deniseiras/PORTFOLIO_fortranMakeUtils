#!/usr/bin/python
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

# Generates files:
#  - allMethodsCalled.txt: Displays all methods called for all source files
#  - allMethodsNotCalled.txt: Displays all methods not called for all source files
#  - methodsInCallerTree.txt: Displays all methods displayed in caller tree
#  - methodsNotInCallerTree.txt: Displays all methods minus the methodsInCallerTree.
#       This file is useful to work with the removeUnusedMethods.py script
#  - calleeTree.txt: Displays a tree containing which methods calls a method
#  - callerTree.txt: Displays a tree containing methods called from a method
#  - objects.mk: File to include in Makefile: Contains source ".f90" and objects ".o" files
#       This file is also usefull for use in removeUnusedFiles.py script
#  - depend.mk: File to include in Makefile: Shows the dependency between object files
#
##########################################

import re
import sys
import os


class Module:
    def __init__(self, filename, name):
        self.filename = filename
        self.name = name
        self.dependsOn = set()

    def __eq__(self, other):
        if isinstance(other, Module):
            return self.__repr__() == other.__repr__()
        else:
            return False

    def __ne__(self, other):
        return (not self.__eq__(other))

    def __repr__(self):
        return self.filename

    def __hash__(self):
        return hash(self.__repr__())

    def getOnlyFileName(self):
        return self.filename.rsplit('/', 1)[-1]

    def getObjectName(self):
        objectName = self.getOnlyFileName().replace(".f90", ".o").replace(".F90", ".o")
        return objectName


class Method:
    def __init__(self, name, module, methodType):
        self.name = name
        self.module = module
        self.methodType = methodType

    def __str__(self):
        return self.module.filename + "\t" + self.methodType + "\t" + self.name

    def __eq__(self, other):
        if isinstance(other, Method):
            # return ((self.name == other.name) and (self.module.name == other.module.name) and (self.methodType == other.methodType))
            return self.__repr__() == other.__repr__()
        else:
            return False

    def __ne__(self, other):
        return (not self.__eq__(other))

    def __repr__(self):
        return self.module.filename + "::" + self.name

    def __hash__(self):
        return hash(self.__repr__())


class CalleMethod:
    def __init__(self, method):
        self.method = method
        self.callees = set()
        self.callers = set()

    def __str__(self):
        return self.method.__str__()

    def __eq__(self, other):
        if isinstance(other, CalleMethod):
            return self.__repr__() == other.__repr__()
        else:
            return False

    def __ne__(self, other):
        return (not self.__eq__(other))

    def __repr__(self):
        return self.method.__repr__()

    def __hash__(self):
        return hash(self.__repr__())


def getFiles():
    files = []
    for root, dirs, filez in os.walk("."):
        for filename in filez:
            if filename.endswith('.f90') or filename.endswith('.F90'):
                files.append(root + "/" + filename)
    return files


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


def isModule(candidate):
    return candidate.lower() == "module"


def isUsedModule(candidade):
    return candidade.lower() == "use"


def addCalleAndCaller(calleeMethod, callerMethod, callees, callers):
    findCalee = [eachCallee for eachCallee in callees if eachCallee.method == calleeMethod]
    if len(findCalee) == 0:
        callee = CalleMethod(calleeMethod)
        callees.add(callee)
    else:
        callee = findCalee[0]

    findCaller = [eachCaller for eachCaller in callers if eachCaller.method == callerMethod]
    if len(findCaller) == 0:
        caller = CalleMethod(callerMethod)
        callers.add(caller)
    else:
        caller = findCaller[0]

    callee.callers.add(caller)
    caller.callees.add(callee)


def createModulesAndDependentsOfCaller(modules, caller):
    if len(caller.callees) > 0:
        for eachCallee in caller.callees:
            if caller.method.module != eachCallee.method.module:
                caller.method.module.dependsOn.add(eachCallee.method.module)
                # modules.add(eachCallee.method.module)
                createModulesAndDependentsOfCaller(modules, eachCallee)
    moduleCandidate = [m for m in modules if m == caller.method.module]
    if len(moduleCandidate) == 1:
        moduleCandidate = moduleCandidate[0]
        moduleCandidate.dependsOn = moduleCandidate.dependsOn.union(caller.method.module.dependsOn)
    else:
        moduleCandidate = caller.method.module
    modules.add(moduleCandidate)


def createModulesAndDependentsOfDependents(module, modulesUsedOrCalled):
    # if module not in modulesUsedOrCalled:
    for eachDep in module.dependsOn:
        createModulesAndDependentsOfDependents(eachDep, modulesUsedOrCalled)

    modDependent = [modDep for modDep in modulesDependents if modDep == module]
    if len(modDependent) > 0:
        modDependent = modDependent[0]
        module.dependsOn = module.dependsOn.union(modDependent.dependsOn)
        modulesUsedOrCalled.add(module)


def writeObjectDependeciesFile(modules):
    fileDepsName = "depend.mk"
    print("Creating dependecies file: {0} ...".format(fileDepsName))
    fileDeps = open(fileDepsName, 'w')
    for eachModule in modules:
        if len(eachModule.dependsOn) > 0:
            fileDeps.write(eachModule.getObjectName() + ":")
            for eachDep in eachModule.dependsOn:
                fileDeps.write(" " + eachDep.getObjectName())
            fileDeps.write("\n")


def writeObjectsFile(modules):
    fileObjsName = "objects.mk"
    print("Creating objects file: {0} ...".format(fileObjsName))
    fileObjs = open(fileObjsName, 'w')
    fileObjs.write("SRCS =")
    for eachModule in modules:
        fileObjs.write(" " + eachModule.getOnlyFileName())
    fileObjs.write("\n\nOBJS =")
    for eachModule in modules:
        fileObjs.write(" " + eachModule.getObjectName())


def createCalleeTree(allCallees):
    print("creating callee tree ...")
    calleeLevel = 0
    for callee in allCallees:
        if len(callee.callers) > 0:
            createCalleeTreeIntenal(callee, allCallees, calleeLevel)


def createCallerTree(allCallers):
    print("creating caller tree ...")
    callerLevel = 0
    for caller in allCallers:
        if len(caller.callees) > 0:
            createCallerTreeIntenal(caller, allCallers, callerLevel)


def createCalleeTreeIntenal(calle, allCalles, calleeLevel):
    if calleeLevel > MAX_LEVEL:
        return
    calleeLevel += 1
    for eachCaller in calle.callers:
        for eachCallee in allCalles:
            if eachCallee.method == eachCaller.method:
                eachCaller.callers = eachCallee.callers
                break
        createCalleeTreeIntenal(eachCaller, allCalles, calleeLevel)


def createCallerTreeIntenal(caller, allCallers, callerLevel):
    if callerLevel > MAX_LEVEL:
        return
    callerLevel += 1
    for eachCallee in caller.callees:
        for eachCaller in allCallers:
            if eachCaller.method == eachCallee.method:
                eachCallee.callees = eachCaller.callees
                break
        createCallerTreeIntenal(eachCallee, allCallers, callerLevel)


def writeCallees(allCallees):
    fileCalleeName = "calleeTree.txt"
    print("Creating callee tree file {0} ...".format(fileCalleeName))
    fileCallee = open(fileCalleeName, 'w')
    level = 0
    for callee in allCallees:
        writeCalleesInternal(fileCallee, callee, level)


def writeCallers(allCallers):
    fileCallerName = "callerTree.txt"
    print("Creating caller tree file " + fileCallerName + " ...")
    fileCaller = open(fileCallerName, 'w')
    level = 0
    for caller in allCallers:
        writeCallersInternal(fileCaller, caller, level)


def writeCalleesInternal(fileCallee, callee, level):
    if level > MAX_LEVEL:
        return
    fileCallee.write(("\t" * level) + callee.method.__str__() + "\n")
    if len(callee.callers) > 0:
        level += 1
        for eachCaller in callee.callers:
            writeCalleesInternal(fileCallee, eachCaller, level)
        level -= 1


def writeCallersInternal(fileCaller, caller, level):
    if level > MAX_LEVEL:
        return
    fileCaller.write(("\t" * level) + caller.method.__str__() + "\n")
    methodsInCallerTree.add(caller.method)
    if len(caller.callees) > 0:
        level += 1
        for eachCallee in caller.callees:
            writeCallersInternal(fileCaller, eachCallee, level)
        level -= 1


def writeCalledFile():
    fName = "allMethodsCalled.txt"
    print("Creating methods called of all files: file " + fName)
    fileCalled = open(fName, 'w')
    for each in sorted(methodsCalled):
        fileCalled.write(each.__str__() + "\n")


def writeNotCalledFile():
    fName = "allMethodsNotCalled.txt"
    print("Creating methods not called of all files: file " + fName)
    fileNotCalled = open(fName, 'w')
    for each in sorted(methodsNotCalled):
        fileNotCalled.write(each.__str__() + "\n")


def writeMethodsInCallerTreeFile():
    fName = "methodsInCallerTree.txt"
    print("Creating methods in Caller Tree: file " + fName)
    fileInCallerTree = open(fName, 'w')
    for method in sorted(methodsInCallerTree):
        fileInCallerTree.write(method.__str__() + "\n")


def writeMethodsNotInCallerTreeFile():
    fName = "methodsNotinCallerTree.txt"
    print("Creating methods not in Caller Tree: file " + fName)
    fileNotInCallerTree = open(fName, 'w')
    for method in sorted(methodsNotInCallerTree):
        fileNotInCallerTree.write(method.__str__() + "\n")


# start program ==================

methodsParameter = sys.argv
del methodsParameter[0]
if len(methodsParameter) != 1 and len(methodsParameter) != 3:
    print(" Please enter the parameters:")
    print(" 1 - Max depth for search callers and callees (mandatory)")
    print(" 2 - Filename of the interested routine.")
    print(" 3 - Interested routine to be searched.")
    exit()

MAX_LEVEL = int(methodsParameter[0])
sys.setrecursionlimit(999)

files = getFiles()
methods = set()
allMethods = set()
methodsCalled = set()
methodsNotCalled = set()
interfaceMethods = set()
callees = set()
callers = set()
methodsInCallerTree = set()
methodsNotInCallerTree = set()
modules = set()
modulesDependents = set()
modulesCalled = set()

print("Searching for modules and routines ( subroutines, functions, interfaces ) ...")
for filename in files:
    stringContinues = False
    strings = []
    with open(filename) as file:
        for line in file:
            stringsLine = re.split("\n| |,", line)
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

                if len(strings) > 1 and isDependency(strings[0]):
                    # TODO interface block (interface in 1 line, subroutine in second line)
                    method = strings[1].split("(")[0].lower()
                    mod = Module(filename, None)
                    met = Method(method, mod, strings[0].lower())
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
for filename in files:
    with open(filename) as file:
        insideModule = None
        for line in file:
            strings = line.split()
            if len(strings) > 0:
                if len(strings) > 1 and (isRoutine(strings[0]) or isFunction(strings[1])):
                    method = strings[1].split("(")[0].lower()
                    mod = Module(filename, None)
                    insideMethod = Method(method, mod, strings[0].lower())
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
                            methodsCalled.add(eachMethod)
                            addCalleAndCaller(eachMethod, insideMethod, callees, callers)

methodsNotCalled = methods - methodsCalled

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

writeCalledFile()
writeNotCalledFile()
writeCallees(callees)
writeCallers(callers)
writeMethodsInCallerTreeFile()


methodsNotInCallerTree = methods - methodsInCallerTree
writeMethodsNotInCallerTreeFile()

# add "use" dependents modules of "called" dependents modules
modulesUsedOrCalled = set()
for modCalled in modulesCalled:
    modDependent = [modDep for modDep in modulesDependents if modDep == modCalled]
    if len(modDependent) > 0:
        modDependent = modDependent[0]
        modCalled.dependsOn = modCalled.dependsOn.union(modDependent.dependsOn)
        createModulesAndDependentsOfDependents(modCalled, modulesUsedOrCalled)
    else:
        modulesUsedOrCalled.add(modCalled)

modulesUsedOrCalled = sorted(modulesUsedOrCalled, key=lambda x: x.getOnlyFileName().lower())
writeObjectsFile(modulesUsedOrCalled)
writeObjectDependeciesFile(modulesUsedOrCalled)
