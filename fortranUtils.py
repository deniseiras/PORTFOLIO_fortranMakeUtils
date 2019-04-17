import os
import sys


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


def getMethodsParameter():
    global methodsParameter
    if methodsParameter is None:
        methodsParameter = sys.argv
        del methodsParameter[0]
    return methodsParameter


def getMaxLevel():
    methodsParameter = getMethodsParameter()
    return int(methodsParameter[1])


def getFiles(initialDir):
    files = []
    for root, dirs, filez in os.walk(initialDir):
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


def isFortranIntrinsicFucntions(candidate):
    return candidate.lower() in [
        'abs', 'achar', 'acos', 'acosh', 'adjustl', 'adjustr', 'aimag', 'aint', 'all', 'allocated', 'anint', 'any',
        'asin', 'asinh', 'associated', 'atan', 'atan2', 'atanh', 'atomic_add', 'atomic_and', 'atomic_cas',
        'atomic_define', 'atomic_fetch_add', 'atomic_fetch_and', 'atomic_fetch_or', 'atomic_fetch_xor', 'atomic_or',
        'atomic_ref', 'atomic_xor', 'bessel_j0', 'bessel_j1', 'bessel_jn', 'bessel_y0', 'bessel_y1', 'bessel_yn', 'bge',
        'bgt', 'bit_size', 'ble', 'blt', 'btest', 'c_associated', 'c_funloc', 'c_f_procpointer', 'c_f_pointer', 'c_loc',
        'c_sizeof', 'ceiling', 'char', 'cmplx', 'co_broadcast', 'co_max', 'co_min', 'co_reduce', 'co_sum',
        'command_argument_count', 'compiler_options', 'compiler_version', 'conjg', 'cos', 'cosh', 'count', 'cpu_time',
        'cshift', 'date_and_time', 'dble', 'digits', 'dim', 'dot_product', 'dprod', 'dshiftl', 'dshiftr', 'eoshift',
        'epsilon', 'erf', 'erfc', 'erfc_scaled', 'event_query', 'execute_command_line', 'exp', 'exponent',
        'extends_type_of', 'float', 'floor', 'fraction', 'gamma', 'get_command', 'get_command_argument',
        'get_environment_variable', 'huge', 'hypot', 'iachar', 'iall', 'iand', 'iany', 'ibclr', 'ibits', 'ibset',
        'ichar', 'ieor', 'image_index', 'index', 'int', 'ior', 'iparity', 'is_iostat_end', 'is_iostat_eor', 'ishft',
        'ishftc', 'kind', 'lbound', 'lcobound', 'leadz', 'len', 'len_trim', 'lge', 'lgt', 'lle', 'llt', 'log', 'log10',
        'log_gamma', 'logical', 'maskl', 'maskr', 'matmul', 'max', 'maxexponent', 'maxloc', 'maxval', 'merge',
        'merge_bits', 'min', 'minexponent', 'minloc', 'minval', 'mod', 'modulo', 'move_alloc', 'mvbits', 'nearest',
        'new_line', 'nint', 'not', 'norm2', 'null', 'num_images', 'pack', 'parity', 'popcnt', 'poppar', 'precision',
        'present', 'product', 'radix', 'random_number', 'random_seed', 'range', 'rank', 'real', 'repeat', 'reshape',
        'rrspacing', 'same_type_as', 'scale', 'scan', 'selected_char_kind', 'selected_int_kind', 'selected_real_kind',
        'set_exponent', 'shape', 'shifta', 'shiftl', 'shiftr', 'sign', 'sin', 'sinh', 'size', 'sngl', 'spacing',
        'spread', 'sqrt', 'storage_size', 'sum', 'system_clock', 'tan', 'tanh', 'this_image', 'tiny', 'trailz',
        'transfer', 'transpose', 'trim', 'ubound', 'ucobound', 'unpack', 'verify']


def isOthreFortranKeywords(candidate):
    return candidate.lower() in ['integer', 'character', 'logical', 'real', 'struct', 'unit', 'false', 'true',
                                 'not', 'and', 'or', 'eq', 'ne', 'lt', 'ge', 'le', 'eqv', 'neqv', 'enddo', 'endif',
                                 'none', 'kind', 'include']


def isFortran77Keyword(candidate):
    return isFortranIntrinsicFucntions(candidate) or isOthreFortranKeywords(candidate) or candidate.lower() in [
        'assign', 'backspace', 'block data', 'call', 'close', 'common', 'continue', 'data', 'dimension', 'do', 'else',
        'else if', 'end', 'endfile', 'endif', 'entry', 'equivalence', 'external', 'format', 'function', 'goto', 'if',
        'implicit', 'inquire', 'intrinsic', 'open', 'parameter', 'pause', 'print', 'program', 'read', 'return',
        'rewind', 'rewrite', 'save', 'stop', 'subroutine', 'then', 'write'
    ]


def isFortran90Keyword(candidate):
    return isFortran77Keyword(candidate) or candidate.lower() in [
        'allocatable', 'allocate', 'case', 'contains', 'cycle', 'deallocate', 'elsewhere', 'exit?, include',
        'interface', 'intent', 'module', 'namelist', 'nullify', 'only', 'operator', 'optional', 'pointer', 'private',
        'procedure', 'public', 'recursive', 'result', 'select', 'sequence', 'target', 'use', 'while', 'where']


def isFortran95Keyword(candidate):
    return isFortran90Keyword(candidate) or candidate.lower() in ['elemental', 'forall', 'pure']


def isFortran2003Keyword(candidate):
    return isFortran95Keyword(candidate) or candidate.lower() in ['abstract', 'associate', 'asynchronous', 'bind',
                                                                  'class', 'deferred', 'enum',
                                                                  'enumerator', 'extends', 'final', 'flush', 'generic',
                                                                  'import', 'non_overridable',
                                                                  'nopass', 'pass', 'protected', 'value', 'volatile',
                                                                  'wait']


def isFortran2008Keyword(candidate):
    return isFortran2003Keyword(candidate) or candidate.lower() in ['block', 'codimension', 'do concurrent',
                                                                    'contiguous', 'critical', 'error stop',
                                                                    'submodule', 'sync all', 'sync images',
                                                                    'sync memory', 'lock', 'unlock']


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
    if calleeLevel > getMaxLevel():
        return
    calleeLevel += 1
    for eachCaller in calle.callers:
        for eachCallee in allCalles:
            if eachCallee.method == eachCaller.method:
                eachCaller.callers = eachCallee.callers
                break
        createCalleeTreeIntenal(eachCaller, allCalles, calleeLevel)


def createCallerTreeIntenal(caller, allCallers, callerLevel):
    if callerLevel > getMaxLevel():
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
    if level > getMaxLevel():
        return
    fileCallee.write(("\t" * level) + callee.method.__str__() + "\n")
    if len(callee.callers) > 0:
        level += 1
        for eachCaller in callee.callers:
            writeCalleesInternal(fileCallee, eachCaller, level)
        level -= 1


def writeCallersInternal(fileCaller, caller, level):
    if level > getMaxLevel():
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


methodsParameter = None
methodsParameter = getMethodsParameter()
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
