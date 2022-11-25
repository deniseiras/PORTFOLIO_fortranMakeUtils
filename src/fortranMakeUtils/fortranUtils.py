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
        objectName = self.getOnlyFileName().replace(".f90", ".o").replace(".F90", ".o").replace(".F", ".o")
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
            return ((self.name == other.name) and (self.module.name == other.module.name) and (self.methodType == other.methodType))
            # return self.__repr__() == other.__repr__()
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


def getFiles(initialDir):
    files = []
    for root, dirs, filez in os.walk(initialDir):
        for filename in filez:
            if filename.endswith('.f90') or filename.endswith('.F90') or filename.endswith('.F'):
                files.append(root + "/" + filename)
    return files


def isRoutine(candidate):
    return candidate in ["subroutine", "function"]


def isFunction(candidate):
    return candidate == "function"


def isDependency(candidate):
    return candidate in ["subroutine", "function", "interface"]


def isEndString(candidate):
    return candidate == "end"


def isComment(candidate):
    return candidate[0] == "!"


def isInterfaceRoutine(candidate):
    return candidate == "procedure"


def isModule(candidate):
    return candidate == "module"

def isRecursive(candidate):
    return candidate == "recursive"

def isUsedModule(candidade):
    return candidade == "use"


def isFortranIntrinsicFucntions(candidate):
    return candidate in [
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
    return candidate in ['in', 'inout', 'file', 'form', 'iostat', 'status', 'action', 'integer', 'character',
                                 'logical', 'real', 'struct', 'unit', 'false', 'true', 'not', 'and', 'or', 'eq', 'ne',
                                 'lt', 'ge', 'le', 'eqv', 'neqv', 'enddo', 'endif', 'none', 'kind', 'include']


def isFortran77Keyword(candidate):
    return isFortranIntrinsicFucntions(candidate) or isOthreFortranKeywords(candidate) or candidate in [
        'assign', 'backspace', 'block data', 'call', 'close', 'common', 'continue', 'data', 'dimension', 'do', 'else',
        'else if', 'end', 'endfile', 'endif', 'entry', 'equivalence', 'external', 'format', 'function', 'goto', 'if',
        'implicit', 'inquire', 'intrinsic', 'open', 'parameter', 'pause', 'print', 'program', 'read', 'return',
        'rewind', 'rewrite', 'save', 'stop', 'subroutine', 'then', 'write'
    ]


def isFortran90Keyword(candidate):
    return isFortran77Keyword(candidate) or candidate in [
        'allocatable', 'allocate', 'case', 'contains', 'cycle', 'deallocate', 'elsewhere', 'exit?, include',
        'interface', 'intent', 'module', 'namelist', 'nullify', 'only', 'operator', 'optional', 'pointer', 'private',
        'procedure', 'public', 'recursive', 'result', 'select', 'sequence', 'target', 'use', 'while', 'where']


def isFortran95Keyword(candidate):
    return isFortran90Keyword(candidate) or candidate in ['elemental', 'forall', 'pure']


def isFortran2003Keyword(candidate):
    return isFortran95Keyword(candidate) or candidate in ['abstract', 'associate', 'asynchronous', 'bind',
                                                                  'class', 'deferred', 'enum',
                                                                  'enumerator', 'extends', 'final', 'flush', 'generic',
                                                                  'import', 'non_overridable',
                                                                  'nopass', 'pass', 'protected', 'value', 'volatile',
                                                                  'wait']


def isFortran2008Keyword(candidate):
    return isFortran2003Keyword(candidate) or candidate in ['block', 'codimension', 'do concurrent',
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


def createModulesAndDependentsOfDependents_internal(module, modulesUsedOrCalled, modulesDependents, max_level, count):
    for eachDep in module.dependsOn:
        count += 1
        if count > max_level: 
            break 
        createModulesAndDependentsOfDependents_internal(eachDep, modulesUsedOrCalled, modulesDependents, max_level, count)


def createModulesAndDependentsOfDependents(module, modulesUsedOrCalled, modulesDependents, max_level):
    # if module not in modulesUsedOrCalled:
    count = 0
    createModulesAndDependentsOfDependents_internal(module, modulesUsedOrCalled, modulesDependents, max_level, count)

    modDependent = [modDep for modDep in modulesDependents if modDep == module]
    if len(modDependent) > 0:
        modDependent = modDependent[0]
        module.dependsOn = module.dependsOn.union(modDependent.dependsOn)
        modulesUsedOrCalled.add(module)


def writeObjectDependeciesFile(modules, out_dir):
    fileDepsName = "{}/depend.mk".format(out_dir)
    print("Creating dependecies file: {0} ...".format(fileDepsName))
    fileDeps = open(fileDepsName, 'w')
    for eachModule in modules:
        if len(eachModule.dependsOn) > 0:
            fileDeps.write(eachModule.getObjectName() + ":")
            for eachDep in eachModule.dependsOn:
                fileDeps.write(" " + eachDep.getObjectName())
            fileDeps.write("\n")
    fileDeps.close()

def writeObjectsFile(modules, out_dir):
    fileObjsName = "{}/objects.mk".format(out_dir)
    print("Creating objects file: {0} ...".format(fileObjsName))
    fileObjs = open(fileObjsName, 'w')
    fileObjs.write("SRCS =")
    for eachModule in modules:
        fileObjs.write(" " + eachModule.getOnlyFileName())
    fileObjs.write("\n\nOBJS =")
    for eachModule in modules:
        fileObjs.write(" " + eachModule.getObjectName())
    fileObjs.close()


def createCalleeTree(allCallees, max_level):
    print("creating callee tree ...")
    calleeLevel = 0
    for callee in allCallees:
        if len(callee.callers) > 0:
            createCalleeTreeIntenal(callee, allCallees, calleeLevel, max_level)


# BUG B3
def createCalleeTreeIntenal(callee, allCalles, calleeLevel, max_level):
    if calleeLevel > max_level:
        return
    calleeLevel += 1
    for eachCaller in callee.callers:
        if callee.method == eachCaller.method:  # recursive
            continue
        for eachCallee in allCalles:
            if eachCallee.method == eachCaller.method:
                eachCaller.callers = eachCallee.callers
                break
        createCalleeTreeIntenal(eachCaller, allCalles, calleeLevel, max_level)


def createCallerTree(allCallers, max_level):
    print("creating caller tree ...")
    callerLevel = 0
    for caller in allCallers:
        if len(caller.callees) > 0:
            createCallerTreeIntenal(caller, allCallers, callerLevel, max_level)


def createCallerTreeIntenal(caller, allCallers, callerLevel, max_level):
    if callerLevel > max_level:
        return
    callerLevel += 1
    for eachCallee in caller.callees:
        if caller.method == eachCallee.method:  # recursive
            continue
        for eachCaller in allCallers:
            if eachCaller.method == eachCallee.method:
                eachCallee.callees = eachCaller.callees
                break
        createCallerTreeIntenal(eachCallee, allCallers, callerLevel, max_level)


def writeCallees(allCallees, out_dir, max_level):
    fileCalleeName = "{}/calleeTree.txt".format(out_dir)
    fileCalleeDotName = "{}/calleeTree.dot".format(out_dir)
    print(f'Creating callee tree files {fileCalleeName} , {fileCalleeDotName} ...')
    fileCallee = open(fileCalleeName, 'w')
    fileCalleeDot = open(fileCalleeDotName, 'w')
    level = 0
    fileCalleeDot.write('digraph G {\n')
    set_callee_dot_strings = set()
    for callee in sorted(allCallees, key=lambda c: c.method.__str__()):
        writeCalleesInternal(fileCallee, fileCalleeDot, callee, level, max_level, set_callee_dot_strings)
    for each_str in sorted(set_callee_dot_strings):
        fileCalleeDot.write(each_str)
    fileCalleeDot.write('}')
    fileCallee.close()
    fileCalleeDot.close()


def writeCalleesInternal(fileCallee, fileCalleeDot, callee, level, max_level, set_callee_dot_strings):
    if level > max_level:
        return
    fileCallee.write(("\t" * level) + callee.method.__str__() + "\n")
    if len(callee.callers) > 0:
        level += 1
        for eachCaller in sorted(callee.callers, key=lambda c: c.method.__str__()):
            set_callee_dot_strings.add(f'    {callee.method.name} -> {eachCaller.method.name}; \n')
            writeCalleesInternal(fileCallee, fileCalleeDot, eachCaller, level, max_level, set_callee_dot_strings)
        level -= 1


def writeCallers(allCallers, methodsInCallerTree, out_dir, max_level):
    fileCallerName = "{}/callerTree.txt".format(out_dir)
    fileCallerDotName = "{}/callerTree.dot".format(out_dir)
    print("Creating caller tree file " + fileCallerName + " ...")
    fileCaller = open(fileCallerName, 'w')
    fileCallerDot = open(fileCallerDotName, 'w')
    level = 0
    fileCallerDot.write('digraph G {\n')
    set_caller_dot_strings = set()
    for caller in sorted(allCallers, key=lambda c: c.method.__str__()):
        writeCallersInternal(fileCaller, fileCallerDot, caller, level, methodsInCallerTree, max_level, set_caller_dot_strings)
    for each_str in sorted(set_caller_dot_strings):
        fileCallerDot.write(each_str)
    fileCallerDot.write('}')
    fileCaller.close()
    fileCallerDot.close()


def writeCallersInternal(fileCaller, fileCallerDot, caller, level, methodsInCallerTree, max_level, set_caller_dot_strings):
    if level > max_level:
        return
    fileCaller.write(("\t" * level) + caller.method.__str__() + "\n")
    methodsInCallerTree.add(caller.method)
    if len(caller.callees) > 0:
        level += 1
        for eachCallee in sorted(caller.callees, key=lambda c: c.method.__str__()):
            set_caller_dot_strings.add(f'    {caller.method.name} -> {eachCallee.method.name}; \n')
            writeCallersInternal(fileCaller, fileCallerDot, eachCallee, level, methodsInCallerTree, max_level, set_caller_dot_strings)
        level -= 1


def writeCalledFile(methodsCalled, out_dir):
    fName = "{}/allMethodsCalled.txt".format(out_dir)
    print("Creating methods called of all files: file " + fName)
    fileCalled = open(fName, 'w')
    for key in sorted(methodsCalled.keys(), key=lambda m: m.__str__()):
        fileCalled.write(key.__str__() + " = " + str(methodsCalled[key]) + "\n")


def writeNotCalledFile(methodsNotCalled, out_dir):
    fName = "{}/allMethodsNotCalled.txt".format(out_dir)
    print("Creating methods not called of all files: file " + fName)
    fileNotCalled = open(fName, 'w')
    for key in sorted(methodsNotCalled, key=lambda m: m.__str__()):    
        fileNotCalled.write(key.__str__() + "\n")
    fileNotCalled.close()


def writeMethodsInCallerTreeFile(methodsInCallerTree, out_dir):
    fName = "{}/methodsInCallerTree.txt".format(out_dir)
    print("Creating methods in Caller Tree: file " + fName)
    fileInCallerTree = open(fName, 'w')
    for method in sorted(methodsInCallerTree, key=lambda m: m.__str__()):
        fileInCallerTree.write(method.__str__() + "\n")
    fileInCallerTree.close()


def writeMethodsNotInCallerTreeFile(methodsNotInCallerTree, out_dir):
    fName = "{}/methodsNotinCallerTree.txt".format(out_dir)
    print("Creating methods not in Caller Tree: file " + fName)
    fileNotInCallerTree = open(fName, 'w')
    for method in sorted(methodsNotInCallerTree, key=lambda m: m.__str__()):
        fileNotInCallerTree.write(method.__str__() + "\n")
    fileNotInCallerTree.close()
