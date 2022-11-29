# FortranMakeUtils

Fortran Utility written in python for generating dependency tree and makefiles. Also includes a beautifier for lowercase keywords.

## How to use 

**For generating dependency tree (callers , called (callee) ) and files for using in makefiles (*.mk):**
```
git clone https://github.com/deniseiras/fortranMakeUtils.git
cd fortranMakeUtils
export PYTHONPATH=$(pwd)
```

This will show the parameters needed:
```
src/fortranMakeUtils/fortranMakeUtils.py
```
Result:

*Please enter the parameters:*  

*1 - Initial search directory (mandatory)*  
*2 - Output directory (mandatory)*  
*3 - Max depth for search callers and callees (mandatory)*  
*4 - Filename of the interested routine.*  
*5 - Interested routine to be searched.*  

This will execute on the directory below (used in tests)
In this case do not specify an routine (param 5) of a file (param 4)
```
src/fortranMakeUtils/fortranMakeUtils.py test/data/test_uses_modules/fortran_files/ ./out_dir 20 
```

Result:  

*Using python 3.6*  
*Searching for modules and routines ( subroutines, functions, interfaces ) ...*  
*Searching for uses of modules and callers of routines ...*  
*creating callee tree ...*  
*creating caller tree ...*  
*Creating modules dependencies ...*  
*Creating methods called of all files: file ./out_dir/allMethodsCalled.txt*  
*Creating methods not called of all files: file ./out_dir/allMethodsNotCalled.txt*  
*Creating callee tree files ./out_dir/calleeTree.txt , ./out_dir/calleeTree.dot ...*  
*Creating caller tree file ./out_dir/callerTree.txt ...*  
*Creating methods in Caller Tree: file ./out_dir/methodsInCallerTree.txt*  
*Creating methods not in Caller Tree: file ./out_dir/methodsNotinCallerTree.txt*  
*Creating objects file: ./out_dir/objects.mk ...*  
*Creating dependecies file: ./out_dir/depend.mk ...*  


## Contributions

- Send me an e-mail: denis.eiras@gmail.com
- Use Gitflow style;
- Check opened Issues;


## History:
- 2017       - Fortran Make Utils: Fortran Utility written in python for generating dependency tree and generating makefiles
- 2019/04/17 - Fortran Beautifier: to lowercase most of fortran keywords
- 2022/04/08 - Fortran Make Utils: Bug fix: check inside module
- 2022/11/17 - Fortran Make Utils: Improvement: use just the filename or part of the filename in filtering command line options
- 2022/11/17 - Bug fix: infinite call to caller methods
- 2022/11/25 - Some Unit tests created - not all fixed (new features not implemented yet)
- 2022/11/25 - Created Git Flow branches for development.

