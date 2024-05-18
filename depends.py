#!/usr/bin/env -S python3 -Werror
# -*- coding: utf-8 -*-

import argparse

class dependency_structure:
    def __init__(self, program, module, dependencies):
        self.program      = program
        self.module       = module
        self.dependencies = dependencies

def get_module_name_from_use(line):
    start_index = line.find("use") + 4
    end_index   = line.find(",")
    
    return line[start_index:end_index]

parser = argparse.ArgumentParser(description="Generates a Makefile fragment describing the dependencies of a Fortran project.")
parser.add_argument('file', nargs='+')
args = parser.parse_args()

depstructs = []
for filename in args.file:
    with open(filename, "r") as file_handler:
        program      = False
        module       = False
        dependencies = set()
        for line in file_handler.readlines():
            if line.strip().startswith("program "):
                program = True
            
            if line.strip().startswith("module "):
                module = True
            
            if line.strip().startswith("use "):
                dependencies.add(get_module_name_from_use(line))
        
        print("{} program={} module={} dependencies={}".format(filename, program, module, dependencies))
        
        if (program and module):
            print("{} contains both a program and a module. depends.py assumes that a file contains one or the other, not both.".format(filename))
            exit(1)
        
        depstructs.append(dependency_structure(program, module, dependencies))

for depstruct in depstructs:
    if depstruct.program:
        print(depstruct.dependencies)

# TODO: Determine whether a program or module and treat differently depending on that.

# TODO: Return error if program contains both module and program.
