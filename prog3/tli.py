# programming assignment 3 by Strother Woog, 1618221 @ Univeristy of California, Santa Cruz
#! /usr/bin/env python3
import fileinput
import sys
import shlex

# used to store a parsed TL expressions which are
# constant numbers, constant strings, variable names, and binary expressions
class Expr :
    def __init__(self,op1,operator,op2=None):
        self.op1 = op1
        self.operator = operator
        self.op2 = op2

    # likely unsused
    def __str__(self):
        if self.op2 == None:
            return self.operator + " " + self.op1
        else:
            return self.op1 + " " + self.operator + " " +  self.op2

    # evaluate this expression given the environment of the symTable
    def eval(self, symTable):
        if self.operator == "var":
            if self.op1[0] == '"':
                return self.op1[1:-1]
            elif self.op1.isdigit():
                return float(self.op1)
            elif "." in self.op1:
                return float(self.op1)
            else: # potential error placement for unknown variable
                return "-1" 
        elif self.operator == '+':
            return float((Expr(self.op1,"var" , None)).eval(symTable)) + float((Expr(self.op2,"var" , None)).eval(symTable))
        elif self.operator == '-':
            return float((Expr(self.op1,"var" , None)).eval(symTable)) - float((Expr(self.op2,"var" , None)).eval(symTable))
        elif self.operator == '*':
            return float((Expr(self.op1,"var" , None)).eval(symTable)) * float((Expr(self.op2,"var" , None)).eval(symTable))
        elif self.operator == '/':
            return float((Expr(self.op1,"var" , None)).eval(symTable)) / float((Expr(self.op2,"var" , None)).eval(symTable))
        elif self.operator == '<':
            if float((Expr(self.op1,"var" , None)).eval(symTable)) < float((Expr(self.op2,"var" , None)).eval(symTable)):
                return 1
            else:
                return 0
        elif self.operator == '>':
            if float((Expr(self.op1,"var" , None)).eval(symTable)) > float((Expr(self.op2,"var" , None)).eval(symTable)):
                return 1
            else:
                return 0
        elif self.operator == "<=":
            if float((Expr(self.op1,"var" , None)).eval(symTable)) <= float((Expr(self.op2,"var" , None)).eval(symTable)):
                return 1
            else:
                return 0
        elif self.operator == ">=":
            if float((Expr(self.op1,"var" , None)).eval(symTable)) >= float((Expr(self.op2,"var" , None)).eval(symTable)):
                return 1
            else:
                return 0
        elif self.operator == "!=":
            if float((Expr(self.op1,"var" , None)).eval(symTable)) != float((Expr(self.op2,"var" , None)).eval(symTable)):
                return 1
            else:
                return 0
        elif self.operator == "==":
            if float((Expr(self.op1,"var" , None)).eval(symTable)) == float((Expr(self.op2,"var" , None)).eval(symTable)):
                return 1
            else:
                return 0

# used to store a parsed TL statement
class Stmt :
    def __init__(self,keyword,exprs):
        self.keyword = keyword
        self.exprs = exprs

    # likely unused
    def __str__(self):
        others = ""
        for exp in self.exprs:
            others = others + " " + str(exp)
        return self.keyword + others

    # perform/execute this statement given the environment of the symTable
    def perform(self, symTable):
        if self.keyword == "print":
            printLine = ""
            exs = []
            ex = []

            # seperate expression if multiple expression
            for e in self.exprs:
                if e != ',':
                    ex.append(e)
                elif e == ',':
                    exs.append(ex)
                    ex = []
            exs.append(ex) 

            # evaluate expressions and print on same line
            for p in exs:
                expr = None
                if len(p) == 1:
                
                    # check if variable is in symTable
                    if any([sym[0] == p[0] for sym in symTable]):
                        for sym in symTable:
                            if sym[0] == p[0]:
                                if isinstance(sym[1] , str):
                                    expr = Expr('"' + str(sym[1]) + '"', "var", None)
                                else:
                                    expr = Expr(str(sym[1]), "var", None)
                    else:
                        expr = Expr(p[0], "var", None)

                    # check if variable name does not exist
                    if expr.eval(symTable) == "-1":
                        print ("Undefined variable " + str(p[0]), end="")
                        return (-3, symTable)
                    
                elif len(p) == 3:
                    val1 = p[0]
                    val2 = p[2]

                    # check if variables are in symTable
                    if any([sym[0] == val1 for sym in symTable]):
                        val1 = list(filter(lambda l: l[0] == val1, symTable))[0][1]
                    if any([sym[0] == val2 for sym in symTable]):
                        val2 = list(filter(lambda l: l[0] == val2, symTable))[0][1]
                    
                    # check if variable names do not exist
                    if (Expr(str(val1), "var", None)).eval(symTable) == "-1":
                        print ("Undefined variable " + str(val1), end="")
                        return (-3, symTable)
                    elif (Expr(str(val2), "var", None)).eval(symTable) == "-1":
                        print ("Undefined variable " + str(val2), end="")
                        return (-3, symTable)

                    expr = Expr(str(val1), p[1], str(val2))

                printLine += (str(expr.eval(symTable)) + " ")

            print (printLine)
            return (-1, symTable)

        elif self.keyword == "let":
            if len(self.exprs) > 3:
                val1 = self.exprs[2]
                val2 = self.exprs[4]

                # check if variables are in symTable
                if any([sym[0] == val1 for sym in symTable]):
                    val1 = list(filter(lambda l: l[0] == val1, symTable))[0][1]
                if any([sym[0] == val2 for sym in symTable]):
                    val2 = list(filter(lambda l: l[0] == val2, symTable))[0][1]

                # check if variable names do not exist
                if (Expr(str(val1), "var", None)).eval(symTable) == "-1":
                        print ("Undefined variable " + str(val1), end="")
                        return (-3, symTable)
                elif (Expr(str(val2), "var", None)).eval(symTable) == "-1":
                    print ("Undefined variable " + str(val2), end="")
                    return (-3, symTable)

                e = Expr(str(val1), self.exprs[3], str(val2))

                # replace value if variable already in symTable 
                if any([sym[0] == self.exprs[0] for sym in symTable]):
                    symTable = [(var,val) if (var != self.exprs[0]) else (var, e.eval(symTable)) for (var, val) in symTable]
                # add new variable otherwise
                else:
                    symTable.append((self.exprs[0], e.eval(symTable)))
            elif len(self.exprs) == 3:
                val1 = self.exprs[2]
                
                # check if variable is in symTable
                if any([sym[0] == val1 for sym in symTable]):
                    val1 = list(filter(lambda l: l[0] == val1, symTable))[0][1]

                # check if variable name does not exist
                if (Expr(str(val1), "var", None)).eval(symTable) == "-1":
                        print ("Undefined variable " + str(val1), end="")
                        return (-3, symTable)

                e = Expr(str(val1), "var", None)

                # replace value if variable already in symTable
                if any([sym[0] == self.exprs[0] for sym in symTable]):
                    symTable = [(var,val) if (var != self.exprs[0]) else (var, e.eval(symTable)) for (var, val) in symTable]
                # add new variable otherwise
                else:
                    symTable.append((self.exprs[0], e.eval(symTable)))

            return (-1, symTable)

        elif self.keyword == "input":
            grab = input()
            inp = shlex.split(grab, posix=False)

            if len(inp) == 1:
                e = Expr(str(inp[0]), "var", None)

                # replace value if variable already in symTable 
                if any([sym[0] == self.exprs[0] for sym in symTable]):
                    symTable = [(var,val) if (var != self.exprs[0]) else (var, e.eval(symTable)) for (var, val) in symTable]
                # add new variable otherwise
                else:
                    symTable.append((self.exprs[0], e.eval(symTable)))
            elif len(inp) == 3:
                e = Expr(inp[0], inp[1], inp[2])
                symTable.append((self.exprs[0], e.eval(symTable)))
            else:
                print ("Illegal or missing input")
                exit(0)

            return (-1, symTable)

        elif self.keyword == "if":
            condExpr = self.exprs[0:-2]

            val1 = condExpr[0]
            val2 = condExpr[2]

            # check if variables are in symTable
            if any([sym[0] == val1 for sym in symTable]):
                val1 = list(filter(lambda l: l[0] == val1, symTable))[0][1]
            if any([sym[0] == val2 for sym in symTable]):
                val2 = list(filter(lambda l: l[0] == val2, symTable))[0][1]

            e = Expr(str(val1), condExpr[1], str(val2))
            label = self.exprs[-1:][0]
            jump = -1

            if e.eval(symTable) == 1:
                if any([sym[0] == label for sym in symTable]):
                    jump = list(filter(lambda l: l[0] == label, symTable))[0][1]
                else:
                    jump = -2

            return (jump, symTable)

def main():
    symTable = []

    # grab input file from command line
    fileName = sys.argv[1]    

    # parse lines and split by whitespace
    lines = [line.rstrip('\n') and shlex.split(line, posix=False)  for line in open(fileName)]

    # remove blank lines
    noBlanks = list(filter(lambda l: l != '' and l != [], lines))

    # parse labels and line in symbol table
    for i in range(0, len(noBlanks)):
        if len(noBlanks[i][0]) > 2 and noBlanks[i][0][-1:] == ':':
            symTable.append((noBlanks[i][0][:-1], i))

    # parse lines into a statement list and check for syntax errors
    i = 1
    stList = []
    for st in noBlanks:
        stType = st[0]

        if stType[-1:] == ':':
            stList.append(Stmt(st[1], st[2:]))

        elif (stType == "let") and (len(st) == 4 or len(st) == 6):
            if (len(st) == 4) and (st[2] == "="):
                stList.append(Stmt(stType, st[1:]))
            elif (len(st) == 6) and (st[2] == "=") and (st[4] == "+" or st[4] == "-" or st[4] == "*" or st[4] == "/" or st[4] == "<" or st[4] == ">" or st[4] == "<=" or st[4] == ">=" or st[4] == "==" or st[4] == "!="):
                stList.append(Stmt(stType, st[1:]))
            else:
                print("Syntax error on line " + str(i) + ".")
                exit(0) 

        elif (stType == "if") and (len(st) == 6):
            if (st[4] == "goto"):
                stList.append(Stmt(stType, st[1:]))
            else:
                print("Syntax error on line " + str(i) + ".")
                exit(0)

        elif (stType == "print"):
            stList.append(Stmt(stType, st[1:]))

        elif (stType == "input") and (len(st) == 2):
            stList.append(Stmt(stType, st[1:]))

        else:
            print("Syntax error on line " + str(i) + ".")
            exit(0)
        i += 1

    # perform the program by iterating through list of statements and perform runtime errors
    i = 0
    while i != len(stList):
        currST = stList[i]
        performTup = currST.perform(symTable)
        symTable = performTup[1]
        # check index for goto label
        if performTup[0] > -1:
            i = performTup[0] - 1
        # continue to next line in program
        elif performTup[0] == -1:
            pass
        # runtime error checking
        elif performTup[0] == -2:
            print ("Illegal goto " + str(noBlanks[i][-1]) + " at line " + str(i+1) + ".")
            exit(0)
        elif performTup[0] == -3:
            print (" at line " + str(i+1) + ".")
            exit(0)
        else:
            print ('Generic runtime error for testing.')
            exit(0)
        i += 1

# run tiny language interpreter
main()