from enum import Enum

class Type(Enum):
    Int32 = 1
    Int64 = 2
    Real = 3
    Double = 4
    Char = 5
    Logical = 6

    # FIXME: adding more types will change count() which random type generator
    # depends on. We need to eliminate this dependency. Also, we need to make sure
    # that adding a new type appends to the existing enum-list since there is a
    # dependency on the literal numbers for the APIs below.
    @staticmethod
    def count():
        return 6

    def is_integral(self):
        if self.value == 1 or self.value == 2:
            return True
        return False

    def is_floating(self):
        if self.value == 3 or self.value == 4:
            return True
        return False

    def is_numeric(self):
        if self.is_integral() or self.is_floating():
            return True
        return False

    def is_logical(self):
        if self.value == 6:
            return True
        return False

    def is_char(self):
        if self.value == 5:
            return True
        return False

    def get_max(self):
        if self.value == 1:
            return 2147483647
        if self.value == 2:
            return 2147483647
        if self.value == 3:
            return 1.7E+38
        if self.value == 4:
            return 1.7E+38
        return 1

    def __str__(self):
        if self.value == 1:
            return "integer"
        if self.value == 2:
            return "integer"
        if self.value == 3:
            return "real"
        if self.value == 4:
            return "real"
        if self.value == 5:
            return "character"
        if self.value == 6:
            return "logical"
        assert False, "unknown type!"


class BinaryOp(Enum):
    Plus =1
    Minus = 2
    Mul = 3
    Div = 4
    Pow = 5

    def __str__(self):
        if self.value == 1:
            return "+"
        if self.value == 2:
            return "-"
        if self.value == 3:
            return "*"
        if self.value == 4:
            return "/"
        if self.value == 5:
            return "**"
        assert False, "unknown type!"


class LogicalOp(Enum):
    And =1
    OR = 2

    def __str__(self):
        if self.value == 1:
            return ".and."
        if self.value == 2:
            return ".or."

        assert False, "unknown type!"


class RelationalOp(Enum):
    EQ =1
    NE= 2
    LT = 3
    LE = 4
    GT = 5
    GE = 6

    def __str__(self):
        if self.value == 1:
            return "=="
        if self.value == 2:
            return "/="
        if self.value == 3:
            return "<"
        if self.value == 4:
            return "<="
        if self.value == 5:
            return ">"
        if self.value == 6:
            return ">="

        assert False, "unknown type!"


class ExprType(Enum):
    Arith = 1
    Logical = 2
    Relational = 3


class Symbol:
    def __init__(self, name, base_type, dims):
        self.name = name
        self.type = base_type
        self.dims = dims

    def get_type(self):
      return self.type

    def __str__(self):
        assert len(self.dims) == 0,"arrays not handled yet"
        return str(self.type) + " :: " + self.name + "\n"

class SymbolTable:
    def __init__(self):
        self.symbol_map = dict()

    def add_symbol(self, symbol):

        if symbol.name in self.symbol_map:
                assert False, "symbol already defined"

        self.symbol_map[symbol.name] = symbol;

    def add_sym_list(self,sym_list):
        for sym in sym_list:
            self.add_symbol(sym)

    def get_as_list(self):
        sym_list = list()
        for sym in self.symbol_map.values():
            sym_list.append(sym)
        return sym_list

    def __str__(self):
        if len(self.symbol_map) == 0:
            return ""
        sym_str = ""
        for sym in self.symbol_map.values():
            sym_str += str(sym)
        return sym_str


class Expr:
    def __init__(self):
        return


class Constant(Expr):
    def __init__(self, value):
        self.value = value

    def __str__(self):
        return str(self.value)


class ObjectName(Expr):
    def __init__(self,symbol):
        self.var = symbol

    def __str__(self):
        return self.var.name


class Array(Expr):
    def __init__(self, var, subs_list):
        self.var = var
        self.subsList = subs_list


class BinaryExpr(Expr):
    def __init__(self, lhs, op, rhs):
        self.lhs = lhs
        self.op = op
        self.rhs = rhs

    def __str__(self):
        return "(" + str(self.lhs) + " " + str(self.op)+ " " + str(self.rhs) + ")"


class LogicalExpr(Expr):
    def __init__(self, lhs, op, rhs):
        self.lhs = lhs
        self.op = op
        self.rhs = rhs

    def __str__(self):
        return "(" + str(self.lhs) + " " + str(self.op)+ " " + str(self.rhs) + ")"


class RelationalExpr(Expr):
    def __init__(self, lhs, op, rhs):
        self.lhs = lhs
        self.op = op
        self.rhs = rhs

    def __str__(self):
        return "(" + str(self.lhs) + " " + str(self.op)+ " " + str(self.rhs) + ")"

class Stmt:
    def __init__(self):
        return


class PrintStmt(Stmt):
    def __init__(self,args_list):
        self.args_list = args_list

    def __str__(self):
        print_str = "PRINT *"
        for arg in self.args_list:
            print_str += ", " + str(arg) + "\n"
        return print_str


class Assignment(Stmt):
    def __init__(self,lhs,rhs):
        self.lhs = lhs
        self.rhs = rhs

    def __str__(self):
        return str(self.lhs) +  " = " + str(self.rhs) + "\n"

class IfStmt(Stmt):
    def __init__(self, cond):
        self.cond = cond

    def __str__(self):
        return "if (" + str(self.cond) + ") then\n"

class DoStmt(Stmt):
    def __init__(self, indvar, lb, ub, step):
        self.indvar = indvar
        self.lb = lb
        self.ub = ub
        self.step = step

    def __str__(self):
        return "do " + str(self.indvar) + " = " + \
                str(self.lb) + ", " + str(self.ub) + "\n"

class Execution:
    def __init__(self):
        self.stmt_list = list()

    def __str__(self):
        exec_str = ""

        for stmt in self.stmt_list:
            exec_str += str(stmt)
        return exec_str

    def add_stmt(self,stmt):
        self.stmt_list.append(stmt)


class Program:
    def __init__(self, name):
        self.name = name
        self.symbol_table = SymbolTable()
        self.exec = Execution()

    def add_sym_table(self, table):
        self.symbol_table = table

    def add_symbol(self, symbol):
        self.symbol_table.add_symbol(symbol)

    def add_stmt(self,stmt):
        self.exec.add_stmt(stmt)

    def __str__(self):
        test_case = "program " + self.name + "\n"
        test_case += str(self.symbol_table)
        test_case += str(self.exec)
        test_case += "end program " + self.name + "\n"
        return test_case
