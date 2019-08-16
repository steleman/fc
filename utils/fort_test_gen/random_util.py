import random
import ast
import string
import re

var_name_count = 1

# I am guessing get_rand_XXX return python stuff and get_random_XXX
# returns fc stuff. Refactor maybe ?
def get_rand_int(low, high):
  return random.randint(low,high)


def get_rand_bool():
  rand_int = random.randint(0,1)
  return (rand_int == 1)


def get_rand_from_list(val_list):
  size = len(val_list) - 1
  rand_int = get_rand_int(0, size)
  return val_list[rand_int]


def get_random_constant(type):
    if type == ast.Type.Int32:
      return ast.Constant(str(random.randint(1,10)))

    if type == ast.Type.Int64:
      return  ast.Constant(str(random.randint(1, 32)))

    if type == ast.Type.Real:
        num = round(random.uniform(1, 10.0), 6)
        return ast.Constant(str(num))

    if type == ast.Type.Double:
        num = round(random.uniform(1, 32.0), 6)
        return ast.Constant(str(num))

    if type == ast.Type.Logical:
      rand_int = get_rand_int(0,1)

      if rand_int == 0:
        return ast.Constant(".FALSE.")
      else:
        return ast.Constant(".TRUE.")

    if type == ast.Type.Char:
      return ast.Constant("'" + str(random.choice(string.ascii_letters)) + "'")

    assert False, "unknown type"


def get_random_type():
    return ast.Type(get_rand_int(1, ast.Type.count()))


# returns a random symbols of type \p type from \p symbols
def get_random_symbol(type, symbols):
    candidates = list()
    for sym in symbols:
        if sym.type == type:
            candidates.append(sym)

    if len(candidates) > 0:
        return get_rand_from_list(candidates)

# returns a random symbol of integral type from \p symbols
def get_random_int_symbol(symbols):
    sym = get_random_symbol(ast.Type.Int32, symbols)
    if sym is None:
        sym = get_random_symbol(ast.Type.Int64, symbols)

    return sym

def get_random_symbol_or_const(type, symbols, convert_to_expr = False):
    choice = get_rand_int(1,2)

    sym = get_random_symbol(type, symbols)

    if sym is None:
        return get_random_constant(type)

    if choice == 1:
        if convert_to_expr:
            return ast.ObjectName(sym)
        return sym

    if choice == 2:
        return get_random_constant(type)


def get_random_var_name():
  global var_name_count
  rand_int = get_rand_int(1,10)
  prefix_str = ""
  for x in range(rand_int):
    prefix_str += random.choice(string.ascii_letters)
  prefix_str += str(var_name_count)
  var_name_count += 1
  return prefix_str


def get_random_symbols(l,h):
  symbols = list()

  # calculate num symbols.
  num = get_rand_int(l,h)

  for x in range(num):
    type = get_random_type()
    name = get_random_var_name()
    symbols.append(ast.Symbol(name, type, list()))

  return symbols


def get_random_logical_op():
  rand_int = get_rand_int(1, 2)
  return ast.LogicalOp(rand_int)


def get_random_arith_op():
  rand_int = get_rand_int(1, 5)
  return ast.BinaryOp(rand_int)


def get_random_relational_op():
  rand_int = get_rand_int(1, 6)
  return ast.RelationalOp(rand_int)


def get_random_binary_expr(expr_type, lhs, rhs):
    # FIXME lhs/rhs could be a constant, (expr) etc. Ideally we need an assert
    # here that makes sure that lhs and rhs type conform with the expr_type. We
    # might need a get_type() for all asts that can have type (including expr)
    if expr_type == ast.ExprType.Arith:
        op_type = get_random_arith_op()
        return ast.BinaryExpr(lhs, op_type, rhs)

    if expr_type == ast.ExprType.Logical:
        op_type = get_random_logical_op()
    return ast.LogicalExpr(lhs,op_type,rhs)

    if expr_type == ast.ExprType.Relational:
        op_type = get_random_relational_op()
    return ast.RelationalExpr(lhs,op_type,rhs)

    assert False, "unknown expr type"

# returns true if expr overflows for resultant_type
def does_expr_overflow(resultant_type, expr):
    if (not resultant_type.is_numeric()):
        return False

    # TODO: We are substituting variables with a '2', ideally we need to
    # substitute with it value at that point, gfortran doesn't check to that
    # extent actually. But gfortran does catch <const/var> ** <const> that can
    # potentially lead to an overflow. This substiution is a temporary fix for
    # that.
    str_expr = re.sub(r"[a-zA-Z_][a-zA-Z0-9_]+", "2", str(expr))
    if (eval(str_expr) > resultant_type.get_max()):
        return True
    return False


# if expr can possibly overflow for resultant_type, than return an expr that can't
def get_no_overflow(resultant_type, expr):
    if (not resultant_type.is_numeric()):
        return expr

    if (does_expr_overflow(resultant_type, expr)):
        return ast.Constant("1")
    return expr

# Recursive function to generate random expression.
def get_random_expr(resultant_type, curr_symbols, depth):
    if depth == 1:
        return get_random_symbol_or_const(resultant_type, curr_symbols, True)

    lhs = get_random_expr(resultant_type, curr_symbols, depth - 1)

    val = not get_rand_bool()
    if val:
        return lhs

    rhs = get_random_expr(resultant_type, curr_symbols, depth - 1)

    lhs = get_no_overflow(resultant_type, lhs)
    rhs = get_no_overflow(resultant_type, rhs)

    if resultant_type.is_numeric():
        return get_random_binary_expr(ast.ExprType.Arith, lhs, rhs)
    elif resultant_type.is_logical():
        return get_random_binary_expr(ast.ExprType.Logical, lhs, rhs)
    else:
        assert False, "unhandled resultant type"
