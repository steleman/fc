from ast import *
import random_util
import extra_util
import sys

curr_prog_num = 1
curr_symbols = list()

# TODO: group all these global "tunable" parameters into a namespace/class
num_tests = 10
min_symbols = 5
max_symbols = 15

min_exps = 1
max_exps = 3

max_nest_depth = 3 # for if-nest and do-nest
max_expr_depth = 3


# Generate initial assignments for every symbol
def gen_assignments(curr_prog):
  for sym in curr_symbols:
    lhs = ObjectName(sym)
    rand_val = random_util.get_random_constant(sym.get_type())
    stmt = Assignment(lhs, rand_val)
    curr_prog.add_stmt(stmt)

def gen_prints(curr_prog):
  for sym in curr_symbols:
    lhs = ObjectName(sym)
    empty_list = list()
    empty_list.append(lhs)
    stmt = PrintStmt(empty_list)
    curr_prog.add_stmt(stmt)

def gen_expressions(curr_prog, curr_symbols):
  rand_int = random_util.get_rand_int(min_exps, max_exps)

  for i in range(rand_int):
    depth = random_util.get_rand_int(2, max_expr_depth)
    lhs = random_util.get_rand_from_list(curr_symbols)
    if (lhs.get_type().is_char()):
        continue
    expr = random_util.get_random_expr(lhs.get_type(), curr_symbols, depth)
    lhs = ObjectName(lhs)
    stmt = Assignment(lhs,expr)
    curr_prog.add_stmt(stmt)

def gen_action_stmts(curr_prog, _curr_symbols, depth, skip_symbols = list()):
    curr_symbols = list(_curr_symbols) # copy curr_symbols
    if depth == 0:
        return

    for i in range(random_util.get_rand_int(1, 3)):
        decision = random_util.get_rand_int(1, 3)

        # generate if-block recursively.
        if (decision == 1):
            if_stmt = IfStmt(random_util.get_random_expr(Type.Logical, curr_symbols,
                3))
            curr_prog.add_stmt(if_stmt)
            gen_action_stmts(curr_prog, curr_symbols, depth - 1)
            if random_util.get_rand_int(0, 1) == 0:
                curr_prog.add_stmt("else\n")
                gen_action_stmts(curr_prog, curr_symbols, depth)

            curr_prog.add_stmt("end if\n")

        # generate do-block revursively
        elif (decision == 2):
            curr_symbols = extra_util.diff_list(curr_symbols, skip_symbols)
            indvar = random_util.get_random_int_symbol(curr_symbols)
            if indvar is None:
                return
            do_stmt = DoStmt(ObjectName(indvar), 0, random_util.get_rand_int(10, 100),
                    random_util.get_rand_int(1, 5))
            curr_prog.add_stmt(do_stmt)

            # add to skip_symbols so that indvar doesn't get redefined by an inner expression or by
            # an inner do-loop
            skip_symbols.append(indvar)

            gen_action_stmts(curr_prog, curr_symbols, depth - 1, skip_symbols)
            curr_prog.add_stmt("end do\n")

        elif (decision == 3):
            curr_symbols = extra_util.diff_list(curr_symbols, skip_symbols)
            gen_expressions(curr_prog, curr_symbols)


def gen_program(prog_name):
    global curr_symbols

    curr_prog = Program(prog_name)

    curr_symbols = random_util.get_random_symbols(min_symbols, max_symbols)
    # Update the current symbol table.
    sym_table = SymbolTable()
    sym_table.add_sym_list(curr_symbols)
    curr_prog.add_sym_table(sym_table)

    # simple values to start with.
    gen_assignments(curr_prog)

    gen_action_stmts(curr_prog, curr_symbols, max_nest_depth)

    # generate prints now
    gen_prints(curr_prog)

    return curr_prog


prog_name = sys.argv[1]
test_dir = sys.argv[2]

curr_prog = gen_program(prog_name)
out_filename = test_dir + "/" + curr_prog.name + ".f90"
with open(out_filename, "w") as out_file:
    out_file.write(str(curr_prog))
