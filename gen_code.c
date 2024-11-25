// Christian Morton, Michael John, Ryan Rohan, Daniel Landsman
// gen_code.c: code generation file, includes function bodies

#include <string.h>
#include <limits.h>
#include "gen_code.h"
#include "code.h"
#include "code_utils.h"
#include "literal_table.h"
#include "id_use.h"
#include "utilities.h"
#include "regname.h"
#include "ast.h"

#define STACK_SPACE 4096

// Initialize the code generator
void gen_code_initialize()
{
    literal_table_initialize();
}

// Write all instructions in code_seq `cs` to binary output file `bf`
static void gen_code_output_seq(BOFFILE bf, code_seq cs)
{
    while (!code_seq_is_empty(cs))
    {
        bin_instr_t instr = (code_seq_first(cs))->instr;
        instruction_write_bin_instr(bf, instr);
        cs = code_seq_rest(cs);
    }
}

// Generate a BOF header for the program
static BOFHeader gen_code_program_header(code_seq main_cs)
{
    BOFHeader header;
    bof_write_magic_to_header(&header);
    header.text_start_address = 0;
    header.text_length = code_seq_size(main_cs) * BYTES_PER_WORD;

    int data_start = MAX(header.text_length, 1024) + BYTES_PER_WORD;
    header.data_start_address = data_start;
    header.data_length = literal_table_size() * BYTES_PER_WORD;

    int stack_bottom = data_start + header.data_length + STACK_SPACE;
    header.stack_bottom_addr = stack_bottom;

    return header;
}

// Output literals to the binary file
static void gen_code_output_literals(BOFFILE bf)
{
    literal_table_start_iteration();
    while (literal_table_iteration_has_next())
    {
        bof_write_word(bf, literal_table_iteration_next());
    }
    literal_table_end_iteration();
}

// Write the program to the binary file
static void gen_code_output_program(BOFFILE bf, code_seq main_cs)
{
    BOFHeader header = gen_code_program_header(main_cs);
    bof_write_header(bf, header);
    gen_code_output_seq(bf, main_cs);
    gen_code_output_literals(bf);
}

// Generate code for the program's block and output it
void gen_code_program(BOFFILE bf, block_t prog)
{
    code_seq main_cs = gen_code_stmts(prog.stmts);
    gen_code_output_program(bf, main_cs);
}

// Generate code for variable declarations
code_seq gen_code_var_decls(var_decls_t vds)
{
    code_seq ret = code_seq_empty();
    var_decl_t *vdp = vds.var_decls;
    while (vdp != NULL)
    {
        code_seq new_var_decl = gen_code_var_decl(*vdp);
        code_seq_concat(&ret, new_var_decl);
        vdp = vdp->next;
    }
    return ret;
}

// Generate code for a single variable declaration
code_seq gen_code_var_decl(var_decl_t vd)
{
    return gen_code_ident_list(vd.ident_list);
}

// Generate code for an identifier list
code_seq gen_code_ident_list(ident_list_t ident_list)
{
    code_seq ret = code_seq_empty();
    ident_t *idp = ident_list.start;
    while (idp != NULL)
    {
        code_seq new_ident = gen_code_ident(*idp);
        code_seq_concat(&ret, new_ident);
        idp = idp->next;
    }
    return ret;
}

// Generate code for a statement
code_seq gen_code_stmt(stmt_t stmt)
{
    switch (stmt.stmt_kind)
    {
        case assign_stmt:
            return gen_code_assign_stmt(stmt.data.assign_stmt);
        case block_stmt:
            return gen_code_begin_stmt(stmt.data.block_stmt);
        case if_stmt:
            return gen_code_if_stmt(stmt.data.if_stmt);
        case read_stmt:
            return gen_code_read_stmt(stmt.data.read_stmt);
        case print_stmt:
            return gen_code_print_stmt(stmt.data.print_stmt);
        default:
            bail_with_error("Unexpected statement kind in gen_code_stmt!");
    }
    return code_seq_empty();
}

// Generate code for assignment statements
code_seq gen_code_assign_stmt(assign_stmt_t stmt)
{
    code_seq ret = code_seq_empty();
    code_seq expr_cs = gen_code_expr(*stmt.expr);
    code_seq_concat(&ret, expr_cs);

    lexical_address *lex_addr = id_use_2_lexical_address(stmt.idu);
    code_seq fp_cs = code_utils_compute_fp(3, lex_addr->levelsOutward);
    code_seq_concat(&ret, fp_cs);

    unsigned int ofst = lex_addr->offsetInAR;
    if (ofst > USHRT_MAX) bail_with_error("Offset too large!");

    code *store_code = code_cpw(3, ofst, SP, 0);
    code_seq_add_to_end(&ret, store_code);

    code_seq dealloc_cs = code_utils_deallocate_stack_space(1);
    code_seq_concat(&ret, dealloc_cs);

    return ret;
}

// Generate code for a list of statements
code_seq gen_code_stmts(stmts_t stmts)
{
    code_seq ret = code_seq_empty();
    stmt_t *sp = stmts.stmt_list.start;
    while (sp != NULL)
    {
        code_seq stmt_cs = gen_code_stmt(*sp);
        code_seq_concat(&ret, stmt_cs);
        sp = sp->next;
    }
    return ret;
}

// (Stub for:) Generate code for BEGIN stmt
code_seq gen_code_begin_stmt(block_stmt_t stmt) {
    return gen_code_stmts(stmt.block->stmts);
}

// Generate code for the IF statement
code_seq gen_code_if_stmt(if_stmt_t stmt)
{
    code_seq ret = code_seq_empty();

    // Generate code for the condition
    code_seq cond_cs = gen_code_condition(stmt.condition);
    code_seq_concat(&ret, cond_cs);

    // Reserve labels for branching
    unsigned int else_label = code_utils_new_label();
    unsigned int end_label = stmt.else_stmts ? code_utils_new_label() : else_label;

    // Jump to else if condition is false
    code *branch_false = code_bne(3, 0, else_label); // 3 = result reg
    code_seq_add_to_end(&ret, branch_false);

    // Generate code for the "then" branch
    code_seq then_cs = gen_code_stmts(*stmt.then_stmts);
    code_seq_concat(&ret, then_cs);

    // If there is an else branch, jump to the end after executing "then"
    if (stmt.else_stmts)
    {
        code *jump_end = code_jmpa(end_label);
        code_seq_add_to_end(&ret, jump_end);
    }

    // Insert the "else" branch label
    code_seq_add_to_end(&ret, code_label(else_label));

    // Generate code for the "else" branch if it exists
    if (stmt.else_stmts)
    {
        code_seq else_cs = gen_code_stmts(*stmt.else_stmts);
        code_seq_concat(&ret, else_cs);

        // Insert the end label
        code_seq_add_to_end(&ret, code_label(end_label));
    }

    return ret;
}

// Generate code for a condition (relational or divisible)
code_seq gen_code_condition(condition_t cond)
{
    code_seq ret = code_seq_empty();

    switch (cond.cond_kind)
    {
        case ck_rel:
            // Relational condition: expr1 rel_op expr2
            code_seq left_cs = gen_code_expr(cond.data.rel_op_cond.expr1);
            code_seq right_cs = gen_code_expr(cond.data.rel_op_cond.expr2);
            code_seq rel_op_cs = gen_code_rel_op(cond.data.rel_op_cond.rel_op, expr_bin);

            code_seq_concat(&ret, left_cs);
            code_seq_concat(&ret, right_cs);
            code_seq_concat(&ret, rel_op_cs);
            break;

        case ck_db:
            // Divisibility condition: dividend % divisor == 0
            code_seq dividend_cs = gen_code_expr(cond.data.db_cond.dividend);
            code_seq divisor_cs = gen_code_expr(cond.data.db_cond.divisor);

            code_seq_concat(&ret, dividend_cs);
            code_seq_concat(&ret, divisor_cs);

            code *mod_code = code_mod(3, SP, 0); // Modulo instruction
            code_seq_add_to_end(&ret, mod_code);

            code *test_zero = code_beq(3, 0, code_utils_new_label());
            code_seq_add_to_end(&ret, test_zero);
            break;

        default:
            bail_with_error("Unexpected condition kind in gen_code_condition!");
    }

    return ret;
}

// Generate code for the READ statement
code_seq gen_code_read_stmt(read_stmt_t stmt)
{
    code_seq ret = code_seq_empty();

    // Allocate stack space
    code_seq alloc_cs = code_utils_allocate_stack_space(1);
    code_seq_concat(&ret, alloc_cs);

    // Read character into stack
    code *read_code = code_rch(SP, 0);
    code_seq_add_to_end(&ret, read_code);

    // Get lexical address of identifier
    lexical_address *lex_addr = id_use_2_lexical_address(stmt.idu);
    code_seq fp_cs = code_utils_compute_fp(3, lex_addr->levelsOutward);
    code_seq_concat(&ret, fp_cs);

    // Store result in variable
    code *store_code = code_cpw(3, lex_addr->offsetInAR, SP, 0);
    code_seq_add_to_end(&ret, store_code);

    // Deallocate stack space
    code_seq dealloc_cs = code_utils_deallocate_stack_space(1);
    code_seq_concat(&ret, dealloc_cs);

    return ret;
}

// Generate code for the PRINT statement
code_seq gen_code_print_stmt(print_stmt_t stmt)
{
    code_seq ret = code_seq_empty();

    // Evaluate expression and push result onto stack
    code_seq expr_cs = gen_code_expr(stmt.expr);
    code_seq_concat(&ret, expr_cs);

    // Print top of stack
    code *print_code = code_pint(SP, 0);
    code_seq_add_to_end(&ret, print_code);

    // Deallocate stack space
    code_seq dealloc_cs = code_utils_deallocate_stack_space(1);
    code_seq_concat(&ret, dealloc_cs);

    return ret;
}

// Generate code for an expression
code_seq gen_code_expr(expr_t exp)
{
    code_seq ret = code_seq_empty();

    switch (exp.expr_kind)
    {
        case expr_bin:
            return gen_code_binary_op_expr(exp.data.binary);
        case expr_ident:
            return gen_code_ident(exp.data.ident);
        case expr_number:
            return gen_code_number(exp.data.number);
        case expr_negated:
            return gen_code_logical_not_expr(*exp.data.negated.expr);
        default:
            bail_with_error("Unexpected expression kind in gen_code_expr!");
    }

    return ret;
}

// Generate code for a binary operation expression
code_seq gen_code_binary_op_expr(binary_op_expr_t exp)
{
    code_seq ret = code_seq_empty();

    // Generate left and right sub-expressions
    code_seq left_cs = gen_code_expr(*exp.expr1);
    code_seq right_cs = gen_code_expr(*exp.expr2);

    // Generate operator code
    code_seq op_cs = gen_code_op(exp.arith_op, expr_bin);

    code_seq_concat(&ret, left_cs);
    code_seq_concat(&ret, right_cs);
    code_seq_concat(&ret, op_cs);

    return ret;
}

// Generate code to apply an operator to the top two stack elements
// and replace them with the result
code_seq gen_code_op(token_t op, expr_kind_e typ)
{
    code_seq ret = code_seq_empty();

    // Determine the operator type and generate the appropriate instruction
    switch (op.code)
    {
        case '+': // Addition
            code_seq_add_to_end(&ret, code_add(SP, 0, SP, -1));
            break;

        case '-': // Subtraction
            code_seq_add_to_end(&ret, code_sub(SP, 0, SP, -1));
            break;

        case '*': // Multiplication
            code_seq_add_to_end(&ret, code_mul(SP, 0));
            break;

        case '/': // Division
            code_seq_add_to_end(&ret, code_div(SP, 0));
            break;

        case '<': // Less than
            code_seq_add_to_end(&ret, code_lt(typ));
            break;

        case '>': // Greater than
            code_seq_add_to_end(&ret, code_gt(typ));
            break;

        case '=': // Equal
            code_seq_add_to_end(&ret, code_eq(typ));
            break;

        case '&': // Logical AND
            code_seq_add_to_end(&ret, code_and(SP, 0, SP, -1));
            break;

        case '|': // Logical OR
            code_seq_add_to_end(&ret, code_bor(SP, 0, SP, -1));
            break;

        default:
            bail_with_error("Unsupported operator in gen_code_op: '%c'", op.code);
    }

    return ret;
}

// Generate code to apply the floating point arith_op to the 2nd from top and top of the stack putting the result on top of the stack in their place
code_seq gen_code_arith_op(token_t arith_op) {
    return gen_code_op(arith_op, expr_bin);
}

// Gen code for rel_op applied to 2nd from top and top of the stack putting result on top of the stack in their place
code_seq gen_code_rel_op(token_t rel_op, expr_kind_e typ) {
    code_seq ret = code_seq_empty();
    switch (rel_op.code) {
        case '<': code_seq_add(&ret, code_lt(typ)); break;
        case '>': code_seq_add(&ret, code_gt(typ)); break;
        case '=': code_seq_add(&ret, code_eq(typ)); break;
        default:
            bail_with_error("Unexpected relational operator in gen_code_rel_op!");
    }
    return ret;
}

// Generate code for an identifier
code_seq gen_code_ident(ident_t id)
{
    code_seq ret = code_seq_empty();

    lexical_address *addr = id_use_2_lexical_address(id.idu);
    code *load_var = code_lwr(addr->levelsOutward, addr->offsetInAR, 0);
    code_seq_add_to_end(&ret, load_var);

    return ret;
}

// Generate code for a number
code_seq gen_code_number(number_t num)
{
    code_seq ret = code_seq_empty();
    unsigned int offset = literal_table_find_or_add(num.text, num.value);
    code *push_literal = code_lit(SP, 0, offset);
    code_seq_add_to_end(&ret, push_literal);
    return ret;
}

// Generate code for a logical NOT expression
code_seq gen_code_logical_not_expr(expr_t exp)
{
    code_seq expr_cs = gen_code_expr(exp);
    code *not_code = code_not();
    code_seq_add_to_end(&expr_cs, not_code);
    return expr_cs;
}
