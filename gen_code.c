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

// (Stub for:) Write all instructions in cs to bf in order
static void gen_code_output_seq(BOFFILE bf, code_seq cs)
{
    while (!code_seq_is_empty(cs))
    {
        bin_instr_t instr = (code_seq_first(cs))->instr;
        instruction_write_bin_instr(bf, instr);
        cs = code_seq_rest(cs);
    }
}

// (Stub for:) Return a header appropriate for the given code
static BOFHeader gen_code_program_header(code_seq main_cs) 
{
    BOFHeader header;
    bof_write_magic_to_header(&header);
    header.text_start_address = 0;
    header.text_length = code_seq_size(main_cs) * BYTES_PER_WORD;

    int data_start = MAX(header.text_length, 1024) + BYTES_PER_WORD;
    header.data_start_address = data_start;
    header.data_length = literal_table_size() * BYTES_PER_WORD;

    // do we need to add data_start + header.data_start_address? seems redundant
    // but that's what's in the professor's code
    int stack_bottom = data_start + header.data_length + STACK_SPACE;
    header.stack_bottom_addr = stack_bottom;
    
    return header;
}

// (Stub for:)
static void gen_code_output_literals(BOFFILE bf) 
{
    literal_table_start_iteration();
    while (literal_table_iteration_has_next()) 
    {
        bof_write_word(bf, literal_table_iteration_next());
    }
    literal_table_end_iteration();
}

// (Stub for:) Write the program's BOFFILE to bf
static void gen_code_output_program(BOFFILE bf, code_seq main_cs) {
    BOFHeader header = gen_code_program_header(main_cs);
    bof_write_header(bf, header);
    gen_code_output_seq(bf, main_cs);
    gen_code_output_literals(bf);
}

// (Stub for:) Generate code for prog into bf
void gen_code_program(BOFFILE bf, block_t prog) {
    code_seq main_cs = gen_code_stmts(prog.stmts);
    gen_code_output_program(bf, main_cs);
}

// (Stub for:) Generate code for the var_decls_t vds to output
code_seq gen_code_var_decls(var_decls_t vds) {
    code_seq ret = code_seq_empty();
    var_decl_t *vdp = vds.var_decls;
    while (vdp != NULL) {
        code_seq new_var_decl = gen_code_var_decl(*vdp);
        code_seq_concat(&ret, new_var_decl);
        vdp = vdp->next;
    }
    return ret;    
}

// (Stub for:) Generate ode for a single <var-decl>, vd
code_seq gen_code_var_decl(var_decl_t vd) {
    return gen_code_ident_list(vd.ident_list);
}

// (Stub for:) Generate code for the identifiers in ident_list in reverse order
code_seq gen_code_ident_list(ident_list_t ident_list) {
    code_seq ret = code_seq_empty();
    ident_t *idp = ident_list.start;
    while (idp != NULL) {
        code_seq new_ident = gen_code_ident(*idp);
        code_seq_concat(&ret, new_ident);
        idp = idp->next;
    }
    return ret;
}

// (Stub for:) Generate code for stmt
code_seq gen_code_stmt(stmt_t stmt) {
    switch (stmt.stmt_kind) {
        case assign_stmt:
            return gen_code_assign_stmt(stmt.data.assign_stmt);
        case block_stmt:
            return gen_code_begin_stmt(stmt.data.block_stmt);
        case if_stmt:
            return gen_code_if_stmt(stmt.data.if_stmt);
        case read_stmt:
            return gen_code_read_stmt(stmt.data.read_stmt);
        case print_stmt:
            return gen_code_write_stmt(stmt.data.print_stmt);
        default:
            bail_with_error("Unexpected statement kind in gen_code_stmt!");
    }
    return code_seq_empty();
}

// (Stub for:) Generate code for ASSIGN stmt
code_seq gen_code_assign_stmt(assign_stmt_t stmt) 
{
    code_seq ret = code_seq_empty(); // Store our built-up code sequence

    // Evaluate expression and push onto stack
    code_seq expr_cs = gen_code_expr(*stmt.expr);
    code_seq_concat(&ret, expr_cs); // Put it in the sequence

    // Get lexical address of variable on left hand side of statement
    if (stmt.idu == NULL) bail_with_error("Assignment statement has NULL id_use field!");
    lexical_address *lex_addr = id_use_2_lexical_address(stmt.idu);
    code_seq fp_cs = code_utils_compute_fp(3, lex_addr->levelsOutward);
    code_seq_concat(&ret, fp_cs);

    // Store result of expression into variable
    unsigned int ofst = lex_addr->offsetInAR;
    if (ofst > USHRT_MAX) bail_with_error("Offset of LHS variable in assignment statement is too large!");
    code* store_code = code_cpw(3, ofst, SP, 0);
    code_seq_add_to_end(&ret, store_code);

    // Deallocate stack space that was allocated by gen_code_expr
    code_seq dealloc_cs = code_utils_deallocate_stack_space(1);
    code_seq_concat(&ret, dealloc_cs);

    return ret;
}

// (Stub for:) Generate code for BEGIN stmt
code_seq gen_code_begin_stmt(block_stmt_t stmt) {
    return gen_code_stmts(stmt.block->stmts);
}

// (Stub for:) Generate code for the list of statements given by stmts
code_seq gen_code_stmts(stmts_t stmts) {
    code_seq ret = code_seq_empty();
    stmt_t *sp = stmts.stmt_list.start;
    while (sp != NULL) {
        code_seq stmt_cs = gen_code_stmt(*sp);
        code_seq_concat(&ret, stmt_cs);
        sp = sp->next;
    }
    return ret;
}

// (Stub for:) Generate code for the IF stmt
code_seq gen_code_if_stmt(if_stmt_t stmt) {
  
}

// (Stub for:) Generate code for the READ stmt
code_seq gen_code_read_stmt(read_stmt_t stmt) {
    code_seq ret = code_seq_empty();
    code_seq_add(&ret, code_read());
    lexical_address *addr = id_use_2_lexical_address(stmt.idu);
    code_seq_add(&ret, code_store(*addr));
    return ret;
}

// (Stub for:) Generate code for the WRITE stmt
code_seq gen_code_write_stmt(print_stmt_t stmt) {
    code_seq expr_cs = gen_code_expr(stmt.expr);
    code_seq ret = expr_cs;
    code_seq_add(&ret, code_write());
    return ret;
}

// (Stub for:) Generate code for the expression exp. Put result at the top of the stack
// MAKE SURE EACH GEN_CODE FUNCTION ALLOCATES STACK SPACE
code_seq gen_code_expr(expr_t exp) {
    switch (exp.expr_kind) {
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
    return code_seq_empty();
}

// (Stub for:) Generate code for the expression exp
code_seq gen_code_binary_op_expr(binary_op_expr_t exp) {
    code_seq left_cs = gen_code_expr(*exp.expr1);
    code_seq right_cs = gen_code_expr(*exp.expr2);
    code_seq op_cs = gen_code_op(exp.arith_op, expr_bin);
    code_seq ret = code_seq_empty();
    code_seq_concat(&ret, left_cs);
    code_seq_concat(&ret, right_cs);
    code_seq_concat(&ret, op_cs);
    return ret;
}

// (Stub for:) Generate code to apply op to the 2nd from top and top of the stack putting the result on top of the stack in their place
code_seq gen_code_op(token_t op, expr_kind_e typ) {

}

// (Stub for:) Generate code to apply the floating point arith_op to the 2nd from top and top of the stack putting the result on top of the stack in their place
code_seq gen_code_arith_op(token_t arith_op) {
    return gen_code_op(arith_op, expr_bin);
}

// (Stub for:) Gen code for rel_op applied to 2nd from top and top of the stack putting result on top of the stack in their place
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

// (Stub for:) Put the value of the given identifier on top of the stack
code_seq gen_code_ident(ident_t id) {
    code_seq ret = code_seq_empty();
    lexical_address *addr = id_use_2_lexical_address(id.idu);
    code_seq_add(&ret, code_push_variable(*addr));
    return ret;
}

// (Stub for:) Put the given number on top of the stack
code_seq gen_code_number(number_t num) {
	code_seq ret = code_seq_empty();
    unsigned int offset = literal_table_find_or_add(num.text, num.value);
    code_seq_add(&ret, code_push_literal(offset));
    return ret;
}

// (Stub for:) Generate code for the expression exp putting the result at top of the stack
code_seq gen_code_logical_not_expr(expr_t exp) {
    code_seq expr_cs = gen_code_expr(exp);
    code_seq not_cs = code_seq_empty();
    code_seq_add(&not_cs, code_not());
    code_seq_concat(&expr_cs, not_cs);
    return expr_cs;
}
