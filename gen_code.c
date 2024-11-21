// Christian Morton, Michael John
// gen_code.c: code generation file, includes function bodies
#include <string.h>
#include "gen_code.h"
#include "literal_table.h"
#include "id_use.h"
#include "utilities.h"
#include "regname.h"

// Initialize the code generator
void gen_code_initialize()
{
	literal_table_initialize();
}

// (Stub for:) Write all instructions in cs to bf in order
static void gen_code_output_seq(BOFFILE bf, code_seq cs) {
	
}

// (Stub for:) Return a header appropriate for the given code
static BOFHeader gen_code_program_header(code_seq main_cs) {
	
}

// (Stub for:)
static void gen_code_output_literals(BOFFILE bf) {
	
}

// (Stub for:) Write the program's BOFFILE to bf
static void gen_code_output_program(BOFFILE bf, code_seq main_cs) {
	
}

// (Stub for:) Generate code for prog into bf
void gen_code_program(BOFFILE bf, block_t prog) {
	
}

// (Stub for:) Generate code for the var_decls_t vds to output
code_seq gen_code_var_decls(var_decls_t vds) {
    code_seq ret = code_seq_empty();
    var_decl_t *vdp = vds.var_decls;
    while(vdp != NULL){
        code_seq new_var_decl = gen_code_var_decl(*vdp);
        code_seq_concat(&new_var_decl, ret);
        ret = new_var_decl;
        vdp = vdp->next;
    }
    return ret;	
}

// (Stub for:) Generate ode for a single <var-decl>, vd
code_seq gen_code_var_decl(var_decl_t vd) {
	return gen_code_ident_list(vd.idents, vd.type);
}

// (Stub for:) Generate code for the identifiers in ident_list in reverse order
code_seq gen_code_ident_list(ident_list_t ident_list) {
	code_seq ret = code_seq_empty();
    ident_t *idp = ident_list.idents;
    while(idp != NULL){

    }
    return ret;
}

// (Stub for:) Generate code for stmt
code_seq gen_code_stmt(stmt_t stmt) {
	switch (stmt.stmt_kind) {
		case assign_stmt:
			return gen_code_assign_stmt(stmt.data.assign_stmt);
			break;
		case begin_stmt:
			return gen_code_begin_stmt(stmt.data.block_stmt);
			break;
		case if_stmt:
			return gen_code_if_stmt(stmt.data.if_stmt);
			break;
		case read_stmt:
			return gen_code_read_stmt(stmt.data.read_stmt);
			break;
		case write_stmt:
			return gen_code_write_stmt(stmt.data.write_stmt);
			break;
		default:
			bail_with_error("Call to gen_code_stmt with an AST that is not a statement!");
			break;
	}
	
	return code_seq_empty();
}

// (Stub for:) Generate code for ASSIGN stmt
code_seq gen_code_assign_stmt(assign_stmt_t stmt) {
	
}

// (Stub for:) Generate code for BEGIN stmt
code_seq gen_code_begin_stmt(begin_stmt_t stmt) {
	
}

// (Stub for:) Generate code for the list of statements given by stmts
code_seq gen_code_stmts(stmts_t stmts) {
    code_seq ret = code_seq_empty();
    stmt_t *sp = stmts.stmts;
    while(sp != NULL){
        ret = code_seq_concat(ret, gen_code_stmt(*sp));
        sp = sp->next;
    }
	return ret;
}

// (Stub for:) Generate code for the IF stmt
code_seq gen_code_if_stmt(if_stmt_t stmt) {
	
}

// (Stub for:) Generate code for the READ stmt
code_seq gen_code_read_stmt(read_stmt_t stmt) {
	
}

// (Stub for:) Generate code for the WRITE stmt
code_seq gen_code_write_stmt(write_stmt_t stmt) {
	
}

// (Stub for:) Generate code for the expression exp. Put result at the top of the stack
code_seq gen_code_expr(expr_t exp) {
	switch(exp.expr_kind){
        case expr_bin_op:
            return gen_code_binary_op_expr(exp.data.binary);
        case expr_indent:
            return gen_code_ident(exp.data.ident);
        case expr_number:
            return gen_code_number(exp.data.number);
        case expr_logical_not:
            return gen_code_logical_not_expr(*(exp.data.logical_not));
        default:
            bail_with_error("Unexpected expr_kind_e (%d) in gen_code_expr", exp.expr_kind);
    }
    return code_seq_empty();
}

// (Stub for:) Generate code for the expression exp
code_seq gen_code_binary_op_expr(binary_op_expr_t exp) {
	
}

// (Stub for:) Generate code to apply op to the 2nd from top and top of the stack putting the result on top of the stack in their place
code_seq gen_code_op(token_t op, type_exp_e typ) {
	
}

// (Stub for:) Generate code to apply the floating point arith_op to the 2nd from top and top of the stack putting the result on top of the stack in their place
code_seq gen_code_arith_op(token_t arith_op) {
	
}

// (Stub for:) Gen code for rel_op applied to 2nd from top and top of the stack putting result on top of the stack in their place
code_seq gen_code_rel_op(token_t rel_op, type_exp_e typ) {
	
}

// (Stub for:) Put the value of the given identifier on top of the stack
code_seq gen_code_ident(ident_t id) {
	
}

// (Stub for:) Put the given number on top of the stack
code_seq gen_code_number(number_t num) {
	
}

// (Stub for:) Generate code for the expression exp putting the result at top of the stack
code_seq gen_code_logical_not_expr(expr_t exp) {
	
}
