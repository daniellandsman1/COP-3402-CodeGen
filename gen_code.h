// Christian Morton, Ryan Rohan
// gen_code.h: code generation header file, includes function declarations
#ifndef _GEN_CODE_H
#define _GEN_CODE_H
#include <stdio.h>
#include "ast.h"
#include "bof.h"
#include "instruction.h"
#include "code_seq.h"

// Pre-Conditions: None
extern void gen_code_initialize();

extern void gen_code_program(BOFFILE bf, block_t prog);

extern code_seq gen_code_var_decls(var_decls_t vds);

extern code_seq gen_code_var_decl(var_decl_t vd);

extern code_seq gen_code_ident_list(ident_list_t ident_list);

extern code_seq gen_code_stmt(stmt_t stmt);

extern code_seq gen_code_assign_stmt(assign_stmt_t stmt);

extern code_seq gen_code_stmts(stmts_t stmts);

extern code_seq gen_code_begin_stmt(block_stmt_t stmt); // may not be needed

extern code_seq gen_code_if_stmt(if_stmt_t stmt);

extern code_seq gen_code_condition(condition_t cond);

extern code_seq gen_code_read_stmt(read_stmt_t stmt);

extern code_seq gen_code_print_stmt(print_stmt_t stmt);

extern code_seq gen_code_expr(expr_t exp);

extern code_seq gen_code_binary_op_expr(binary_op_expr_t exp);

extern code_seq gen_code_op(token_t op);

extern code_seq gen_code_arith_op(token_t arith_op);

extern code_seq gen_code_rel_op(token_t rel_op);

extern code_seq gen_code_ident(ident_t id);

extern code_seq gen_code_number(number_t num);

extern code_seq gen_code_block(block_t block);

// Functions for handling constant declarations and definitions
extern code_seq gen_code_constDecls(const_decls_t cds);

extern code_seq gen_code_constDecl(const_decl_t cd);

extern code_seq gen_code_const_def_list(const_def_list_t cdl);

extern code_seq gen_code_constDef(const_def_t cdf);

#endif