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
#include "spl.tab.h"
#include "instruction.h"

#define STACK_SPACE 4096
#define SAVED_STATIC_LINK_OFFSET (-3)

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
static BOFHeader gen_code_program_header(code_seq main_cs) {
    BOFHeader header;
    bof_write_magic_to_header(&header); // Write magic using provided function
    header.text_start_address = 0;

    header.text_length = code_seq_size(main_cs);
    int data_start = MAX(header.text_length, 1024);
    header.data_start_address = data_start;
    header.data_length = literal_table_size();
    header.stack_bottom_addr = data_start + header.data_length + STACK_SPACE;

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
void gen_code_program(BOFFILE bf, block_t prog) {

    code_seq program_cs = code_seq_empty();

    // Set up the program
    code_seq setup_cs = code_utils_set_up_program();
    code_seq_concat(&program_cs, setup_cs);

    // Generate code for the program's block
    code_seq block_cs = gen_code_block(prog);
    code_seq_concat(&program_cs, block_cs);

    // Tear down the program
    code_seq teardown_cs = code_utils_tear_down_program();
    code_seq_concat(&program_cs, teardown_cs);

    // Output the program
    gen_code_output_program(bf, program_cs);
}

// Generate code for a block
code_seq gen_code_block(block_t block) {
    code_seq ret = code_seq_empty();

    // Handle empty blocks
    if (block.var_decls.var_decls == NULL && block.stmts.stmt_list.start == NULL &&
        block.const_decls.start == NULL) {  // Fix for `const_decls`
        code *exit_instr = code_exit(0);
        code_seq_add_to_end(&ret, exit_instr);
        return ret;
    }

    // Save static link
    code *store_static_link = code_swr(FP, SAVED_STATIC_LINK_OFFSET, FP);
    code_seq_add_to_end(&ret, store_static_link);

    // Save frame pointer
    code_seq save_fp_cs = code_utils_copy_regs(3, FP);
    code_seq_concat(&ret, save_fp_cs);

    // Save registers
    code_seq save_cs = code_utils_save_registers_for_AR();
    code_seq_concat(&ret, save_cs);

    // Handle const declarations
    if (block.const_decls.start != NULL) {  // Fix for `const_decls`
        code_seq const_decls_cs = gen_code_constDecls(block.const_decls);
        code_seq_concat(&ret, const_decls_cs);
    }

    // Generate variable declarations
    code_seq var_decls_cs = gen_code_var_decls(block.var_decls);
    code_seq_concat(&ret, var_decls_cs);

    // Generate statements
    code_seq stmts_cs = gen_code_stmts(block.stmts);
    code_seq_concat(&ret, stmts_cs);

    // Restore registers
    code_seq restore_cs = code_utils_restore_registers_from_AR();
    code_seq_concat(&ret, restore_cs);

    return ret;
}

// Generate code for variable declarations
code_seq gen_code_var_decls(var_decls_t vds)
{
    code_seq ret = code_seq_empty();
    
    var_decl_t *vdp = vds.var_decls; // Start with the first variable declaration
    if (vdp == NULL) {
        return ret; // No declarations, return an empty code sequence
    }

    while (vdp != NULL)
    {
        code_seq new_var_decl = gen_code_var_decl(*vdp);
        code_seq_concat(&ret, new_var_decl);
        vdp = vdp->next; // Move to the next variable declaration
    }

    return ret;
}

// Generate code for a single variable declaration
code_seq gen_code_var_decl(var_decl_t vd)
{
    code_seq ret = gen_code_ident_list(vd.ident_list); // Generate code for the identifier list
    return ret;
}

// Generate code for an identifier list
code_seq gen_code_ident_list(ident_list_t ident_list)
{
    code_seq ret = code_seq_empty();
    ident_t *idp = ident_list.start; // Start with the first identifier
    
    while (idp != NULL)
    {
        // Allocate room on the stack for the identifier
        code_seq alloc_and_init_cs = code_utils_allocate_stack_space(1);
        
        // Initialize to 0
        code_seq_add_to_end(&alloc_and_init_cs, code_lit(SP, 0, 0));
        
        // Append in reverse order
        code_seq temp = ret;  
        ret = alloc_and_init_cs;  
        code_seq_concat(&ret, temp); 
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
    if (stmts.stmts_kind == empty_stmts_e)
    {
        return code_seq_singleton(code_nop());
    }

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

// Generate code for the IF statement (offset instead of label)
code_seq gen_code_if_stmt(if_stmt_t stmt) {
    code_seq ret = code_seq_empty();
    unsigned int curr_offset = 0;  // Initialize current offset

    // Generate code for the condition
    code_seq cond_cs = gen_code_condition(stmt.condition);
    code_seq_concat(&ret, cond_cs);
    
    // The result of the condition is now on the stack: 0 (false) or 1 (true)
    // We want to jump to the "else" block if false, or proceed with "then" if true
    // Calculate the offsets for conditional branches
    unsigned int skip_else_offset = 3;  // We'll skip 3 instructions for the "else" part
    unsigned int end_offset = stmt.else_stmts ? 6 : 3;  // If there's an else, we skip 6 instructions; otherwise 3

    // Branch to else if condition is false (using offset)
    code *branch_false = code_bne(SP, 1, skip_else_offset); // Compare the condition (top of stack) to 0
    code_seq_add_to_end(&ret, branch_false);
    curr_offset += skip_else_offset;

    // Generate code for the "then" branch
    code_seq then_cs = gen_code_stmts(*stmt.then_stmts);
    code_seq_concat(&ret, then_cs);
    curr_offset += code_seq_size(then_cs);

    // If there is an else branch, jump to the end after executing "then"
    if (stmt.else_stmts) {
        code *jump_end = code_jrel(end_offset);  // Jump to the end offset after "then" part
        code_seq_add_to_end(&ret, jump_end);
        curr_offset += 3;  // Add 3 to the offset for the jump instruction
    }

    // Insert the "else" branch code
    code_seq_add_to_end(&ret, code_lit(SP, 1, 0)); // Push 0 (false) onto the stack
    curr_offset += 3;  // Account for the instruction

    // Generate code for the "else" branch if it exists
    if (stmt.else_stmts) {
        code_seq else_cs = gen_code_stmts(*stmt.else_stmts);
        code_seq_concat(&ret, else_cs);
        curr_offset += code_seq_size(else_cs);
    }

    // Push 1 (true) if the condition is true and the "else" block isn't executed
    code_seq_add_to_end(&ret, code_lit(SP, 1, 1));
    curr_offset += 3;  // Account for this instruction

    // Deallocate space for the condition result (top of stack) and update stack
    code_seq dealloc_cs = code_utils_deallocate_stack_space(1);
    code_seq_concat(&ret, dealloc_cs);

    return ret;
}

// Generate code for a condition (relational or divisible)
code_seq gen_code_condition(condition_t cond)
{
    code_seq ret = code_seq_empty();

    switch (cond.cond_kind)
    {
        // Relational condition: expr1 rel_op expr2
        case ck_rel:
            code_seq left_cs = gen_code_expr(cond.data.rel_op_cond.expr1);
            code_seq right_cs = gen_code_expr(cond.data.rel_op_cond.expr2);
            code_seq rel_op_cs = gen_code_rel_op(cond.data.rel_op_cond.rel_op);

            // Push in reverse so expr1 ends up on top of stack
            code_seq_concat(&ret, right_cs);
            code_seq_concat(&ret, left_cs);
            code_seq_concat(&ret, rel_op_cs);
            break;

        // Divisibility condition: dividend % divisor == 0
        case ck_db:
            
            // Evaluate the expressions for dividend and divisor and push onto stack
            // Make sure dividend ends up on top so it can be used as dividend in DIV instruction
            code_seq divisor_cs = gen_code_expr(cond.data.db_cond.divisor);
            code_seq dividend_cs = gen_code_expr(cond.data.db_cond.dividend);
            code_seq_concat(&ret, divisor_cs);
            code_seq_concat(&ret, dividend_cs);

            // Compute division, which places remainder in HI register
            code* div_code = code_div(SP, 1);
            code_seq_add_to_end(&ret, div_code);

            // Fetch remainder and place at SP + 1, where divisor was
            code* cfhi_code = code_cfhi(SP, 1);
            code_seq_add_to_end(&ret, cfhi_code);

            // Push 0 onto the stack (replacing dividend) to compare
            code* push_zero_code = code_lit(SP, 0, 0);
            code_seq_add_to_end(&ret, push_zero_code);

            // Skip over next two instructions if remainder is equal to 0
            code* compare_zero_code = code_beq(SP, 1, 2);
            code_seq_add_to_end(&ret, compare_zero_code);

            // Push 0 (false) on stack, replacing remainder, not divisible
            code* push_false_code = code_lit(SP, 1, 0);
            code_seq_add_to_end(&ret, push_false_code);

            // Skip over next instruction that pushes true on stack
            code* skip_push_one_code = code_jrel(2);
            code_seq_add_to_end(&ret, skip_push_one_code);

            // Put 1 (true) on stack, divisible
            code* push_one_code = code_lit(SP, 1, 1);
            code_seq_add_to_end(&ret, push_one_code);

            // Deallocate the 0 at top of stack, making the truth value the
            // new top of the stack
            code_seq dealloc_cs = code_utils_deallocate_stack_space(1);
            code_seq_concat(&ret, dealloc_cs);

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
            break;
        case expr_ident:
            return gen_code_ident(exp.data.ident);
            break;
        case expr_number:
            return gen_code_number(exp.data.number);
            break;
        case expr_negated:
            code_seq_concat(&ret, gen_code_expr(*exp.data.negated.expr));

            // Allocate space to push a 0 on the stack
            code_seq alloc_cs = code_utils_allocate_stack_space(1);
            code_seq_concat(&ret, alloc_cs);
            code_seq_add_to_end(&ret, code_lit(SP, 0, 0));

            // Subtract value of expression from 0 to get negated version
            code_seq_add_to_end(&ret, code_sub(SP, 1, SP, 1));

            // Deallocate top of stack, leaving negated expression as new top
            code_seq dealloc_cs = code_utils_deallocate_stack_space(1);
            code_seq_concat(&ret, dealloc_cs);

            return ret;
            break;
        default:
            bail_with_error("Unexpected expression kind in gen_code_expr!");
            break;
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
    code_seq op_cs = gen_code_op(exp.arith_op);

    code_seq_concat(&ret, left_cs);
    code_seq_concat(&ret, right_cs);
    code_seq_concat(&ret, op_cs);

    return ret;
}

// Generate code to apply an operator to the top two stack elements
// and replace them with the result
code_seq gen_code_op(token_t op)
{
    // Determine the operator type and generate the appropriate instruction
    switch (op.code)
    {
        case eqeqsym: case neqsym:
        case ltsym: case leqsym:
        case gtsym: case geqsym:
            return gen_code_rel_op(op);
            break;

        case plussym: case minussym:
        case multsym: case divsym:
            return gen_code_arith_op(op);
            break;

        default:
            bail_with_error("Unsupported operator in gen_code_op: '%c'", op.code);
            break;
    }

    return code_seq_empty();
}

// Generate code to apply the floating point arith_op to the 2nd from top and top of the stack putting the result on top of the stack in their place
code_seq gen_code_arith_op(token_t arith_op) {
    return gen_code_op(arith_op);
}

// Gen code for rel_op applied to 2nd from top and top of the stack putting result on top of the stack in their place
code_seq gen_code_rel_op(token_t rel_op) 
{
    code_seq ret = code_seq_empty(); // Build up code sequence to return
    code_seq op_cs = code_seq_empty(); // Sequence to do operation
    
    // Note: Both operands are already on top of the stack,
    // with expr1 in SP and expr2 in SP + 1
    switch (rel_op.code)
    {
        case eqeqsym:
            op_cs = code_seq_singleton(code_beq(SP, 1, 3));
            break;
        case neqsym:
            op_cs = code_seq_singleton(code_bne(SP, 1, 3));
            break;
        case ltsym:
            op_cs = code_seq_singleton(code_sub(SP, 0, SP, 1));
            code_seq_add_to_end(&op_cs, code_bltz(SP, 0, 3));
            break;
        case leqsym:
            op_cs = code_seq_singleton(code_sub(SP, 0, SP, 1));
            code_seq_add_to_end(&op_cs, code_blez(SP, 0, 3));
            break;
        case gtsym:
            op_cs = code_seq_singleton(code_sub(SP, 0, SP, 1));
            code_seq_add_to_end(&op_cs, code_bgtz(SP, 0, 3));
            break;
        case geqsym:
            op_cs = code_seq_singleton(code_sub(SP, 0, SP, 1));
            code_seq_add_to_end(&op_cs, code_bgez(SP, 0, 3));
            break;
        default:
            bail_with_error("Unexpected relational operator in gen_code_rel_op!");
            break;
    }

    // Add code for operation, may skip next two instructions
    code_seq_concat(&ret, op_cs);

    // Push 0 (false) on stack at SP + 1, will become top after dealloc
    code_seq_add_to_end(&ret, code_lit(SP, 1, 0));
    code_seq_add_to_end(&ret, code_jrel(2)); // Skip pushing true value

    // Push 1 (true) on stack at SP + 1, will become top after dealloc
    code_seq_add_to_end(&ret, code_lit(SP, 1, 1));

    // Deallocate expr1 from stack, leaving truth value as new top
    code_seq dealloc_cs = code_utils_deallocate_stack_space(1);
    code_seq_concat(&ret, dealloc_cs);

    return ret;
}

// Generate code for an identifier
code_seq gen_code_ident(ident_t id)
{
    code_seq ret = code_seq_empty();

    if (id.idu == NULL) {
        bail_with_error("Invalid identifier: id_use is NULL.");
    }

    lexical_address *addr = id_use_2_lexical_address(id.idu);
    if (addr == NULL) {
        bail_with_error("Invalid lexical address.");
    }

    code *load_var = code_lwr(addr->levelsOutward, addr->offsetInAR, 0);
    if (load_var == NULL) {
        bail_with_error("Failed to generate code for loading variable.");
    }

    code_seq_add_to_end(&ret, load_var);

    return ret;
}

// Generate code for a number
code_seq gen_code_number(number_t num)
{
    code_seq ret = code_seq_empty(); // Build up code sequence to return

    // Get literal's offset and allocate stack space  
    unsigned int offset = literal_table_find_or_add(num.text, num.value);
    code_seq alloc_cs = code_utils_allocate_stack_space(1);
    code_seq_concat(&ret, alloc_cs);

    // Use offset to push literal onto stack
    code* push_lit_code = code_cpw(SP, 0, GP, offset);
    code_seq_add_to_end(&ret, push_lit_code);

    //code *push_literal = code_lit(SP, 0, offset); 
    //code_seq_add_to_end(&ret, push_literal);
    return ret;
}

/// Generate code for const declarations list
code_seq gen_code_constDecls(const_decls_t cds) {
    code_seq ret = code_seq_empty();

    const_decl_t *cd = cds.start;  // Fix for `start`
    while (cd != NULL) {
        code_seq const_decl_cs = gen_code_constDecl(*cd);
        code_seq_concat(&ret, const_decl_cs);
        cd = cd->next;  // Traverse to the next const declaration
    }

    return ret;
}

// Generate code for a single const declaration
code_seq gen_code_constDecl(const_decl_t cd) {
    code_seq ret = code_seq_empty();

    // Handle the constant definition list
    code_seq const_def_list_cs = gen_code_const_def_list(cd.const_def_list);
    code_seq_concat(&ret, const_def_list_cs);

    return ret;
}

// Generate code for const definitions list
code_seq gen_code_const_def_list(const_def_list_t cdl) {
    code_seq ret = code_seq_empty();

    const_def_t *cdf = cdl.start;  // Fix for `start`
    while (cdf != NULL) {
        code_seq const_def_cs = gen_code_constDef(*cdf);
        code_seq_concat(&ret, const_def_cs);
        cdf = cdf->next;  // Traverse to the next const definition
    }

    return ret;
}

// Generate code for a single const definition
code_seq gen_code_constDef(const_def_t cdf) {
    code_seq ret = code_seq_empty();

    // Add the constant value to the literal table
    unsigned int offset = literal_table_find_or_add(cdf.ident.name, cdf.number.value);  // Fix for `ident` and `number`

    // Generate code to load the constant into memory
    code *load_const = code_cpw(SP, 0, GP, offset);
    code_seq_add_to_end(&ret, load_const);

    return ret;
}