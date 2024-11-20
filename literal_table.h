// Daniel Landsman
// literal_table.h: literal table header file, includes function declarations
#ifndef _LITERAL_TABLE_H
#define _LITERAL_TABLE_H
#include <stdbool.h>

// Pre-Conditions: None
// Post-Conditions: Initializes an empty literal table
extern void literal_table_initialize();

// Pre-Conditions: Literal table has been properly initialized
// Post-Conditions: Returns the number of entries in the literal table as an unsigned int
extern unsigned int literal_table_size();

// Pre-Conditions: Literal table has been properly initialized
// Post-Conditions: Returns true if the literal table is empty, false otherwise
extern bool literal_table_empty();

// Pre-Conditions: Literal table has been properly initialized,
// target_text is a valid string
// Post-Conditions: Searches the literal table for an entry that matches
// target_text, returns matching entry's offset if found, returns -1 if not found 
extern int literal_table_search_ofst(const char* target_text);

// Pre-Conditions: Literal table has been properly initialized,
// target_text is a valid string
// Post-Conditions: Searches the literal table for an entry that matches
// target_text, returns true if found, false otherwise.
// Effectively a boolean version of literal_table_search_ofst
extern bool literal_table_contains(const char* target_text);

// Pre-Conditions: Literal table has been properly initialized,
// target_text is a valid string
// Post-Conditions: Searches the literal table for an entry that matches
// target_text, returns matching entry's offset if found, adds a new
// corresponding entry to the table and returns its new offset if not found.
extern unsigned int literal_table_find_or_add(const char* target_text, int my_val);

// Pre-Conditions: Literal table has been properly initialized
// Post-Conditions: Starts the iteration process for the literal table,
// produces error message if literal table is already iterating
extern void literal_table_start_iteration();

// Pre-Conditions: Literal table has been properly initalized 
// and is currently iterating
// Post-Conditions: Ends the iteration process for the literal table
extern void literal_table_end_iteration();

// Pre-Conditions: Literal table has been properly initialized
// and is currently iterating
// Post-Conditions: Checks if there is a next entry in the table,
// returns true if there is, returns false and ends iteration otherwise
extern bool literal_table_iteration_has_next();

// Pre-Conditions: Literal table has been properly initialized
// Post-Conditions: Returns the integer value of the next entry,
// produces error message if next entry does not exist
extern int literal_table_iteration_next();

#endif