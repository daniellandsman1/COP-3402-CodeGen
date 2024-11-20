// Daniel Landsman
// literal_table.c: literal table file, includes literal table data structure and function bodies
#include <string.h>
#include "literal_table.h"
#include "utilities.h"

// Literal table data structure, contains next pointer for
// linked list implementation, as well as an offset, a string,
// and an integer value
typedef struct lit_table_entry
{
    struct lit_table_entry* next; // Next pointer for linked list
    unsigned int ofst; // Offset of the entry in the literal table
    const char* text; // String field
    int val; // Integer value field
} literal_table_entry;

static literal_table_entry* start; // Beginning of linked list
static literal_table_entry* end; // End of linked list
static unsigned int next_word_ofst; // Offset of the next entry in the table

static bool currently_iter; // True if currently iterating through literal table, false otherwise
static literal_table_entry* next_iter; // Next position in iteration process

// Pre-Conditions: None
// Post-Conditions: Verifies if literal table is in a valid state,
// produces error message if it is not
static void literal_table_invariant()
{
    bool is_empty = literal_table_empty(); // True if empty, false otherwise

    // If empty, next_word_ofst must be 0; if not empty, next_word_ofst must not be 0
    if (is_empty != (next_word_ofst == 0))
    {
        bail_with_error("Invariant violation: Literal table empty status doesn't match next word offset!");
    }

    // If empty, start must be NULL; if not empty, start must not be NULL
    if (is_empty != (start == NULL))
    {
        bail_with_error("Invariant violation: Literal table empty status doesn't match start of table!");
    }

    // If empty, end must be NULL; if not empty, end must not be NULL
    if (is_empty != (end == NULL))
    {
        bail_with_error("Invariant violation: Literal table empty status doesn't match end of table!");
    }
}

// Pre-Conditions: None
// Post-Conditions: Initializes an empty literal table
void literal_table_initialize()
{
    // Default fields of empty literal table
    start = NULL;
    end = NULL;
    next_word_ofst = 0;

    literal_table_invariant(); // Check for valid state

    currently_iter = false; // By default, we are not iterating
    next_iter = NULL; // Nothing ahead in iteration process

    literal_table_invariant(); // Check for valid state
}

// Pre-Conditions: Literal table has been properly initialized
// Post-Conditions: Returns the number of entries in the literal table as an unsigned int
unsigned int literal_table_size()
{
    return next_word_ofst; // Corresponds to number of entries
}

// Pre-Conditions: Literal table has been properly initialized
// Post-Conditions: Returns true if the literal table is empty, false otherwise
bool literal_table_empty()
{
    return (literal_table_size() == 0);
}

// Pre-Conditions: Literal table has been properly initialized,
// target_text is a valid string
// Post-Conditions: Searches the literal table for an entry that matches
// target_text, returns matching entry's offset if found, returns -1 if not found 
int literal_table_search_ofst(const char* target_text)
{
    literal_table_invariant(); // Check for valid state

    literal_table_entry* cur = start; // Pointer to iterate through table

    // Iterate through literal table
    while (cur != NULL)
    {
        // If matching text string is found
        if (!strcmp(cur->text, target_text))
        {
            return cur->ofst; // Return the entry's offset
        }

        cur = cur->next; // Go to next entry
    }

    return -1; // Matching entry not found
}

// Pre-Conditions: Literal table has been properly initialized,
// target_text is a valid string
// Post-Conditions: Searches the literal table for an entry that matches
// target_text, returns true if found, false otherwise.
// Effectively a boolean version of literal_table_search_ofst
bool literal_table_contains(const char* target_text)
{
    literal_table_invariant(); // Check for valid state

    // If search function returns non-negative value (not -1),
    // matching entry was found
    return (literal_table_search_ofst(target_text) >= 0);
}

// Pre-Conditions: Literal table has been properly initialized,
// target_text is a valid string
// Post-Conditions: Searches the literal table for an entry that matches
// target_text, returns matching entry's offset if found, adds a new
// corresponding entry to the table and returns its new offset if not found
unsigned int literal_table_find_or_add(const char* target_text, int my_val)
{
    // Check if target_text already matches an entry in the table
    int ret_ofst = literal_table_search_ofst(target_text);

    // If it does, we don't need to add it, just return its offset
    if (literal_table_contains(target_text)) return ret_ofst;

    // target_text not found in literal table, we need to add it
    // Create new literal table entry
    literal_table_entry* new_entry = (literal_table_entry*)malloc(sizeof(literal_table_entry));
    if (new_entry == NULL) bail_with_error("Attempt to allocate new literal table entry failed!");

    ret_ofst = next_word_ofst; // Save current offset to return

    // Set the new entry's fields appropriately
    new_entry->next = NULL; // Set next to NULL
    new_entry->ofst = next_word_ofst++; // Next offset is one more than current offset
    new_entry->text = target_text; // Set new text
    new_entry->val = my_val; // Set new value

    // If linked list empty, set both start and end to the new entry
    if (start == NULL)
    {
        start = new_entry;
        end = new_entry;
    }

    // If linked list not empty, add entry to the end and update the list
    else
    {
        end->next = new_entry;
        end = new_entry;
    }

    literal_table_invariant(); // Check for valid state

    return ret_ofst; // Return offset of newly added entry
}

// Pre-Conditions: Literal table has been properly initialized
// Post-Conditions: Starts the iteration process for the literal table,
// produces error message if literal table is already iterating
void literal_table_start_iteration()
{
    // Ensure that literal table is not already iterating
    if (currently_iter) bail_with_error("Can't start literal table iteration when already iterating!");

    literal_table_invariant(); // Check for valid state
    currently_iter = true; // Set flag to show we are now iterating
    next_iter = start; // Start iteration at the beginning of the list
}

// Pre-Conditions: Literal table has been properly initalized 
// and is currently iterating
// Post-Conditions: Ends the iteration process for the literal table
void literal_table_end_iteration()
{    
    currently_iter = false; // Set flag to show we have stopped iterating
}

// Pre-Conditions: Literal table has been properly initialized
// and is currently iterating
// Post-Conditions: Checks if there is a next entry in the table,
// returns true if there is, returns false and ends iteration otherwise
bool literal_table_iteration_has_next()
{
    literal_table_invariant(); // Check for valid state

    // Check to see if next entry exists
    bool has_next = (next_iter != NULL);

    // If it doesn't exist, end iteration
    if (!has_next) literal_table_end_iteration();

    return has_next; // Return status of next entry
}

// Pre-Conditions: Literal table has been properly initialized
// Post-Conditions: Returns the integer value of the next entry,
// produces error message if next entry does not exist
int literal_table_iteration_next()
{
    // Check if next entry exists
    if (next_iter == NULL) bail_with_error("Cannot get next literal table entry, does not exist!");

    int ret_val = next_iter->val; // Save value of the entry to return
    next_iter = next_iter->next; // Advance to the next entry
    return ret_val; // Return the entry's value
}