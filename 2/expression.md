# Symbolic Differentiation

## Specification
Input: an arithmetic expression, a variable
Output: the derivative of the expression with regard to the variable in the simplest form

## Sub-problems and Requirements

### Low level
- data representation of arithmetic expressions:
    - sum
    - product
    - exponentiation
- simplification algorithm for each type of expression

#### What is the "simplest form"?
- merge terms
- merge plain numeric values
- merge products of the same variable

-> Simplification Algorithm:
Input: an arithmetic expression
Output: an equivalent arithmetic expression in the simplest form

Examples:
1 + 2 + 3 = 6
1 + x = x + 1        // reorder terms in sums
1 + x + x = 2x + 1   // merge same variables into products
2x + x = 3x      
1 + (2 + x) = 1 + 2 + x = 3 + x // merge terms
3(x + y) = 3x + 3y
2x + 3(y - 1) + 4 = 2x + 3y - 3 + 4 = 2x + 3y + 1 // recursion...

Algorithm sketch for sums:

1. map simplify over all terms (recursion)
2. filter out null-terms
3. unpack sub-expressions if same operator: +
4. accumulate onto 3 stacks:
    1. running numeric total
        - add
    2. list of variables
        - next is a variable (= a plain symbol or a product of a number and a symbol)
        - factor = product ? multiplier : 1
        - do we have that variable already? (-> dictionary? -> SUB PROBLEM)
            - yes: merge
            - no: append a new entry to the list
    3. other expressions
        - simply collect in a list

ABSTRACTION: dictionary lookup, change existing value
    - dictionary data structure: BST (don't bother balancing it here)
        nodes: key -> symbol, value -> factor OR product; left/right pointers
        how to define an ORDERING among symbols?  -> (symbol<? 'a 'b) -> #t
    - lookup     (map-get map key) -> value or #f (OR 0 in this case?)
    - insertion: (map-set map key value) -> modified map
    - deletion:  (map-delete map key) -> modified map

adding in variables: (map-set variables v (+ (map-get variables v) factor))
(let ((previous-factor (map-get variables v))
  (map-set variables v (+ previous-factor new-factor)))) -> returns the modifed map.

### Top level
Differentiation algorithm
