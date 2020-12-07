// Playground Session: https://api2.sourceacademy.nus.edu.sg/1m
const tree = value => left => right => list(value, left, right);
const not_defined = "N/A";
const top = tree => is_null(tree) ? null : head(tree);
const get_value = node => is_null(node) ? null : head(node);
const next = node => is_null(node) ? null : tail(node);
const left = tree => is_null(tree) ? not_defined : get_value(next(tree));
const right = tree => is_null(tree) ? not_defined : get_value(next(next(tree)));

// Tests
const a = tree(4)(list(5, 7))(6);
display(top(a));                // 4
display(left(a));               // [5, [7, null]]
display(top(left(a)));          // 5
display(right(a));              // 6
display(left(left(a)));         // 7
display(right(left(a)));        // null 
right(null);                    // "N/A" - Not applicable as right of null is not defined 