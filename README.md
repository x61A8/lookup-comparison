# Lookup Comparison
This program compares different methods of checking if a string exists in a set.
The methods compared are:

1. A linear scan of the set, comparing each string to the query string.
2. Computing a hash table of the set, and then checking the query string against the hash table.
3. Computing a trie of the set, and then traversing the trie with the query string.
4. Compiling the trie into a function, and applying the function to the query string.

## Results
Prior to declaring optimizations, the compiled function runtime  was 50% of the runtime trie traversal.
After declaring optimizations, the compiled function runtime decreased to 13% of the runtime trie traversal.

After declaring optimizations, the hash table and compiled function were approximately equivalent.

## Conclusion
In this instance the hash table approach would be preferred due to the simpler implementation.
For a more complex problem, such as a RegEx engine, the technique of runtime compilation can
provide significant performance gains.

## Related Reading
Andrey Kotlarski uses this technique in [this article](https://m00natic.github.io/lisp/manual-jit.html)
to optimize a custom SQL-like query engine.