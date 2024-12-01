# FreakingOoleh Language

FreakingOoleh is a transpiler which takes a simplified and limited subset of C and transpiles
it into the absolute shittiest, incomplete, buggiest and ugliest "programming"
language (absolute crap) there has ever been.

## Built-in Data Types

| Type   | Description                      | Size    |
|--------|----------------------------------|---------|
| byte   | Integer value between 0 and 255  | 1 Byte  |
| int    | Integer value between 0 and 2^16 | 2 Bytes |
| string | String type                      | n Bytes |

### Arrays

Arrays can have a fixed or dynamic length but must only contain data types with known
internal sizes at compile time. This means, strings can not be placed into arrays but
2-dimensional array's of fixed sized `char`'s will work.

```js

```

## Functions

- Functions will be transpiled as inlined. 

## Built-in Functions

### CharsToString(char[])

Converts an array of `char`'s to a `string`.

### `T`.ToString()