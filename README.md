# kLang
---

kerboscript was once the only language for kOS. Then, with kRISC came `.ksm`. Now its time to make `.ksm` a compile target with kLang.

This goal of this project is to replace kerboscript with an actual (somewhat) modern programming language, while adhering to a subset of kRISC to be able to make it an actual **REDUCED** Instruct Set Computer.

## Syntax
Here is some mock kLang code:

```kLang
const ZERO: i32 = 0i32;

type MY_STRUCT: struct {
    name: str,
    index: i32
};

func main: (vessel: Vessel /* Vessel holds vessel data */) -> () {
    let mut hi_text: str = "hi kerbin?";
    hi_text = "Hello kOS!";
    if ?hi_text { // Not worth printing if its empty
        print(hi_text);
    }
}
```

### Keywords
- `func`: specifies a function or function signature
- `let`: specifies an immutable variable
- `const`: specifies a compile time constant, only allowed in global scope
- `macro`: specifies a compile time macro (code replacement), only allowed in global scope
- `mut`: marks a variable or refrence as mutable
- `return`: returns from a function
- `if`: if-statement
- `else`: else-statement
- `switch`: switch-statement

#### Type Keywords
- `i16`
- `i32`
- `f32`
- `f64`
- `bool`
- `byte`
- `str`
- `struct`
- `enum`

#### Reserved Keywords
These are reserved, there is currently no plan to implement them

- `async`
- `import`

### Unary Prefix Operators
- `?`: Logical Operator, Evaluates "truthness" of another type (uses kOS's `OpcodeLogicToBool` in `src/kOS.Safe/Compilation/Opcode.cs`)
- `&`: Refrence
- `*`: Derefrence
- `!`: Logical Not
- `-`: Arithmetic Negation

### Binary Infix Operators
- `+`: Addition
- `-`: Subtraction
- `*`: Multiplication
- `\`: Division

### Number Literals
Integer and byte literals may begin with `0x` to indicate hexadecimal parsing

### String Literals
String literals must be surrounded with `"` characters

#### String Escape Sequences
- `\n`: Newline
- `\t`: Tab
- `\r`: Carriage Return
- `\0`: Nullbyte
- `\3`: Hex `0x03`, to print ksm argument headers
- `\\`: `\` character
- `\"`: `"` character
