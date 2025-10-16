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
- [x] `func`: specifies a function or function signature
- [x] `let`: specifies an immutable variable
- [x] `const`: specifies a compile time constant, only allowed in global scope
- [x] `macro`: specifies a compile time macro (code replacement), only allowed in global scope
- [ ] `mut`: marks a variable or refrence as mutable
- [x] `return`: returns from a function
- [x] `if`: if-statement
- [x] `else`: else-statement
- [ ] `switch`: switch-statement

#### Type Keywords
- [x] `i16`
- [x] `i32`
- [x] `f32`
- [x] `f64`
- [x] `bool`
- [x] `byte`
- [x] `str`
- [ ] `struct`
- [ ] `enum`

#### Reserved Keywords
These are reserved, there is currently no plan to implement them

- `async`
- `import`

### Unary Prefix Operators
- [x] `?`: Logical Operator, Evaluates "truthness" of another type (uses kOS's `OpcodeLogicToBool` in `src/kOS.Safe/Compilation/Opcode.cs`)
- [ ] `&`: Refrence
- [ ] `*`: Derefrence
- [x] `!`: Logical Not
- [x] `-`: Arithmetic Negation

### Binary Infix Operators
- [x] `+`: Addition
- [x] `-`: Subtraction
- [x] `*`: Multiplication
- [x] `\`: Division
- [x] `&&`: Boolean And
- [x] `||`: Boolean Or
- [x] `==`: Equality
- [x] `!=`: Inequality
- [x] `>`: Greater than
- [x] `<`: Less than
- [x] `>=`: Greater than or equals
- [x] `<=`: Less than or equals

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
