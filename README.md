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
    let mut hi_text: str = "hi world?";
    hi_text = "Hello World!";
    if ?hi_text { // Not worth printing if its empty
        print(hi_text);
    }
}
```
