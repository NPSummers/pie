### Language Tour

Basics:

- Declarations: `let <type> <name> = <expr>;`
- Functions: `def <ret> <name>(<type> <arg>, ...) { ... }`
- Modules: `module Name { ... }` (nestable)
- Imports: `use path/to/file;` and `use pie/std;`

Types:

- Built-ins: `int`, `float`, `bool`, `string`, `list`, `map`, `void`
- Structs:

```pie
module demo {
    struct Person { name: string, age: int }
}
```

Expressions and statements:

- Literals: numbers, strings "...", chars 'a', lists `[1, 2]`, maps `{ "k": 1, "v": 2 }`
- Arithmetic and comparisons: `+ - * / % == != < <= > >= && || & | ^`
- Assignment and compound assignment: `= += -= *= /=`
- Control flow: `if { ... } elif { ... } else { ... }`, `while { ... }`, `for x in <iter> { ... }`
- Calls and qualification: `std::print("hi")`, `lib::math::square(7)`

Example:

```pie
use pie/std;

module demo { struct Person { name: string, age: int } }

def void main() {
    let int x = 7 * 7;
    std::print("square(7): " + x);
    let Person p = demo::Person { name: "Ada", age: 37 };
    std::print(p.name);
}
```

Iteration:

```pie
use pie/std;

def void main() {
    let any it = std::iter::range(0, 5, 1);
    for i in it { std::print(i); }
}
```

Notes:

- Strings + numbers concatenate via `+`.
- Many stdlib operations return `null` on error; check with `std::is_null(x)`.
