### Modules and Imports

Imports:

- `use pie/std;` enables the standard library. This import is handled specially and not inlined into your program.
- `use path/to/module;` inlines `path/to/module.pie` at compile time. Paths are resolved relative to the current file.

Modules:

```pie
module math {
    def int add(int a, int b) { return a + b; }
}

module app {
    def void main() { std::print(math::add(2, 3)); }
}
```

Qualification:

- Use `Module::Name` to refer to items inside modules.
- Member access with `.` is parsed but currently has dynamic/"any" typing; prefer module paths for named items.

Entry point:

- Top-level `def`, `let`, and `struct` are automatically wrapped into an implicit `main` module.
