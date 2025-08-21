### IO

- **std::print(any val)**: prints to stdout.
- **std::print_err(any val)**: prints to stderr.
- **std::args() -> list**: returns CLI args as a list of strings.

Filesystem:

- **std::fs::read(string path) -> string | null**: reads a file. Returns `null` on error.
- **std::fs::write(string path, string contents) -> bool**: writes a file. Returns `true` on success.

Example:

```pie
use pie/std;

def void main() {
    std::print("Hello, world");
    let list args = std::args();
    std::print(args);
}
```
