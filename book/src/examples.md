### Examples

See the repository `examples/` directory. A few highlights:

- `working_example.pie`:

```pie
use pie/std;
use lib/math;

module demo {
    struct Person { name: string, age: int }
}

def void main() {
    std::print("square(7) from lib/math: " + lib::math::square(7));
    let Person p = demo::Person { name: "Ada", age: 37 };
    std::print(p.name);
}
```

- `http_test.pie`: minimal web server with route map.
