### Getting Started

Prerequisites:

- Rust toolchain (stable)
- LLVM 17 installed and discoverable by the `inkwell` crate
  - On some systems you may need to set `LLVM_SYS_170_PREFIX` to your LLVM install prefix

Build and run from the Pie crate directory:

```bash
cd pie/pie
cargo run --release -- ../examples/working_example.pie
```

You should see output similar to:

- Executing program...
- Program finished with exit code: 0

Run another example (HTTP server):

```bash
cargo run --release -- ../examples/http_test.pie
```

Then open `http://localhost:8080/` in your browser.

CLI usage:

```text
pie <file.pie>
```

Optimization:

- Pass `-O`, `-O1`, `-O2`, or `-O3` as an extra argument to select JIT optimization level.
