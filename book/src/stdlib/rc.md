### Reference Counting and Nulls

- **std::is_null(any) -> bool**: tests if a value is `null`.
- **std::rc::inc_ref(any)** and **std::rc::dec_ref(any)**: low-level refcount adjustments. You typically do not need these in normal code.

Note:

- Many stdlib functions return `null` to indicate failure (e.g., out-of-range, parse errors).
