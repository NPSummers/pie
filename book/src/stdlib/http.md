### HTTP

Methods:

- **std::http::get() -> string** ("GET"), similarly `post`, `put`, `delete`.

Requests:

- **std::http::request(string method, string url, map headers, string body) -> string**
  - Returns response body or an error string like `"http error: ..."`.

Servers:

- **std::http::serve(int port, string body)**: serves constant body on all routes.
- **std::http::serve_routes(int port, map routes)**: dynamic routing via a map.
  - Route values can be a string body or a map with keys: `status` (int), `headers` (map), `body` (string).
  - Route keys: `"GET /"`, or just `"/"` as fallback. Trailing slashes are normalized.

Example:

```pie
use pie/std;

def void main() {
    let map routes = std::map::new();
    let map home = std::map::new();
    std::map::set(home, "status", 200);
    std::map::set(home, "body", "Hello from PIE");
    std::map::set(routes, "GET /", home);
    std::http::serve_routes(8080, routes);
}
```
