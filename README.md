# barely

- barely a programming language
- barely a compiler

### Principles

1. Assembly output should be easily predicted from the source code, no magic
2. Types are a lie

### Examples

```
macro print(s) = write(1, s, len(s))

print("hello world\n")
exit(0)
```

```
import printf

func fibh(a, b, n) =
  if n == 0
    then a
    else fibh(b, a+b, n-1)

macro fib(n) = fibh(1, 1, n)

out = fib(10)
printf("%d\0", out)
exit(0)
```
