# Platypus
A toy language built using scala parser combinators

## Example
`fn foo(a) -> {print a foo(a)} let x = 2 in { foo(x)}`

output:

```
2
2
2
2
2
2
2
2
2
2
[error] (run-main-0) java.lang.StackOverflowError
java.lang.StackOverflowError
```
