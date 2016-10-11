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


## Example 2

```
  fn foo() -> { 3 }
  let x = (5 + foo())
    in {
        print x
            }
```

output:
`Num(8.0)`
