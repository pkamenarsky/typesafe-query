# typesafe-query

Typesafe query expressions.

```
data P = P1 { stringField :: String, intField :: Int }
       | P2 { stringField :: String, boolField :: Bool, pField :: P }
       | P3 String

mkTSEntities False ''P
```

```
ghci> :t (_stringField `eq` "asd") `qand` (_intField `grt` 7)
(_stringField `eq` "asd") `qand` (_intField `grt` 7) :: Query P
```
