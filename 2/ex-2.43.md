# ex-2.43

### Original
Called (n-1) times as the board size increases

```
(flatmap
  (lambda (rest-of-queens)
    (map 
      (lambda (new-row) (adjoin-position new-row k rest-of-queens))
      (enumerate-interval 1 board-size)))
  (queen-cols (- k 1)))
```

### Modified
Called (n ^ (n-1)) times as the board size increases

```
(flatmap
  (lambda (new-row)
    (map 
      (lambda (rest-of-queens) (adjoin-position new-row k rest-of-queens))
      (queen-cols (- k 1))))
  (enumerate-interval 1 board-size))
 ```

