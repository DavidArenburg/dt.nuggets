# dt.nuggets
Some convenience functions using the`data.table` 

There is nothing much to add. Just trying to create some useful `data.table` wrappers reposetry, that's all.

**Installation**

```r
devtools::install_github('davidarenburg/dt.nuggets')
```
**Examples**

The first function in the package is `frollprod` which can roll products by groups (or not) while updating by reference.

```r
DT <- data.table(x = sample(10), y = sample(1:2, 10, replace = TRUE), key = "y")
frollprod(DT, "x", 3, by = "y", type = "lead")
DT
#      x y Prod3
#  1:  8 1   480
#  2:  6 1   540
#  3: 10 1   180
#  4:  9 1    NA
#  5:  2 1    NA
#  6:  3 2    84
#  7:  4 2    28
#  8:  7 2    35
#  9:  1 2    NA
# 10:  5 2    NA
```
