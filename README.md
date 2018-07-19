# dt.nuggets
Some convenience functions using the`data.table` 

There is nothing much to add. Just trying to create some useful `data.table` wrappers reposetry, that's all.

**Installation**

```r
devtools::install_github('davidarenburg/dt.nuggets')
```
**Examples**

The first function in the package is `frollprod` which can roll products by groups (or not) while updating by reference. It can handle `NA`s and do a partial roll too.

```r
DT <- data.table(x = sample(20), y = sample(1:2, 20, replace = TRUE), key = "y")
DT[c(2, 8), x := NA]

frollprod(DT, "x", 3, by = "y", type = "lead", partial = TRUE, na.rm = TRUE)
DT
#      x y x_prod_3
#  1:  2 1        8
#  2: NA 1       52
#  3:  4 1      312
#  4: 13 1     1560
#  5:  6 1      120
#  6: 20 1       20
#  7:  1 1        7
#  8: NA 1       35
#  9:  7 1      595
# 10:  5 1       85
# 11: 17 1       17
# 12: 11 2     1320
# 13: 10 2     1080
# 14: 12 2     1620
# 15:  9 2     2430
# 16: 15 2     5130
# 17: 18 2     4788
# 18: 19 2      798
# 19: 14 2       42
# 20:  3 2        3
```
