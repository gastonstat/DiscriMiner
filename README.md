R package DiscriMiner
============================

This is a very nice package which provides functions for Discriminant Analysis and Classification purposes. Among the covered topics one can find methods for descriptive, geometric, linear, quadratic, PLS, as well as qualitative discriminant analyses.

Requirements and Installation
-----------------------------
DiscriMiner (stable version from CRAN)
```
install.packages("DiscriMiner")
```
DiscriMiner (latest version from github)
```
install.packages("devtools") 
library(devtools)
install_github('DiscriMiner',  username='gastonstat')
```

Example Usage
-------------
```
# load package
library(DiscriMiner)

# laod dataset
data(iris)

# apply geometric predictive discriminant analysis
my_model = geoDA(iris[,1:4], iris$Species)

# show me the model
my_model
```

More info at [www.gastonsanchez.com/discriminer](http://www.gastonsanchez.com/discriminer)

Links
-----
[DiscriMiner package github](http://github.com/gastonstat/DiscriMiner)

[DiscriMiner slides](http://www.gastonsanchez.com/discriminer)


Author Contact
--------------
Gaston Sanchez (gaston.stat at gmail.com)
