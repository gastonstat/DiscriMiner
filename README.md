# `"DiscriMiner"`

`DiscriMiner` is an R package that provides functions for Discriminant Analysis and Classification purposes. Among the covered topics one can find methods for descriptive, geometric, linear, quadratic, PLS, as well as qualitative discriminant analyses.


## Motivation

Among my personal library of books about data analysis and multivariate statistics, I have a small collection of French textbooks. These are awesome books, very didactic, with clear explanations, good examples, and lots of graphics. They all share the underlying spirit of the French school known as *Analyse des Données*, something similar -but not identical- to the [Exploratory Data Analysis](http://en.wikipedia.org/wiki/Exploratory_data_analysis) approach of (John Tuckey)[http://en.wikipedia.org/wiki/John_Tukey]. 

One drawback is that these books are written in French with, sadly, just some random limited editions in English. But the real problem (at least for me) was the lack of software availability for performing discriminant analysis in the way they are described in my books. Such was my frustration and annoyance, that I decided to create `DiscriMiner` in order to have software that allowed me to perform discriminant analysis, as well as getting the results, in the way they are explained in my french textbooks.


## Installation

Stable version [CRAN](http://cran.r-project.org/web/packages/DiscriMiner/index.html)
```ruby
# stable version
install.packages("DiscriMiner")
```

Development version on [github](https://github.com/gastonstat/DiscriMiner)
```ruby
# development version
library(devtools)
install_github('DiscriMiner',  username='gastonstat')
```

## Some Examples
```ruby
# load package
library(DiscriMiner)

# laod dataset
data(iris)

# apply geometric predictive discriminant analysis
my_model = geoDA(iris[,1:4], iris$Species)

# show me the model
my_model
```

## Acknowledgements

Many thanks to Charles Determan for his contributions to the function `plsDA`


## Author Contact

[www.gastonsanchez.com](http://www.gastonsanchez.com)

Gaston Sanchez (`gaston.stat at gmail.com`)
