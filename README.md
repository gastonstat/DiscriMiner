DiscriMiner package (version 0.1-20)
============================

This is a very nice package which provides functions for Discriminant Analysis and Classification purposes. Among the covered topics one can find methods for descriptive, geometric, linear, quadratic, PLS, as well as qualitative discriminant analyses.

Requirements and Installation
-----------------------------
*  DiscriMiner (stable version from CRAN)

   install.packages("DiscriMiner")

*  DiscriMiner (latest version from github)

   download.file("https://github.com/downloads/gastonstat/DiscriMiner/DiscriMiner_0.1-20.tar.gz", "DiscriMiner_0.1-20.tar.gz")
   
   install.packages("DiscriMiner_0.1-20.tar.gz", repos=NULL, type="source")


Example Usage
-------------
    > library(DiscriMiner)

    > data(iris)

    > my_model = geoDA(iris[,1:4], iris$Species)

    > my_model


Or check out [www.gastonsanchez.com/discriminer](http://www.gastonsanchez.com/discriminer) for some useful slides.

Links
-----
[DiscriMiner package github](http://github.com/gastonstat/DiscriMiner)

[The R Project](http://www.r-project.org/)

[gaston sanchez](http://www.gastonsanchez.com)


Author Contact
--------------
Gaston Sanchez (gaston.stat at gmail.com)
