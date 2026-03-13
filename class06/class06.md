# Class06: R Functions
Cyrus Shabahang (PID: A19145663)

- [Background](#background)
- [A first function](#a-first-function)
- [A second function](#a-second-function)
- [A new cool function](#a-new-cool-function)

## Background

Functions are at the heart of using R. Everything we do involves calling
and using functions (from data input, analysis to results output).

All functions in R have at least 3 things:

1.  A **name** the thing we use to call the function.
2.  One or more input **arguments** that are comma seperated
3.  The **body**, lines of code between curly brackets {} that does the
    work of the function.

## A first function

We are going to write a function to add some numbers:

``` r
add <- function(x) {
  x + 1
}
```

Let’s try it out now

``` r
add(100)
```

    [1] 101

Will this work?

``` r
add(c(100, 200, 300))
```

    [1] 101 201 301

Modify the original function to be more useful and add more than just 1

``` r
add <- function(x, y=1) {
  x + y
}
```

``` r
add(100, 10)
```

    [1] 110

``` r
log(10, base=10)
```

    [1] 1

``` r
add(100)
```

    [1] 101

> **N.B** Input agrguments can be either **required** or **optional**.
> The later have a fall-back default that is specified in the function
> code with an equals sign.

``` r
#add(100, 200, 300)
```

## A second function

All functions in R look like thi s

    name <- function(arg) {
      body
    }

The `sample()` function in R randomly selects a specified number of
items from a given set of values, with or without replacement.

> Q. Return 12 numbers picked randomly from the input 1:10

``` r
sample(1:10, size=12, replace = TRUE)
```

     [1] 10  4  3  1  5  9  3  8  3  5  6  3

> Q. Write the code to generate a 12 nucleotide long DNA sequence?

``` r
sample(c("A", "C", "G", "T"), size=12, replace = TRUE)
```

     [1] "A" "A" "A" "C" "G" "G" "C" "A" "T" "A" "C" "T"

> Q. Write a first version function called `generate_dna()` that
> generates a user specified length `n` random DNA sequence?

``` r
generate_dna <- function(n) {
  sample(c("A", "C", "G", "T"), size=n, replace = TRUE) 
  
}
```

``` r
generate_dna(10)
```

     [1] "C" "C" "A" "T" "A" "T" "A" "C" "A" "C"

> Q. Modify your function to return a FASTA-like sequence so rather
> than: \[1\] “T” “A” “G” “T” “C” “A” “T”, we want “TAGTCAT”.

``` r
generate_dna <- function(n) {
  bases <- c("A", "C", "G", "T")
  ans <- sample(bases, size=n, replace=TRUE)
  ans <- paste(ans, collapse= "")
  return(ans)
}
```

``` r
generate_dna(10)
```

    [1] "CGCATATCTC"

> Q. Give the user an option to return FASTA format output sequence or
> standard multi-element vector format?

``` r
generate_dna <- function(n=6, fasta=TRUE) {
  bases <- c("A", "C", "G", "T")
  ans <- sample(bases, size=n, replace=TRUE)
  
  if(fasta) {
    ans <- paste(ans, collapse= "")
    cat("Hello there...")
  }
  else {
    cat("General Kenobi")
  }
  return(ans)
}
```

``` r
generate_dna(10)
```

    Hello there...

    [1] "CCAACGGCTG"

``` r
generate_dna(10, fasta= F)
```

    General Kenobi

     [1] "C" "T" "T" "C" "G" "G" "A" "C" "G" "T"

## A new cool function

> Q. Write a function called `generate_protein()` that generates a user
> specified length protein sequence in FASTA-like format?

``` r
generate_protein <- function(n, fasta = TRUE) {
  aa <- c("A","R","N","D","C","E","Q","G","H","I",
  "L","K","M","F","P","S","T","W","Y","V")
  ans <- sample(aa, size=n, replace=TRUE)
  ans <- paste(ans, collapse = "")
  return(ans)
}
```

``` r
generate_protein(20)
```

    [1] "IYAGWRDEDRCWRPWDESKF"

> Q. Use your new `generate_protein()` function to generate sequences
> between length 6 and 12 amino-acids in length and check if any of
> these are unique in nature (i.e. found in the NR database at NCBI)?

``` r
generate_protein <- function(n, fasta = TRUE) {
  aa <- c("A","R","N","D","C","E","Q","G","H","I",
  "L","K","M","F","P","S","T","W","Y","V")
  ans <- sample(aa, size=n, replace=TRUE)
  ans <- paste(ans, collapse = "")
  return(ans)
}
for(i in 6:12) {
  cat(">", i, sep="", "\n")
  cat(generate_protein(i), "\n")
}
```

    >6
    AIFDQI 
    >7
    LSHEYWD 
    >8
    RLPLYMYA 
    >9
    MNWPSENFT 
    >10
    FRSNAEFCTM 
    >11
    EAPDTMTWVHT 
    >12
    QGYLNGIGKQSF 
