# LLVM-Interval-Analyzer

## What is this?

This repo contains a toy interval (static) analyzer targeting the LLVM IR.

## Installation and Usage

### Install Stack

```$ curl -sSL https://get.haskellstack.org/ | sh```

### Install packages

```$ stack install llvm-hs optparse-applicative ieee754```

### Compile

```$ stack ghc main.hs```

### Run

```$ ./Main -i {your input .ll file}```

## Extension plan (TODOs)

### Achieved

1. Top-down interprocedural analysis

### Not Yet Achieved

1. Extending domain to include access paths
