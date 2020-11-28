# LLVM-Interval-Analyzer

## What is this?

This repo contains a toy interval (static) analyzer targeting the LLVM IR.

## Installation and Usage

### Install Stack

```$ curl -sSL https://get.haskellstack.org/ | sh```

### Install packages

```$ stack install aeson llvm-hs optparse-applicative```

### Compile

```$ stack ghc main.hs```

### Run

```$ main -i {your input .ll file}```

## Extension plan (TODOs)

### Achieved

1. Top-down interprocedural analysis
2. Extending domain to include access paths

### Not Yet Achieved
