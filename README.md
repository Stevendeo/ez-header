# EZ-HEADER

## What is it?

This simple tool removes all headers from your ocaml files and adds
yours from a given text file.

## Usage

First, write your header in a text file (for example, HEADER.txt). Then, simply
call ez-header:

ez-header -H HEADER.txt file1 file2 ... 

## How to build

You need OCaml version at least 4.14.0. This project has no other dependency. 
Just call `make build` and you should be good. 

Dune afficionados can also call `dune build`.
