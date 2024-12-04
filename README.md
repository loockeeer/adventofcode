# Advent Of Code

These are my solutions for the [Advent Of Code](https://adventofcode.com) annual event, along with my custom-made auto-downloader program that allows me to quickly setup my ocaml workflow. The latter can be found under the `auto_setup` folder.
Since I do these for fun, please note that I sometimes avoid using Stdlib overly powerful tools, likes Str.regexp. They kind of ruin the fun.
## Auto Setup
The name is pretty obvious : it automatically setups my environment for any Advent Of Code day.
Although the code is **utterly messy**, it works and I can't bother making it any cleaner. On top of this issue, I made it specifically for my preferences, and hence the boilerpalte `dune-project` and `.ocamlformat` are not editable.
A further improvement could be making this project:
- Language agnostic; which would require making the boilerplate(s) customizable (preferably in ~/.config, or in the adventofcode folder itself idk) and could lead to the possibility of doing each day in a different language, and submit many solutions
- Cleanier; the code is messy, not well separated and not commented at all

## Note on the licence
I am very unsure about who the inputs belong to, and forgot to remove them from the project (I would also like to keep them here).
To be clear, every file named `input.txt` or `input_test.txt` should not be considered part of this project, nor do they fall under the licence (CC0) used in this project.
They, instead, fall under Advent Of Code full property, rights or whatever.
