# markov
A Markov Text generator in Nim (v 1.4.2)

Uses markov chains (https://en.wikipedia.org/wiki/Markov_chain) to generate nonsensical but somewhat grammatically valid text from one or more text files.

To build the command line utility:
```
nim --define:MarkovCmd c markov.nim
```
To use as a library just import and don't define "MarkovCmd"

To use the command line utility to generate text:
`markov <options>`

All options have a short  "-r" and a long command "--read" and are followed by an argument

**Markov Order**
A Markov Order is the number of phrases considered to be a "state" in the markov chain.  The higher this is the more like the original text your output will be.

`-o <integer>  or --order <integer>`

**Sentence Count**
This determines the number of sentences generated by the utility.  Default is 500.

`-c <integer> or --count <integer>`

**Load datafile**
This option loads a previously created sqlite database file into the markov generator.

`-l <filename> or --load <filename>`

**Read Files**
This option reads files into the markov generator as sample text.

`-r <textfile> or --read <textfile>`

**Save datafile**
This option (which happens after generating sentences) writes the markov generator to a sqlite datafile.

`-s <filename> or --save <filename>`

**The save option will clear out all data if the datafile exists.**

For corpus, project gutenberg is a good source of text.  Of course just about any grammatically valid text will do.

Have fun.

-David Medlock
