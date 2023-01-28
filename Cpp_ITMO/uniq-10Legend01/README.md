# uniq command line utility

uniq -- report or filter out repeated lines in a file

The uniq utility reads the specified input_file comparing adjacent lines, and writes a copy of each unique input line to the standard output.
If input_file is a single dash (`-`) or absent, the standard input is read.
The second and succeeding copies of identical adjacent input lines are not written.  Repeated lines in the input will not be detected if
they are not adjacent, so it may be necessary to sort the files first.

```bash
uniq [OPTIONS] [FILE]
```

options:
* `-c, --count` - prefix lines by the number of occurrences
* `-d, --repeated` - only print duplicate lines, one for each group
* `-u, --unique` - only print unique lines

### Example
```bash
$ cat e.txt
Aaa
Aaa
777
BbBb
Bbbb
123
123
Aaa
$ uniq e.txt
Aaa
777
BbBb
Bbbb
123
Aaa
$ uniq -c e.txt
      2 Aaa
      1 777
      1 BbBb
      1 Bbbb
      2 123
      1 Aaa
$ uniq -cd e.txt
      2 Aaa
      2 123
```
