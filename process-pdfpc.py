#!/usr/bin/env python
import sys
import re

notes = False
slide = ''
note = ''

def pretty_print_note(line):
    result = line
    result = re.sub('\\\item','\n', result)
    result = re.sub('\\\(begin|end)\s+{itemize}(\[[^]]+\])?','', result)
    result = re.sub('\\\\\\\\','\n', result)
    print(result)


for line in sys.stdin:
    line = line[:-1]
    if not notes:
        print(line)
        if re.match(r'\[notes\]',line):
            notes = True
            print('### 1')
        continue
    fmatch = re.match(r'### ([0-9]+)',line)
    if fmatch:
        if line != slide:
            print('### {}'.format(int(fmatch.group(1))+1))
            slide = line
            note = line
        continue
    if line != note:
        pretty_print_note(line)
        note = line



