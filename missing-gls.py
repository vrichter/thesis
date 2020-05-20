#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import os
import sys
import re

dir_path = os.path.dirname(os.path.realpath(__file__))

terms = []

def append_fix(term):
    global terms
    terms.append(term.replace('XXXHYPXXX', '-'))
    if 'XXXHYPXXX' in term:
        terms.append(term.replace('XXXHYPXXX', r'\hyp{}'))


with open(os.path.join(dir_path, 'frontback', 'Glossary.tex')) as data:
    data = data.read()

    # for match in re.finditer(r'\\newacronym\{.*?\}\{(.*?)\}',
    #                          data):
    #     terms.append(match.group(1))

    data = data.replace(r'\hyp{}', 'XXXHYPXXX')

    for match in re.finditer(
            r'\\newglossaryentry\{.*?\}\W*\{\W*name=\{(.*?)}[\W.]*(?:plural=\{(.*?)\})?',
            data, flags=re.MULTILINE):
        append_fix(match.group(1))
        if match.group(2):
            append_fix(match.group(2))
        else:
            append_fix(match.group(1) + 's')

    for match in re.finditer(
            r'\\newglossarydualentry\{.*?\}\{(.*?)\}\{(.*?)\}',
            data, flags=re.MULTILINE):
        append_fix(match.group(1))
        if match.group(2):
            append_fix(match.group(2))
        else:
            append_fix(match.group(1) + 's')

# sort terms from longest to shortest so that warnings for substrings are
# avoided
terms = list(reversed(sorted(terms, key=lambda e: len(e))))

with open(sys.argv[1]) as data:
    lines = data.readlines()

for line, text in enumerate(lines, 1):

    # first, mask out all existing correct calls to glossary entries
    text = re.subn(r'\\([aA]cr|[gG]ls).*?\{[\w-]+?\}',
                   lambda m: '±' * len(m.group(0)), text)[0]
    if re.match(r'^\s*\%',text):
        continue # ignore comments

    for term in terms:
        start = 0
        while start >= 0:
            start = text.find(term, start)

            if start >= 0:
                if re.match(
                        r'.*('
                        r'\w|(?# ignore things inside words)'
                        r'\\([aA]cr|([nN]ewdef)?[gG]ls).*?\{|(?# ignore gls calls)'
                        r'\\inidata\{[^}]+\}\{[^}]*|(?# ignore data calls)'
                        r'\\[cCvV]?ref\{[^}]*|(?# ignore references)'
                        r'\\label\{[^}]*|(?# ignore labels)'
                        r'\\newglossaryentry{[^}]*|(?# ignore glossary entries)'
                        r'\s*(name|plural|attributive|first)={+[^}]*|(?# ignore glossary entries)'
                        r'\\(includegraphics|input)(\[[^\]]+\])?\{[^}]*(?# ignore imgs, inputs)'
                        r')$',
                        text[:start]) is None:
                    print('{line}:{col} - {line}:{end_col}: '
                          'Use glossary entry for {term}'.format(
                              line=line,
                              col=start + 1,
                              end_col=start + len(term),
                              term=term))
                    text = text[:start] + '±' * len(term) + text[start +
                                                                  len(term):]

                start = start + len(term)
