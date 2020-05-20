# -*- coding: utf-8 -*-
# this script reads an elan file and prints annotations in csv format
from __future__ import print_function
import sys
import re
import pympi
import pprint
from os import path
from collections import defaultdict
import sys

# config
reference_tier = 'Wizard'
reference_tier_filter = 'Aufgabe'
extra_columns = ['Filename', 'Vp_num', 'Order', 'Condition', 'Annotator']
regex_filename = re.compile(r'study([0-9])_11_(.*)-(shiftedAnnotations[^.]*)?.eaf$')
regex_wizard = re.compile(r'Aufgabe ([1-7]) (.+)')

annotations = defaultdict(list)
tiers = []


# helpers
def eprint(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)


def dump(data):
    result = '\t'.join(data)
    print(result.encode('utf-8'))


def collect_tiers(document, tiers):
    for i in document.tiers.keys():
        if i not in tiers:
            tiers.append(i)


def consistent_overlap(tier_annotation):
    for i in tier_annotation:
        if i[2] != tier_annotation[0][2]:
            return False
    return True


def data_from_name(filename):
    match = re.search(regex_filename, filename)
    assert (match)
    assert (len(match.groups()) == 3)
    return [
        ('filename', match.group(0)),
        ('trial', match.group(1)),
        ('date', match.group(2)),
        ('annotation_modifier', match.group(3))
    ]


def process_annotation(tier, data):
    callbacks = {
        'Filename': data_from_name,
    }
    if tier in callbacks:
        return callbacks[tier](data)
    else:
        return [(tier, data)]


def add_annotations(annotations, data):
    if type(data) == tuple:
        assert (len(data) == 2)
        annotations[data[0]].append(data[1])
    if type(data) == list:
        for i in data:
            assert (type(i) == tuple)
            assert (len(i) == 2)
            annotations[i[0]].append(i[1])


def collect_all_dialog_acts(document):
    dialog_tiers = ['/meka/dialogact/dialogActInformer/', '/meka/rejected/dialogact/dialogActInformer/']
    result = []
    for tier in dialog_tiers:
        for start, end, annotation in document.get_annotation_data_for_tier(tier):
            result.append((start, end, annotation))
    filtered = []
    last = None
    for r in sorted(result, key=lambda x: x[1]):
        if last is not None:
            diff = r[0]-last[0]
            if diff < 30: # simultaneous recognitions, diff < 28ms are nameRequest/timeRequest bug
                eprint('skipping simultaneous dialogs. diff = {}, replacing {} with {}'.format(diff,filtered[-1],r))
                filtered[-1] = r # take the latter dialog act, nameRequest is usually faster
            else:
                filtered.append(r)
                last = r
        else:
            last = r
    return filtered


def concat_annotations(ann):
    values = []
    for v in sorted(ann, key=lambda a: a[0]):
        values.append(v[2])
    return '<=>'.join(values)


def find_exact_start_annotation(tier, document, start):
    tier_annotation = document.get_annotation_data_at_time(tier, start)
    result = []
    for a in tier_annotation:
        if a[0] == start:
            result.append(a[2])
    return '<x>'.join(result)


def find_meka_addressee_annotation(tier, document, start):
    tier_annotation = document.get_annotation_data_between_times(tier, start, start+20)
    result = []
    for a in tier_annotation:
        # nameRequests can all safely be ignored as they stem from an error in the nlu and are ignored
        if a[2] != 'nameRequest':
            result.append(a[2])
    return '<x>'.join(result)


def find_annotation_nearest_start_dialog(tier, document, start):
    def dist(a):
        return abs(a[0]-start)
    delta = 50
    tier_annotation = document.get_annotation_data_between_times(tier, start-delta, start+delta)
    nearest = None
    for a in tier_annotation:
        if a[2] == 'nameRequest' or dist(a) > delta:
            continue
        if (nearest is None) or (dist(a) < dist(nearest)):
            nearest = a
    if nearest is not None:
        return nearest[2]
    else:
        return ""

def find_annotation_nearest_start(tier, document, start):
    def dist(a):
        return abs(a[0]-start)
    tier_annotation = document.get_annotation_data_between_times(tier, start-50, start+50)
    nearest = None
    for a in tier_annotation:
        if a[2] == 'nameRequest':
            continue
        if (nearest is None) or (dist(a) < dist(nearest)):
            nearest = a
    if nearest is not None:
        return nearest[2]
    else:
        return ""


def find_annotation_last_before(tier, document, start):
    tier_annotation = document.get_annotation_data_for_tier(tier)
    result = []
    nearest = None
    if start == 147795:
        eprint(tier, len(tier_annotation))
        for a in tier_annotation:
            if a[0] <= start:
                eprint(a)
    for a in tier_annotation:
        if a[2] == 'nameRequest' or a[0] > start:
            continue
        if (nearest is None) or ((start-a[0]) < (start-nearest[0])):
            nearest = a
    if nearest is not None:
        return nearest[2]
    else:
        return ''

def find_proper_annotation(tier, document, start):
    callbacks = {
        '/meka/dialogact/dialogActInformer/': find_exact_start_annotation,
        '/meka/rejected/dialogact/dialogActInformer/': find_exact_start_annotation,
        'meka-addressee': find_annotation_nearest_start,
        '/meka/eval/all/dialogact/': find_annotation_nearest_start_dialog,
        '/meka/eval/gaze/dialogact/': find_annotation_nearest_start_dialog,
        '/meka/eval/mouth/dialogact/': find_annotation_nearest_start_dialog,
        '/meka/addressee-meka/scatter/Person0_Mouth/': find_annotation_last_before,
        '/meka/addressee-meka/scatter/Viewing_Person0_Meka/': find_annotation_last_before,
        '/meka/addressee-meka/scatter/Addressee/': find_annotation_last_before
    }
    if tier in callbacks:
        return callbacks[tier](tier, document, start)
    else:
        tier_annotation = document.get_annotation_data_between_times(tier, start, start+20)
        if len(tier_annotation) == 0:
            return ""
        elif len(tier_annotation) == 1 or consistent_overlap(tier_annotation):
            return tier_annotation[0][2]
        else:
            return concat_annotations(tier_annotation)

# load all documents
documents = []
for filename in sys.argv[1:]:
    document = pympi.Elan.Eaf(filename)
    collect_tiers(document, tiers)
    documents.append((filename, document))

for filename, document in documents:
    filename_annotation = process_annotation('Filename', filename)
    eprint(filename_annotation)
    for start, end, annotation in collect_all_dialog_acts(document):
        add_annotations(annotations, filename_annotation)
        add_annotations(annotations, ('start_time', str(start)))
        # get content for all other tiers
        for tier in tiers:
            if tier not in document.tiers:  # this annotation does not have this tier
                add_annotations(annotations, process_annotation(tier, ""))
            else:
                add_annotations(annotations, process_annotation(tier,find_proper_annotation(tier, document, start)))


def clean_tabs(array):
    result = []
    for val in array:
        result.append(val.replace('\t', ''))
    return result

# convert to row based array
cols = len(annotations.keys())
rows = len(annotations['filename'])
eprint(cols, rows)
data = []
for key, value in annotations.iteritems():
    assert (len(value) == rows)
    data.append(clean_tabs([key] + value))
data = zip(*data)

# print
for row in data:
    print('\t'.join(row).encode('utf-8'))
