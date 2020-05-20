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
extra_columns = ['Filename', 'Vp_num', 'Order', 'Condition', 'Annotator' ]
regex_filename = re.compile(r'VP([0-9]+)_F?RF?([12])-(nonverbal|verbal)[_\.](.+)\.eaf$')
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
    assert(match)
    assert(len(match.groups()) == 4)
    return [
            ('Filename', match.group(0)),
            ('Vp_num', match.group(1)),
            ('Order', match.group(2)),
            ('Condition', match.group(3)),
            ('Annotator', match.group(4))
            ]

def data_from_wizard(data):
    match = re.search(regex_wizard, data)
    assert(match)
    assert(len(match.groups()) == 2)
    return [
            ('Wizard', match.group(0)),
            ('Wizard task', match.group(1)),
            ('Wizard addressee', match.group(2))
            ]

def reduced_addressee(data):
    reduced = {
            # diese sind zu spezifisch und selten
            u'Bildschirm (Wohnzimmer, Fensterbank)': u'Bildschirm (Wohnzimmer)',
            u'Bildschirm (Wohnzimmertisch)': u'Bildschirm (Wohnzimmer)',
            u'Bildschirm (Wohnzimmerwand)': u'Bildschirm (Wohnzimmer)',
            u'Einrichtungsgegenstände des Apartments': 'Teile des Apartments',
            u'Schalterblock (Aparmenteingang)': 'Teile des Apartments',
            u'Schalterblock (Apartmenteingang)': 'Teile des Apartments',
            u'Schalterblock (Küche, Richtung Kühlschrank)': 'Teile des Apartments',
            u'Schalterblock (Wohnzimmer, an Stehlampe)': 'Teile des Apartments',
            u'Schalterblock (Wohnzimmer, Richtung Küche)': 'Teile des Apartments',
            u'Schalter (Elektrogeräte Wohnzimmer)': 'Teile des Apartments',
            u'Schalter (Elektrogeräte, Wohnzimmer)': 'Teile des Apartments',
            u'Schiebetür (Küche)': 'Teile des Apartments',
            # dies passiert nur in 2 fällen wo 'räume' angesprochen werden
            u'Apartment als Ganzes': 'Teile des Apartments'
            }
    reduced = {
            u'Apartment als Ganzes':                       'Teile des Apartments',
            u'Bildschirm (Apartmenteingang)':              'Teile des Apartments',
            u'Bildschirm (Küche, auf Arbeitsfläche)':      'Teile des Apartments',
            u'Bilschirm (Küche, auf Arbeitsfläche)':       'Teile des Apartments',
            u'Bildschirm (Wohnzimmer, Fensterbank)':       'Teile des Apartments',
            u'Bildschirm (Wohnzimmertisch)':               'Teile des Apartments',
            u'Bildschirm (Wohnzimmerwand)':                'Teile des Apartments',
            u'Einrichtungsgegenstände des Apartments':     'Teile des Apartments',
            u'Kleine Bildschirme im Flur':                 'Teile des Apartments',
            u'Schalterblock (Aparmenteingang)':            'Teile des Apartments',
            u'Schalterblock (Apartmenteingang)':           'Teile des Apartments',
            u'Schalterblock (Küche, Richtung Kühlschrank)':'Teile des Apartments',
            u'Schalterblock (Wohnzimmer, an Stehlampe)':   'Teile des Apartments',
            u'Schalterblock (Wohnzimmer, Richtung Küche)': 'Teile des Apartments',
            u'Schalter (Elektrogeräte Wohnzimmer)':        'Teile des Apartments',
            u'Schalter (Elektrogeräte, Wohnzimmer)':       'Teile des Apartments',
            u'Schiebetür (Küche)':                         'Teile des Apartments',
            u'Tür des Paketschranks (Küche)':              'Teile des Apartments',
            u'Weiße Lautsprecherbox (auf Küchenschrank)':  'Teile des Apartments',
 
            }
    return reduced.get(data,data)
 
def data_from_addressee(data):
    return [
            (u'Adressat final', data),
            (u'Adressat final (reduziert)', reduced_addressee(data))
            ]

def data_from_foa(data):
    return [
            (u'Aufmerksamkeitsfokus', data),
            (u'Aufmerksamkeitsfokus (reduziert)', reduced_addressee(data))
            ]

def process_annotation(tier, data):
    callbacks = {
            'Filename': data_from_name,
            'Wizard': data_from_wizard,
            'Adressat final': data_from_addressee,
            'Aufmerksamkeitsfokus': data_from_foa
            }
    if tier in callbacks:
        return callbacks[tier](data)
    else:
        return [(tier, data)]

def add_annotations(annotations,data):
    if type(data)==tuple:
        assert(len(data) == 2)
        annotations[data[0]].append(data[1])
    if type(data)==list:
        for i in data:
            assert(type(i) == tuple)
            assert(len(i) == 2)
            annotations[i[0]].append(i[1])

def check_time_of_action(document, time, filename):
    # using this as reference
    tier = 'Studienverlauf Detail'
    # return last ended if near enough.
    tier_annotation = filter(
            lambda e: e[2]==u'L\xf6sungsversuch',
            sorted(document.get_annotation_data_for_tier(tier),
                key= lambda e: e[1])
            )
    #eprint(pprint.pformat(tier_annotation))
    assert(len(tier_annotation) >= 1)
    # find the last before or in the best case the parallel
    last  = None
    for a in tier_annotation:
        if a[0] <= time <= a[1]:
            return time # found exact match
        elif last is None:
            last = a
        elif last[1] <= a[1] <= time:
            last = a
    #eprint(filename, time, last, document.get_annotation_data_at_time('Wizard',time))
    return last[1]

# load all documents
documents = []
for filename in sys.argv[1:]:
    document = pympi.Elan.Eaf(filename)
    collect_tiers(document,tiers)
    documents.append((filename,document))


for filename, document in documents:
    filename_annotation = process_annotation('Filename',filename)
    # iterate over all annotations in wizard tier
    for start, end, annotation in document.get_annotation_data_for_tier(reference_tier):
        if not annotation.startswith(reference_tier_filter):
            continue
        add_annotations(annotations,filename_annotation)
        # check and adapt start time. sometimes the wizard action is some millis after the action
        start2 = check_time_of_action(document,start,filename)
        if start2 != start:
            add_annotations(annotations,('Time_adapted',str(start-start2)))
            start = start2-10 # reduce a little bit to prevent overlapping of next annotations
        else:
            add_annotations(annotations,('Time_adapted','0'))
        # get content for all other tiers at start time
        for tier in tiers:
            if tier not in document.tiers: # this annotation does not have this tier
                add_annotations(annotations,process_annotation(tier,""))
            elif tier == reference_tier:
                add_annotations(annotations,process_annotation(tier,annotation))
            else:
                tier_annotation = document.get_annotation_data_at_time(tier,start)
                if len(tier_annotation) == 0:
                    add_annotations(annotations,process_annotation(tier,""))
                elif len(tier_annotation) == 1 or consistent_overlap(tier_annotation):
                    add_annotations(annotations,process_annotation(tier,tier_annotation[0][2]))
                else:
                    eprint("tier annotation '" + tier + "' expected to return a single result",tier_annotation)
                    eprint("will use the earlier annotation")
                    add_annotations(annotations,process_annotation(tier,sorted(tier_annotation, key=lambda a: a[1])[0][2]))


def clean_entry(elem):
    # missing opening and closing quotations
    e = elem
    if (e.count('"') % 2) != 0:
        if e[-1] == '"':
            e = '"'+e
        else:
            e = e+'"'
    # points the end of an entry somehow join the row in libreoffice
    if e.endswith('".'):
        e = e[:-2]+'."'
    if e.endswith('.'):
        e = e[:-1]
    return e.replace('"','__')

def translate(dictionary, key):
    return dictionary.get(key,key)

def translate_key(key):
    dictionary = {
            u'Adressat final': 'Addressee final',
            u'Apartment Anruf': 'Apartment call',
            u'Apartment Paket': 'Apartment parcel',
            u'Apartment Tuer': 'Apartment door state',
            u'Apartment Uhrzeit': 'Apartment time',
            u'Aufmerksamkeitsfokus': 'Focus of attention',
            u'Ausdruck (Mimik, Gestik, Sprache)': 'Expression (facial, gestural, verbal)',
            u'Ausdruck spezifisch': 'Expression specific',
            u'Display Anzeige': 'Displayed Text',
            u'Methode': 'Method',
            u'Methode spezifisch': 'Method specific',
            u'Roboter Geste': 'Robot gesture',
            u'Roboter Sprache': 'Robot speech',
            u'Schrank Griff': 'Cupboard handle light',
            u'Schrank Ton': 'Cupboard handle sound',
            u'Schrank Tuer': 'Cupboard door state',
            u'Sprache Anredeform': 'Speech form of address',
            u'Sprache Höflichkeit': 'Speech politeness',
            u'Sprache Satzart': 'Speech type of sentence',
            u'Sprache spezifisch': 'Speech specific',
            u'Sprache Zweck': 'Speech intention',
            u'Studienverlauf Detail': 'Study progress (detailed)',
            u'Studienverlauf grob': 'Study progress (coarse)',
            u'Adressat final (reduziert)': 'Addressee final (reduced)',
            u'Aufmerksamkeitsfokus (reduziert)': 'Focus of attention (reduced)',
            }
    return translate(dictionary,key)

def translate_addressees(addressees):
    dictionary = {
            u'Apartment als Ganzes': 'Apartment as a whole',
            u'Bildschirm (Apartmenteingang)': 'Screen (entrance)',
            u'Bildschirm (Küche, auf Arbeitsfläche)': 'Screen (kitchen on worktop)',
            u'Bilschirm (Küche, auf Arbeitsfläche)': 'Screen (kitchen on worktop)', # typo
            u'Bildschirm (Wohnzimmer, Fensterbank)': 'Screen (living room window)',
            u'Bildschirm (Wohnzimmertisch)': 'Screen (living room table)',
            u'Bildschirm (Wohnzimmerwand)': 'Screen (living room wall)',
            u'Einrichtungsgegenstände des Apartments': 'Furniture of apartment',
            u'Kleine Bildschirme im Flur': 'Tablets in hallway',
            u'Licht im Flur': 'Light in the hallway \(L_H\)',
            u'nicht erkennbar': 'Not discernible',
            u'': 'Not discernible',
            u'Roboter': 'Robot',
            u'Schalterblock (Aparmenteingang)': 'Switches (entrance)', # typo
            u'Schalterblock (Apartmenteingang)': 'Switches (entrance)',
            u'Schalterblock (Küche, Richtung Kühlschrank)': 'Switches (kitchen by the fridge)',
            u'Schalterblock (Wohnzimmer, an Stehlampe)': 'Switches (living room lamp)',
            u'Schalterblock (Wohnzimmer, Richtung Küche)': 'Switches (living room by the kitchen)',
            u'Schalter (Elektrogeräte Wohnzimmer)': 'Switch (living room)', # typo
            u'Schalter (Elektrogeräte, Wohnzimmer)': 'Switch (living room)',
            u'Schiebetür (Küche)': 'Sliding door (between hallway and kitchen)',
            u'selbst': 'Self',
            u'Stehlampe (Wohnzimmer)': 'Floor lamp \(L_F\)',
            u'Tür des Paketschranks (Küche)': 'Cupboard door',
            u'unspezifisch': 'Unspecific',
            u'Weiße Lautsprecherbox (auf Küchenschrank)': 'Loudspeaker (by the fridge)',
            u'Bildschirm (Wohnzimmer)': 'Screen (living room)',
            u'Teile des Apartments': 'Parts of the apartment'
    }
    result = []
    for i in addressees:
        result.append(translate(dictionary,i))
    return result

def translate_course_of_study_coarse(entries):
    dictionary = {
            u'Ausführen der Aufgabe (durch VPn)': 'Task solution',
            u'Lesen der Aufgabe (durch VPn)': 'Task reading'
    }
    result = []
    for i in entries:
        if i.startswith(u'Ausführen der Aufgabe (durch VPn) '): # filter repeated annotation
            result.append(translate(dictionary,u'Ausführen der Aufgabe (durch VPn)'))
        else:
            result.append(translate(dictionary,i))
    return result

def translate_wizard_addressee(entries):
    dictionary = {
            u'Roboter': 'Robot',
            u'Apartment': 'Apartment',
            u'kein Adressat': 'None',
            u'Stehlampe': 'Floor lamp \(L_F\)'
            }
    result = []
    for i in entries:
        result.append(translate(dictionary,i))
    return result

def translate_expression_specific(entries):
    dictionary = {
            u'': 'unobserved',
            u'neutral': 'neutral',
            u'höflich': 'polite',
            u'freudig': 'joyful',
            u'neugierig': 'curious',
            u'ratlos': 'baffled',
            u'enttäuscht': 'disappointed',
            u'überrascht': 'surprised',
            u'ohne Anrede': 'with Name',
            u'mit Anrede': 'without Name',
            u'Kommando (einzelne Wörter)': 'Command (words)',
            u'Kommando (vollständiger Satz)': 'Command (sentence)',
            u'Frage (einzelne Wörter)': 'Question (words)',
            u'Frage (vollständiger Satz)': 'Question (sentence)',
            u'Aussage (einzelne Wörter)': 'Statement (words)',
            u'Aussage (vollständiger Satz)': 'Statement (sentence)'
            }
    result = []
    for i in entries:
        result.append(translate(dictionary,i))
    return result

def translate_generic(entries):
    dictionary = {
            u'Sprache': 'speech',
            u'Gestik': 'gesture',
            u'Berührung': 'touch'
            }
    result = []
    for i in entries:
        result.append(translate(dictionary,i))
    return result



def translate_entries(key, entries):
    if key in ['Addressee final', 'Focus of attention', 
            'Addressee final (reduced)', 'Focus of attention (reduced)'
            ]:
        return translate_addressees(entries)
    elif key == 'Study progress (coarse)':
        return translate_course_of_study_coarse(entries)
    elif key == 'Wizard addressee':
        return translate_wizard_addressee(entries)
    elif key in ('Expression (facial, gestural, verbal)', 
                 'Speech politeness', 
                 'Speech form of address',
                 'Speech type of sentence'):
        return translate_expression_specific(entries)
    return translate_generic(entries)


# convert to row based array
cols = len(annotations.keys())
rows = len(annotations['Filename'])
data = []
for key, value in annotations.iteritems():
    assert(len(value) == rows)
    key = translate_key(key)
    column = [key]+translate_entries(key,value)
    for i,elem in enumerate(column):
        column[i]=clean_entry(elem)
    data.append(column)
data = zip(*data)

# print
for row in data:
    print('\t'.join(row).encode('utf-8'))
