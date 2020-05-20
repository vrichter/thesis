import re


def _protect(string):
    return '{{{}}}'.format(string)


def hyphenate(string):
    return re.sub(r'(\w)-(\w)', '\\1\\hyp{}\\2', string)


def transform(entry):
    # Protect corporate authors
    if entry['ID'] in [
            'USArmyDepartmentofDefense2001',
            'TheEconomist2015',
            'SunMicrosystems1997']:
        entry['author'] = _protect(entry['author'])

    if 'date' not in entry:
        entry['date'] = ''

    if entry['ENTRYTYPE'] == 'techreport' and 'series' in entry:
        del entry['series']

    # hack to create manuals from gray literature entries in Citavi
    if entry['ENTRYTYPE'] == 'techreport' and 'userd' in entry:
        entry['ENTRYTYPE'] = entry['userd']

    for field in [
            'author',
            'editor',
            'title',
            'eventtitle',
            'subtitle',
            'titleaddon',
            'publisher']:
        if field in entry:
            entry[field] = hyphenate(entry[field])
