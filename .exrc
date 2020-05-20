set spelllang=en_us

let g:vimtex_quickfix_method = 'pplatex'

let g:ale_linters = {
    \ 'tex': ['chktex', 'vale', 'missing-gls'],
    \ }

" todo: use own
"command! Bibfix !cd ~/thesis && bibfix -x bibfix_transform.py ~/sciebo/literature/Citavi/thesis/everything.bib thesis.bib

execute 'silent! runtime! ale_linters/tex/*.vim'

call ale#linter#Define('tex', {
\   'name': 'vale',
\   'executable': 'vale',
\   'command': 'cat %t | ' . getcwd() . '/detex.py | vale --output=JSON',
\   'callback': 'ale#handlers#vale#Handle',
\})

" copied
function! Handlegls(buffer, lines)
    let l:pattern = '^\(\d\+\):\(\d\+\) - \(\d\+\):\(\d\+\): \(.*\)$'

    let l:loclist = []
    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:loclist, {
        \   'lnum': l:match[1] + 0,
        \   'col': l:match[2] + 0,
        \   'end_col': l:match[4] + 0,
        \   'text': l:match[5],
        \   'type': 'W',
        \})
    endfor

    return l:loclist
endfunction

call ale#linter#Define('tex', {
\   'name': 'missing-gls',
\   'executable': getcwd() . '/missing-gls.py',
\   'command': getcwd() . '/missing-gls.py %t',
\   'callback': 'Handlegls',
\})

let g:grammarous#languagetool_cmd = getcwd() . '/detex-languagetool'
let g:grammarous#disabled_rules = {
    \ 'tex' : ['WHITESPACE_RULE', 'SENTENCE_WHITESPACE',
    \          'COMMA_PARENTHESIS_WHITESPACE'],
    \ }
