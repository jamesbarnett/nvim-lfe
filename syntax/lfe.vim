" Vim syntax file
" Language:             Racket
" Maintainer:           D. Ben Knoble <ben.knoble+github@gmail.com>
" Previous Maintainer:  Will Langstroth <will@langstroth.com>
" URL:                  https://github.com/benknoble/vim-racket
" Description:          Contains all of the keywords in #lang racket
" Last Change: 2022 Aug 12

" Initializing:
if exists("b:current_syntax")
  finish
endif

" Highlight unmatched parens
syntax match lfeError ,[]})],

if version < 800
  set iskeyword=33,35-39,42-58,60-90,94,95,97-122,126,_
else
  " syntax iskeyword 33,35-39,42-58,60-90,94,95,97-122,126,_
  " converted from decimal to char
  " :s/\d\+/\=submatch(0)->str2nr()->nr2char()/g
  " but corrected to remove duplicate _, move ^ to end
  syntax iskeyword @,!,#-',*-:,<-Z,a-z,~,_,^
  " expanded
  " syntax iskeyword !,#,$,%,&,',*,+,,,-,.,/,0-9,:,<,=,>,?,@,A-Z,_,a-z,~,^
endif

" Forms in order of appearance at
" http://docs.racket-lang.org/reference/index.html
"
" Integers
"   - Regular decimal notation (1234 -123 0)
"   - Binary Notation (#b0 #b10101 #b-1100)
"   - Octal
"   - Explicit Decimal
"   - Hex
"   - Explicit Base
"   - Character Notation #\a #\$ #\e
"   - Character hex #\x1f42d;
"   - Floating Point
"   - Strings
"   - Binary Strings
"   - Character escaping
"   - Binaries #B(43 (42 (size 16)) (42 (size 32)))
"   - Lists () (the empty list) (foo bar baz) etc.
"   - Tuples #(value1 value2 ...) #() (empty tuple)
"   - Maps #M(key1 value1 key2 value2 ... )
"   - Structs #S(struct-name key1 value1 key2 value2 ... ) // No struct
"   literals
"   - Symbols (only | \' ' , # may not be the first character of the symbols
"   name but are allowed as subsequent letters
"   - Comments one line (;) and block #| |#
"
" (quote e)
" (cons head tail)
" (car e)
" (cdr e)
" (list e ... )
" (tuple e ... )
" (tref tuple index)
" (tset tuple index val)
" (binary seg ... )
" (map key val ...)
" (map-size map) (msiz m)
" (map-get map key) (mref m k)
" (map-set map key val ...) (mset m k v ...)
" (map-update map key val ...) (mupd m k v ...)
" (map-remove map key ...) (mrem m k k ...)
" (lambda (arg ...) ...)
" (match-lambda
"   ((arg ... ) {{(when e ...)}} ...)           - Matches clauses
"   ... )
" (function func-name arity)                    - Function reference
" (function mod-name func-name arity)
" (let ((pat {{(when e ...)}} e)
"       ...)
"   ... )
" (let-function ((name lambda|match-lambda)     - Local functions
"                ... )
"   ... )
" (letrec-function ((name lambda|match-lambda)  - Local functions
"                   ... )
"   ... )
" (let-macro ((name lambda-match-lambda)        - Local macros
"             ...)
"   ...)
" (progn ... )
" (if test true-expr {{false-expr}})
" (case e
"   (pat {{(when e ...)}} ...)
"   ... ))
" (receive
"   (pat {{(when e ...)}} ... )
"   ...
"   (after timeout ... ))
" (catch ... )
" (try
"   e
"   {{(case ((pat {{(when e ...)}} ... )
"           ... ))}}
"   {{(catch
"      ((tuple type value stacktrace)|_ {{(when e ...)}}
"                             - Must be tuple of length 3 or just _!
"       ... )
"      ... )}}
"   {{(after ... )}})
" (funcall func arg ... )
" (call mod func arg ... )    - Call to Mod:Func(Arg, ... )
"
" (define-record name fields)
" (record name field val ...)
" (is-record record name)
" (record-index name field)
" (record-field record name field)
" (record-update record name field val ...)
"
" (define-struct fields)
" (struct mod-name field val ...)
" (is-struct struct)
" (is-struct struct name)
" (struct-field struct name field)
" (struct-update struct name field val ...)
"
" (define-module name meta-data attributes)
" (extend-module meta-data attributes)
"
" (define-function name meta-data lambda|match-lambda)
" (define-macro name meta-data lambda|match-lambda)
"
" (define-type type definition)
" (define-opaque-type type definition)
" (define-function-spec func spec)
" "
syntax keyword lfeSyntax defun defmacro defrecord defmodule ++
syntax keyword lfeSyntax module module* module+ require provide quote
syntax keyword lfeSyntax lists:all lists:any lists:append lists:append lists:concat
syntax keyword lfeSyntax lists:delete lists:droplast lists:dropwhile lists:duplicate lists:enumerate
syntax keyword lfeSyntax lists:enumerate lists:filter lists:filtermap lists:flatlength lists:flatmap
syntax keyword lfeSyntax lists:flatten lists:flatten lists:foldl lists:foldr lists:foreach
syntax keyword lfeSyntax lists:join lists:keydelete lists:keyfind lists:keymap lists:keymember
syntax keyword lfeSyntax lists:keymerge lists:keyreplace lists:keysearch lists:keysort lists:keystore
syntax keyword lfeSyntax lists:keytake lists:last lists:map lists:mapfoldl lists:mapfoldr
syntax keyword lfeSyntax lists:max lists:member lists:merge lists:merge lists:merge
syntax keyword lfeSyntax lists:merge3 lists:min lists:module_info lists:module_info lists:nth
syntax keyword lfeSyntax lists:nthtail lists:partition lists:prefix lists:reverse lists:reverse
syntax keyword lfeSyntax lists:rkeymerge lists:rmerge lists:rmerge lists:rmerge3 lists:rukeymerge
syntax keyword lfeSyntax lists:rumerge lists:rumerge lists:rumerge3 lists:search lists:seq
syntax keyword lfeSyntax lists:seq lists:sort lists:sort lists:split lists:splitwith
syntax keyword lfeSyntax lists:sublist lists:sublist lists:subtract lists:suffix lists:sum
syntax keyword lfeSyntax lists:takewhile lists:ukeymerge lists:ukeysort lists:umerge lists:umerge
syntax keyword lfeSyntax lists:umerge lists:umerge3 lists:uniq lists:uniq lists:unzip
syntax keyword lfeSyntax lists:unzip3 lists:usort lists:usort lists:zf lists:zip
syntax keyword lfeSyntax lists:zip3 lists:zipwith lists:zipwith3

" 10.3 Delayed Evaluation
syntax keyword lfeFunc promise? delay lazy force promise-forced? promise-running?

" 10.3.1 Additional Promise Kinds
syntax keyword lfeFunc delay/name promise/name delay/strict delay/sync delay/thread delay/idle
syntax match lfeDelimiter !\<\.\>!

syntax cluster lfeTop contains=lfeSyntax,lfeFunc,lfeDelimiter

syntax match lfeConstant  ,\<\*\k\+\*\>,
syntax match lfeConstant  ,\<<\k\+>\>,

syntax match lfeStringEscapeError "\\." contained display
syntax match lfeStringEscape "\\[abtnvfre'"\\]"        contained display
syntax match lfeStringEscape "\\$"                     contained display
syntax match lfeStringEscape "\\\o\{1,3}\|\\x\x\{1,2}" contained display

syntax match lfeUStringEscape "\\u\x\{1,4}\|\\U\x\{1,8}" contained display
syntax match lfeUStringEscape "\\u\x\{4}\\u\x\{4}"       contained display

syntax region lfeString start=/\%(\\\)\@<!"/ skip=/\\[\\"]/ end=/"/ contains=lfeStringEscapeError,lfeStringEscape,lfeUStringEscape
syntax region lfeString start=/#"/           skip=/\\[\\"]/ end=/"/ contains=lfeStringEscapeError,lfeStringEscape

if exists("lfe_no_string_fold")
  syn region lfeString start=/#<<\z(.*\)$/ end=/^\z1$/
else
  syn region lfeString start=/#<<\z(.*\)$/ end=/^\z1$/ fold
endif


syntax cluster lfeTop  add=lfeError,lfeConstant,lfeString

" Numbers

" anything which doesn't match the below rules, but starts with a #d, #b, #o,
" #x, #i, or #e, is an error
syntax match lfeNumberError         "\<#[xdobie]\k*"

syntax match lfeContainedNumberError   "\<#o\k*[^-+0-7delfinas#./@]\>"
syntax match lfeContainedNumberError   "\<#b\k*[^-+01delfinas#./@]\>"
syntax match lfeContainedNumberError   "\<#[ei]#[ei]"
syntax match lfeContainedNumberError   "\<#[xdob]#[xdob]"

" start with the simpler sorts
syntax match lfeNumber    "\<\(#[dobie]\)\{0,2}[-+]\?\(\d\+\|\d\+#*\.\|\d*\.\d\+\)#*\(/\d\+#*\)\?\([sdlef][-+]\?\d\+#*\)\?\>" contains=lfeContainedNumberError
syntax match lfeNumber    "\<\(#[dobie]\)\{0,2}[-+]\?\d\+/\d\+\>" contains=lfeContainedNumberError
syntax match lfeNumber    "\<\(#[dobie]\)\{0,2}[-+]\?\d\+/\d\+[-+]\d\+\(/\d\+\)\?i\>" contains=lfeContainedNumberError

" different possible ways of expressing complex values
syntax match lfeNumber    "\<\(#[dobie]\)\{0,2}[-+]\(\d\+\|\d\+#*\.\|\d*\.\d\+\)#*\(/\d\+#*\)\?\([sdlef][-+]\?\d\+#*\)\?i\>" contains=lfeContainedNumberError
syntax match lfeNumber    "\<\(#[dobie]\)\{0,2}[-+]\?\(\d\+\|\d\+#*\.\|\d*\.\d\+\)#*\(/\d\+#*\)\?\([sdlef][-+]\?\d\+#*\)\?[-+]\(\d\+\|\d\+#*\.\|\d*\.\d\+\)#*\(/\d\+#*\)\?\([sdlef][-+]\?\d\+#*\)\?i\>" contains=lfeContainedNumberError
syntax match lfeNumber    "\<\(#[dobie]\)\{0,2}[-+]\(inf\|nan\)\.[0f][-+]\(\d\+\|\d\+#*\.\|\d*\.\d\+\)#*\(/\d\+#*\)\?\([sdlef][-+]\?\d\+#*\)\?i\>" contains=lfeContainedNumberError
syntax match lfeNumber    "\<\(#[dobie]\)\{0,2}[-+]\?\(\d\+\|\d\+#*\.\|\d*\.\d\+\)#*\(/\d\+#*\)\?\([sdlef][-+]\?\d\+#*\)\?[-+]\(inf\|nan\)\.[0f]i\>" contains=lfeContainedNumberError
syntax match lfeNumber    "\<\(#[dobie]\)\{0,2}[-+]\?\(\d\+\|\d\+#*\.\|\d*\.\d\+\)#*\(/\d\+#*\)\?\([sdlef][-+]\?\d\+#*\)\?@[-+]\?\(\d\+\|\d\+#*\.\|\d*\.\d\+\)#*\(/\d\+#*\)\?\([sdlef][-+]\?\d\+#*\)\?\>" contains=lfeContainedNumberError
syntax match lfeNumber    "\<\(#[dobie]\)\{0,2}[-+]\(inf\|nan\)\.[0f]@[-+]\?\(\d\+\|\d\+#*\.\|\d*\.\d\+\)#*\(/\d\+#*\)\?\([sdlef][-+]\?\d\+#*\)\?\>" contains=lfeContainedNumberError
syntax match lfeNumber    "\<\(#[dobie]\)\{0,2}[-+]\?\(\d\+\|\d\+#*\.\|\d*\.\d\+\)#*\(/\d\+#*\)\?\([sdlef][-+]\?\d\+#*\)\?@[-+]\(inf\|nan\)\.[0f]\>" contains=lfeContainedNumberError

" hex versions of the above (separate because of the different possible exponent markers)
syntax match lfeNumber    "\<\(#x\|#[ei]#x\|#x#[ei]\)[-+]\?\(\x\+\|\x\+#*\.\|\x*\.\x\+\)#*\(/\x\+#*\)\?\([sl][-+]\?\x\+#*\)\?\>"
syntax match lfeNumber    "\<\(#x\|#[ei]#x\|#x#[ei]\)[-+]\?\x\+/\x\+\>"
syntax match lfeNumber    "\<\(#x\|#[ei]#x\|#x#[ei]\)[-+]\?\x\+/\x\+[-+]\x\+\(/\x\+\)\?i\>"

syntax match lfeNumber    "\<\(#x\|#[ei]#x\|#x#[ei]\)[-+]\(\x\+\|\x\+#*\.\|\x*\.\x\+\)#*\(/\x\+#*\)\?\([sl][-+]\?\x\+#*\)\?i\>"
syntax match lfeNumber    "\<\(#x\|#[ei]#x\|#x#[ei]\)[-+]\?\(\x\+\|\x\+#*\.\|\x*\.\x\+\)#*\(/\x\+#*\)\?\([sl][-+]\?\x\+#*\)\?[-+]\(\x\+\|\x\+#*\.\|\x*\.\x\+\)#*\(/\x\+#*\)\?\([sl][-+]\?\x\+#*\)\?i\>"
syntax match lfeNumber    "\<\(#x\|#[ei]#x\|#x#[ei]\)[-+]\(inf\|nan\)\.[0f][-+]\(\x\+\|\x\+#*\.\|\x*\.\x\+\)#*\(/\x\+#*\)\?\([sl][-+]\?\x\+#*\)\?i\>"
syntax match lfeNumber    "\<\(#x\|#[ei]#x\|#x#[ei]\)[-+]\?\(\x\+\|\x\+#*\.\|\x*\.\x\+\)#*\(/\x\+#*\)\?\([sl][-+]\?\x\+#*\)\?[-+]\(inf\|nan\)\.[0f]i\>"
syntax match lfeNumber    "\<\(#x\|#[ei]#x\|#x#[ei]\)[-+]\?\(\x\+\|\x\+#*\.\|\x*\.\x\+\)#*\(/\x\+#*\)\?\([sl][-+]\?\x\+#*\)\?@[-+]\?\(\x\+\|\x\+#*\.\|\x*\.\x\+\)#*\(/\x\+#*\)\?\([sl][-+]\?\x\+#*\)\?\>"
syntax match lfeNumber    "\<\(#x\|#[ei]#x\|#x#[ei]\)[-+]\(inf\|nan\)\.[0f]@[-+]\?\(\x\+\|\x\+#*\.\|\x*\.\x\+\)#*\(/\x\+#*\)\?\([sl][-+]\?\x\+#*\)\?\>"
syntax match lfeNumber    "\<\(#x\|#[ei]#x\|#x#[ei]\)[-+]\?\(\x\+\|\x\+#*\.\|\x*\.\x\+\)#*\(/\x\+#*\)\?\([sl][-+]\?\x\+#*\)\?@[-+]\(inf\|nan\)\.[0f]\>"

" these work for any radix
syntax match lfeNumber    "\<\(#[xdobie]\)\{0,2}[-+]\(inf\|nan\)\.[0f]i\?\>" contains=lfeContainedNumberError
syntax match lfeNumber    "\<\(#[xdobie]\)\{0,2}[-+]\(inf\|nan\)\.[0f][-+]\(inf\|nan\)\.[0f]i\>" contains=lfeContainedNumberError
syntax match lfeNumber    "\<\(#[xdobie]\)\{0,2}[-+]\(inf\|nan\)\.[0f]@[-+]\(inf\|nan\)\.[0f]\>" contains=lfeContainedNumberError

syntax keyword lfeBoolean  #t #f #true #false #T #F

syntax match lfeError   "\<#\\\k*\>"

syntax match lfeChar    "\<#\\.\w\@!"
syntax match lfeChar    "\<#\\space\>"
syntax match lfeChar    "\<#\\newline\>"
syntax match lfeChar    "\<#\\return\>"
syntax match lfeChar    "\<#\\null\?\>"
syntax match lfeChar    "\<#\\backspace\>"
syntax match lfeChar    "\<#\\tab\>"
syntax match lfeChar    "\<#\\linefeed\>"
syntax match lfeChar    "\<#\\vtab\>"
syntax match lfeChar    "\<#\\page\>"
syntax match lfeChar    "\<#\\rubout\>"
syntax match lfeChar    "\<#\\\o\{1,3}\>"
syntax match lfeChar    "\<#\\x\x\{1,2}\>"
syntax match lfeChar    "\<#\\u\x\{1,6}\>"

syntax cluster lfeTop  add=lfeNumber,lfeBoolean,lfeChar

" Command-line parsing
syntax keyword lfeExtFunc command-line current-command-line-arguments once-any help-labels multi once-each

syntax match lfeSyntax    "#lang "
syntax match lfeExtSyntax "#:\k\+"

syntax cluster lfeTop  add=lfeExtFunc,lfeExtSyntax

" syntax quoting, unquoting and quasiquotation
syntax match lfeQuote "#\?['`]"

syntax match lfeUnquote "#,"
syntax match lfeUnquote "#,@"
syntax match lfeUnquote ","
syntax match lfeUnquote ",@"

" Comments
syntax match lfeSharpBang "\%^#![ /].*" display
syntax match lfeComment /;.*$/ contains=lfeTodo,lfeNote,@Spell
syntax region lfeMultilineComment start=/#|/ end=/|#/ contains=lfeMultilineComment,lfeTodo,lfeNote,@Spell
syntax match lfeFormComment "#;" nextgroup=@lfeTop

syntax match lfeTodo /\C\<\(FIXME\|TODO\|XXX\)\ze:\?\>/ contained
syntax match lfeNote /\CNOTE\ze:\?/ contained

syntax cluster lfeTop  add=lfeQuote,lfeUnquote,lfeComment,lfeMultilineComment,lfeFormComment

" Synchronization and the wrapping up...
syntax sync match matchPlace grouphere NONE "^[^ \t]"
" ... i.e. synchronize on a line that starts at the left margin

" Define the default highlighting.
highlight default link lfeSyntax Statement
highlight default link lfeFunc Function

highlight default link lfeString String
highlight default link lfeStringEscape Special
highlight default link lfeUStringEscape Special
highlight default link lfeStringEscapeError Error
highlight default link lfeChar Character
highlight default link lfeBoolean Boolean

highlight default link lfeNumber Number
highlight default link lfeNumberError Error
highlight default link lfeContainedNumberError Error

highlight default link lfeQuote SpecialChar
highlight default link lfeUnquote SpecialChar

highlight default link lfeDelimiter Delimiter
highlight default link lfeParen Delimiter
highlight default link lfeConstant Constant

highlight default link lfeLit Type
highlight default link lfeRe Type

highlight default link lfeComment Comment
highlight default link lfeMultilineComment Comment
highlight default link lfeFormComment SpecialChar
highlight default link lfeSharpBang Comment
highlight default link lfeTodo Todo
highlight default link lfeNote SpecialComment
highlight default link lfeError Error

highlight default link lfeExtSyntax Type
highlight default link lfeExtFunc PreProc

let b:current_syntax = "lfe"
