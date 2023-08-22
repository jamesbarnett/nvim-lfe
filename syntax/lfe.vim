" Vim syntax file
" Language:             Racket
" Maintainer:           D. Ben Knoble <ben.knoble+github@gmail.com>
" Previous Maintainer:  Will Langstroth <will@langstroth.com>
" URL:                  https://github.com/benknoble/vim-lfe
" Description:          Contains all of the keywords in #lang lfe
" Last Change: 2022 Aug 12

" Initializing:
if exists("b:current_syntax")
  finish
endif

syntax clear
syn case ignore

let s:cpo = &cpo
set cpo&vim

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

" syn match lfeParentheses "[^ '`\t\n()\[\]";]\+"
" syn match lfeParentheses "[)\]]"

"
"
"
" Furthermore, there is an include file which developers may which to utilize in their LFE programs: (include-lib "lfe/include/cl.lfe"). Currently this offers Common Lisp predicates, but may include other useful macros and functions in the future. The provided predicate macros wrap the various is_* Erlang functions; since these are expanded at compile time, they are usable in guards. The include the following:
"
" (alivep x)
" (atomp x)
" (binaryp x)
" (bitstringp x)
" (boolp x) and (booleanp x)
" (builtinp x)
" (consp x)
" (floatp x)
" (funcp x) and (functionp x)
" (intp x) and (integerp x)
" (listp x)
" (mapp x)
" (numberp x)
" (pidp x)
" (process-alive-p x)
" (recordp x tag)
" (recordp x tag size)
" (refp x) and (referencep x)
" (tuplep x)
" (vectorp x)
"
" Non-predicate macros in lfe/include/cl.lfe include:
"
" (dolist ...)
" (vector ...)
"
" Supplemental Clojure Functions
"
" From LFE's earliest days, it's Lisp-cousin Clojure (created around the same time) has inspired LFE developers to create similar, BEAM-versions of those functions. These were collected in a separate library and then expanded upon, until eventually becoming part of the LFE standard library.
"
" Function definition macros:
"
" (clj:defn ...)
" (clj:defn- ...)
" (clj:fn ...)
"
" Threading macros:
"
"(clj:-> ...)
" (clj:->> ...)
" (clj:as-> ...)
" (clj:cond-> ...)
" (clj:cond->> ...)
" (clj:some-> ...)
" (clj:some->> ...)
" (clj:doto ...)
"
" Conditional Macros
"
" (clj:if-let ...)
" (clj:iff-let ...)
" (clj:condp ...)
" (clj:if-not ...)
" (clj:iff-not ...)
" (clj:when-not ...)
" (clj:not= ...)
"
" Predicate macros:
"
" (clj:atom? x)
" (clj:binary? x)
" (clj:bitstring? x)
" (clj:bool? x)
" (clj:boolean? x)
" (clj:even? x)
" (clj:false? x)
" (clj:falsy? x)
" (clj:float? x)
" (clj:func? x)
" (clj:function? x)
" (clj:identical? x)
" (clj:int? x)
" (clj:integer? x)
" (clj:map? x)
" (clj:neg? x)
" (clj:nil? x)
" (clj:number? x)
" (clj:odd? x)
" (clj:pos? x)
" (clj:record? x)
" (clj:reference? x)
" (clj:true? x)
" (clj:tuple? x)
" (clj:undef? x)
" (clj:undefined? x)
" (clj:zero? x)
"
" Other:
"
" (clj:str x)
" (clj:lazy-seq x)
" (clj:conj ...)
" (clj:if ...)
syntax keyword lfeSyntax        module module* module+ require provide
syntax keyword lfeSyntax        quote car cdr list tuple tref tset binary
syntax keyword lfeSyntax        map map-size map-get map-set map-update map-remove
syntax keyword lfeSyntax        lambda match-lambda function let let-function
syntax keyword lfeSyntax        letrec-function let-macro progn
syntax keyword lfeSyntax        if case receive catch try after funcall call define-record
syntax keyword lfeSyntax        is-record record-index record-fields struct is-struct
syntax keyword lfeSyntax        struct-field struct-update define-module extend-module
syntax keyword lfeSyntax        define-function define-macro define-type define-function-spec
syntax keyword lfeSyntax        define-opaque-type
syntax keyword lfeSyntax        ? ++ -- list* flet flet* fletrec cond andalso orelse
syntax keyword lfeSyntax        fun lc list-comp bc binary-comp ets-ms trace-ms
syntax keyword lfeFunction      defmacro defsyntax macrolet syntaxlet prog1 prog2 when
syntax keyword lfeSyntax        defmodule defrecord defstruct set defun
syntax keyword lfeSyntax        mset msiz mref mupd binary record is-record record-field
syntax keyword lfeSyntax        type-test guard-bif acons pairlis assoc assoc-if
syntax keyword lfeSyntax        assoc-if-not rassoc rassoc-if rassoc-if-not
syntax keyword lfeSyntax        subst subst-if subst-if-not sublis
syntax keyword lfeSyntax        cl:make-lfe-bool cl:make-cl-bool cl:mapcar cl:maplist
syntax keyword lfeSyntax        cl:mapc cl:mapl cl:symbol-plist cl:symbol-name cl:get cl:get cl:getl cl:putprop
syntax keyword lfeSyntax        cl:remprop cl:getf cl:getf cl:putf cl:remf cl:get-properties
syntax keyword lfeSyntax        cl:elt cl:length cl:reverse cl:some cl:every cl:notany cl:notevery cl:reduce
syntax keyword lfeSyntax        cl:reduce cl:reduce cl:reduce cl:remove cl:remove-if cl:remove-if-not
syntax keyword lfeSyntax        cl:remove-duplicates cl:find cl:find-if cl:find-if-not cl:find-duplicates
syntax keyword lfeSyntax        cl:position cl:position-if cl:position-if-not cl:position-duplicates
syntax keyword lfeSyntax        cl:count cl:count-if cl:count-if-not cl:count-duplicates
syntax keyword lfeSyntax        cl:car cl:first cl:cdr cl:rest cl:nth cl:nthcdr
syntax keyword lfeSyntax        cl:last cl:butlast cl:subst cl:subst-if cl:subst-if-not
syntax keyword lfeSyntax        cl:sublis cl:member cl:member-if cl:member-if-not cl:adjoin cl:union
syntax keyword lfeSyntax        cl:intersection cl:set-difference cl:set-exclusive-or cl:subsetp
syntax keyword lfeSyntax        cl:acons cl:pairlis cl:pairlis cl:assoc cl:assoc-if cl:assoc-if-not
syntax keyword lfeSyntax        cl:rassoc cl:rassoc-if cl:rassoc-if-not cl:type-of cl:coerce
syntax keyword lfeSyntax        lists:all lists:any lists:append lists:append lists:concat
syntax keyword lfeSyntax        lists:delete lists:droplast lists:dropwhile lists:duplicate lists:enumerate
syntax keyword lfeSyntax        lists:enumerate lists:filter lists:filtermap lists:flatlength lists:flatmap
syntax keyword lfeSyntax        lists:flatten lists:flatten lists:foldl lists:foldr lists:foreach
syntax keyword lfeSyntax        lists:join lists:keydelete lists:keyfind lists:keymap lists:keymember
syntax keyword lfeSyntax        lists:keymerge lists:keyreplace lists:keysearch lists:keysort lists:keystore
syntax keyword lfeSyntax        lists:keytake lists:last lists:map lists:mapfoldl lists:mapfoldr
syntax keyword lfeSyntax        lists:max lists:member lists:merge lists:merge lists:merge
syntax keyword lfeSyntax        lists:merge3 lists:min lists:module_info lists:module_info lists:nth
syntax keyword lfeSyntax        lists:nthtail lists:partition lists:prefix lists:reverse lists:reverse
syntax keyword lfeSyntax        lists:rkeymerge lists:rmerge lists:rmerge lists:rmerge3 lists:rukeymerge
syntax keyword lfeSyntax        lists:rumerge lists:rumerge lists:rumerge3 lists:search lists:seq
syntax keyword lfeSyntax        lists:seq lists:sort lists:sort lists:split lists:splitwith
syntax keyword lfeSyntax        lists:sublist lists:sublist lists:subtract lists:suffix lists:sum
syntax keyword lfeSyntax        lists:takewhile lists:ukeymerge lists:ukeysort lists:umerge lists:umerge
syntax keyword lfeSyntax        lists:umerge lists:umerge3 lists:uniq lists:uniq lists:unzip
syntax keyword lfeSyntax        lists:unzip3 lists:usort lists:usort lists:zf lists:zip
syntax keyword lfeSyntax        lists:zip3 lists:zipwith lists:zipwith3

syn match lfeDelimiter !\<\.\>!

syn match lfeSymbol    ,\k+,  contained

syn cluster lfeNormal  contains=lfeSyntax,lfeFunc,lfeDelimiter
syn cluster lfeQuotedStuff  contains=lfeSymbol
syn cluster lfeQuotedOrNormal  contains=lfeDelimiter

syn match lfeConstant  ,\<\*\k\+\*\>,
syn match lfeConstant  ,\<<\k\+>\>,

syn region lfeQuotedStruc start="("rs=s+1 end=")"re=e-1     contains=@lfeQuotedStuff,@lfeQuotedOrNormal contained
syn region lfeQuotedStruc start="#("rs=s+2 end=")"re=e-1    contains=@lfeQuotedStuff,@lfeQuotedOrNormal contained
syn region lfeQuotedStruc start="{"rs=s+1 end="}"re=e-1   contains=@lfeQuotedStuff,@lfeQuotedOrNormal contained
syn region lfeQuotedStruc start="#{"rs=s+2 end="}"re=e-1  contains=@lfeQuotedStuff,@lfeQuotedOrNormal contained
syn region lfeQuotedStruc start="\["rs=s+1 end="\]"re=e-1   contains=@lfeQuotedStuff,@lfeQuotedOrNormal contained
syn region lfeQuotedStruc start="#\["rs=s+2 end="\]"re=e-1  contains=@lfeQuotedStuff,@lfeQuotedOrNormal contained

syn cluster lfeQuotedStuff  add=lfeQuotedStruc

syn cluster lfeNormal  contains=lfeSyntax,lfeFunc,lfeDelimiter
syn cluster lfeQuotedStuff  contains=lfeSymbol
syn cluster lfeQuotedOrNormal  contains=lfeDelimiter

" Non-quoted lists, and strings
syn region lfeStruc matchgroup=Delimiter start="("rs=s+1 matchgroup=Delimiter end=")"re=e-1 contains=@lfeNormal
syn region lfeStruc matchgroup=Delimiter start="#("rs=s+2 matchgroup=Delimiter end=")"re=e-1 contains=@lfeNormal
syn region lfeStruc matchgroup=Delimiter start="{"rs=s+1 matchgroup=Delimiter end="}"re=e-1 contains=@lfeNormal
syn region lfeStruc matchgroup=Delimiter start="#{"rs=s+2 matchgroup=Delimiter end="}"re=e-1 contains=@lfeNormal
syn region lfeStruc matchgroup=Delimiter start="\["rs=s+1 matchgroup=Delimiter end="\]"re=e-1 contains=@lfeNormal
syn region lfeStruc matchgroup=Delimiter start="#\["rs=s+2 matchgroup=Delimiter end="\]"re=e-1 contains=@lfeNormal

" Simple literals
syn region lfeString start=/\%(\\\)\@<!"/ skip=/\\[\\"]/ end=/"/
syn region lfeString start=/#<<\z(.*\)$/ end=/^\z1$/

syn cluster lfeNormal  add=lfeError,lfeConstant,lfeStruc,lfeString
syn cluster lfeQuotedOrNormal  add=lfeString

" Numbers

" anything which doesn't match the below rules, but starts with a #d, #b, #o,
" #x, #i, or #e, is an error
syn match lfeNumberError         "\<#[xdobie]\k*"

syn match lfeContainedNumberError   "\<#o\k*[^-+0-7delfinas#./@]\>"
syn match lfeContainedNumberError   "\<#b\k*[^-+01delfinas#./@]\>"
syn match lfeContainedNumberError   "\<#[ei]#[ei]"
syn match lfeContainedNumberError   "\<#[xdob]#[xdob]"

" start with the simpler sorts
syn match lfeNumber    "\<\(#[dobie]\)\{0,2}[-+]\?\(\d\+\|\d\+#*\.\|\d*\.\d\+\)#*\(/\d\+#*\)\?\([sdlef][-+]\?\d\+#*\)\?\>" contains=lfeContainedNumberError
syn match lfeNumber    "\<\(#[dobie]\)\{0,2}[-+]\?\d\+/\d\+\>" contains=lfeContainedNumberError
syn match lfeNumber    "\<\(#[dobie]\)\{0,2}[-+]\?\d\+/\d\+[-+]\d\+\(/\d\+\)\?i\>" contains=lfeContainedNumberError

" different possible ways of expressing complex values
syn match lfeNumber    "\<\(#[dobie]\)\{0,2}[-+]\(\d\+\|\d\+#*\.\|\d*\.\d\+\)#*\(/\d\+#*\)\?\([sdlef][-+]\?\d\+#*\)\?i\>" contains=lfeContainedNumberError
syn match lfeNumber    "\<\(#[dobie]\)\{0,2}[-+]\?\(\d\+\|\d\+#*\.\|\d*\.\d\+\)#*\(/\d\+#*\)\?\([sdlef][-+]\?\d\+#*\)\?[-+]\(\d\+\|\d\+#*\.\|\d*\.\d\+\)#*\(/\d\+#*\)\?\([sdlef][-+]\?\d\+#*\)\?i\>" contains=lfeContainedNumberError
syn match lfeNumber    "\<\(#[dobie]\)\{0,2}[-+]\(inf\|nan\)\.[0f][-+]\(\d\+\|\d\+#*\.\|\d*\.\d\+\)#*\(/\d\+#*\)\?\([sdlef][-+]\?\d\+#*\)\?i\>" contains=lfeContainedNumberError
syn match lfeNumber    "\<\(#[dobie]\)\{0,2}[-+]\?\(\d\+\|\d\+#*\.\|\d*\.\d\+\)#*\(/\d\+#*\)\?\([sdlef][-+]\?\d\+#*\)\?[-+]\(inf\|nan\)\.[0f]i\>" contains=lfeContainedNumberError
syn match lfeNumber    "\<\(#[dobie]\)\{0,2}[-+]\?\(\d\+\|\d\+#*\.\|\d*\.\d\+\)#*\(/\d\+#*\)\?\([sdlef][-+]\?\d\+#*\)\?@[-+]\?\(\d\+\|\d\+#*\.\|\d*\.\d\+\)#*\(/\d\+#*\)\?\([sdlef][-+]\?\d\+#*\)\?\>" contains=lfeContainedNumberError
syn match lfeNumber    "\<\(#[dobie]\)\{0,2}[-+]\(inf\|nan\)\.[0f]@[-+]\?\(\d\+\|\d\+#*\.\|\d*\.\d\+\)#*\(/\d\+#*\)\?\([sdlef][-+]\?\d\+#*\)\?\>" contains=lfeContainedNumberError
syn match lfeNumber    "\<\(#[dobie]\)\{0,2}[-+]\?\(\d\+\|\d\+#*\.\|\d*\.\d\+\)#*\(/\d\+#*\)\?\([sdlef][-+]\?\d\+#*\)\?@[-+]\(inf\|nan\)\.[0f]\>" contains=lfeContainedNumberError

" hex versions of the above (separate because of the different possible exponent markers)
syn match lfeNumber    "\<\(#x\|#[ei]#x\|#x#[ei]\)[-+]\?\(\x\+\|\x\+#*\.\|\x*\.\x\+\)#*\(/\x\+#*\)\?\([sl][-+]\?\x\+#*\)\?\>"
syn match lfeNumber    "\<\(#x\|#[ei]#x\|#x#[ei]\)[-+]\?\x\+/\x\+\>"
syn match lfeNumber    "\<\(#x\|#[ei]#x\|#x#[ei]\)[-+]\?\x\+/\x\+[-+]\x\+\(/\x\+\)\?i\>"

syn match lfeNumber    "\<\(#x\|#[ei]#x\|#x#[ei]\)[-+]\(\x\+\|\x\+#*\.\|\x*\.\x\+\)#*\(/\x\+#*\)\?\([sl][-+]\?\x\+#*\)\?i\>"
syn match lfeNumber    "\<\(#x\|#[ei]#x\|#x#[ei]\)[-+]\?\(\x\+\|\x\+#*\.\|\x*\.\x\+\)#*\(/\x\+#*\)\?\([sl][-+]\?\x\+#*\)\?[-+]\(\x\+\|\x\+#*\.\|\x*\.\x\+\)#*\(/\x\+#*\)\?\([sl][-+]\?\x\+#*\)\?i\>"
syn match lfeNumber    "\<\(#x\|#[ei]#x\|#x#[ei]\)[-+]\(inf\|nan\)\.[0f][-+]\(\x\+\|\x\+#*\.\|\x*\.\x\+\)#*\(/\x\+#*\)\?\([sl][-+]\?\x\+#*\)\?i\>"
syn match lfeNumber    "\<\(#x\|#[ei]#x\|#x#[ei]\)[-+]\?\(\x\+\|\x\+#*\.\|\x*\.\x\+\)#*\(/\x\+#*\)\?\([sl][-+]\?\x\+#*\)\?[-+]\(inf\|nan\)\.[0f]i\>"
syn match lfeNumber    "\<\(#x\|#[ei]#x\|#x#[ei]\)[-+]\?\(\x\+\|\x\+#*\.\|\x*\.\x\+\)#*\(/\x\+#*\)\?\([sl][-+]\?\x\+#*\)\?@[-+]\?\(\x\+\|\x\+#*\.\|\x*\.\x\+\)#*\(/\x\+#*\)\?\([sl][-+]\?\x\+#*\)\?\>"
syn match lfeNumber    "\<\(#x\|#[ei]#x\|#x#[ei]\)[-+]\(inf\|nan\)\.[0f]@[-+]\?\(\x\+\|\x\+#*\.\|\x*\.\x\+\)#*\(/\x\+#*\)\?\([sl][-+]\?\x\+#*\)\?\>"
syn match lfeNumber    "\<\(#x\|#[ei]#x\|#x#[ei]\)[-+]\?\(\x\+\|\x\+#*\.\|\x*\.\x\+\)#*\(/\x\+#*\)\?\([sl][-+]\?\x\+#*\)\?@[-+]\(inf\|nan\)\.[0f]\>"

" these work for any radix
syn match lfeNumber    "\<\(#[xdobie]\)\{0,2}[-+]\(inf\|nan\)\.[0f]i\?\>" contains=lfeContainedNumberError
syn match lfeNumber    "\<\(#[xdobie]\)\{0,2}[-+]\(inf\|nan\)\.[0f][-+]\(inf\|nan\)\.[0f]i\>" contains=lfeContainedNumberError
syn match lfeNumber    "\<\(#[xdobie]\)\{0,2}[-+]\(inf\|nan\)\.[0f]@[-+]\(inf\|nan\)\.[0f]\>" contains=lfeContainedNumberError


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

syn cluster lfeNormal  add=lfeNumber,lfeBoolean,lfeChar
syn cluster lfeQuotedOrNormal  add=lfeNumber,lfeBoolean

" Command-line parsing
syntax keyword lfeExtFunc command-line current-command-line-arguments once-any help-labels multi once-each

syntax match lfeSyntax    "#lang "
syntax match lfeExtSyntax "#:\k\+"

syn cluster lfeNormal  add=lfeExtFunc,lfeExtSyntax

" syntax quoting, unquoting and quasiquotation
syn region lfeQuoted matchgroup=Delimiter start="['`]" end=![ \t()\[\]";]!me=e-1 contains=@lfeQuotedStuff,@lfeQuotedOrNormal
syn region lfeQuoted matchgroup=Delimiter start="['`](" matchgroup=Delimiter end=")" contains=@lfeQuotedStuff,@lfeQuotedOrNormal
syn region lfeQuoted matchgroup=Delimiter start="['`]\?#(" matchgroup=Delimiter end=")" contains=@lfeQuotedStuff,@lfeQuotedOrNormal

syn region lfeUnquote matchgroup=Delimiter start="#,"rs=s+2 end=![ \t\[\]()";]!re=e-1,me=e-1 contained contains=@lfeNormal
syn region lfeUnquote matchgroup=Delimiter start="#,@"rs=s+3 end=![ \t\[\]()";]!re=e-1,me=e-1 contained contains=@lfeNormal
syn region lfeUnquote matchgroup=Delimiter start="#,("rs=s+3 end=")"re=e-1 contained contains=@lfeNormal
syn region lfeUnquote matchgroup=Delimiter start="#,@("rs=s+4 end=")"re=e-1 contained contains=@lfeNormal
syn region lfeUnquote matchgroup=Delimiter start="#,\["rs=s+3 end="\]"re=e-1 contained contains=@lfeNormal
syn region lfeUnquote matchgroup=Delimiter start="#,@\["rs=s+4 end="\]"re=e-1 contained contains=@lfeNormal
syn region lfeUnquote matchgroup=Delimiter start=","rs=s+1 end=![ \t\[\]()";]!re=e-1,me=e-1 contained contains=@lfeNormal
syn region lfeUnquote matchgroup=Delimiter start=",@"rs=s+2 end=![ \t\[\]()";]!re=e-1,me=e-1 contained contains=@lfeNormal
syn region lfeUnquote matchgroup=Delimiter start=",("rs=s+2 end=")"re=e-1 contained contains=@lfeNormal
syn region lfeUnquote matchgroup=Delimiter start=",@("rs=s+3 end=")"re=e-1 contained contains=@lfeNormal
syn region lfeUnquote matchgroup=Delimiter start=",#("rs=s+3 end=")"re=e-1 contained contains=@lfeNormal
syn region lfeUnquote matchgroup=Delimiter start=",@#("rs=s+4 end=")"re=e-1 contained contains=@lfeNormal
syn region lfeUnquote matchgroup=Delimiter start=",\["rs=s+2 end="\]"re=e-1 contained contains=@lfeNormal
syn region lfeUnquote matchgroup=Delimiter start=",@\["rs=s+3 end="\]"re=e-1 contained contains=@lfeNormal
syn region lfeUnquote matchgroup=Delimiter start=",#\["rs=s+3 end="\]"re=e-1 contained contains=@lfeNormal
syn region lfeUnquote matchgroup=Delimiter start=",@#\["rs=s+4 end="\]"re=e-1 contained contains=@lfeNormal

syn cluster lfeQuotedStuff add=lfeUnquote

syn region lfeQuoted matchgroup=Delimiter start="#['`]"rs=s+2 end=![ \t()\[\]";]!re=e-1,me=e-1 contains=@lfeQuotedStuff,@lfeQuotedOrNormal
syn region lfeQuoted matchgroup=Delimiter start="#['`]("rs=s+3 matchgroup=Delimiter end=")"re=e-1 contains=@lfeQuotedStuff,@lfeQuotedOrNormal

" Comments
syn match lfeComment /;.*$/ contains=lfeTodo,lfeNote,@Spell
syn region lfeMultilineComment start=/#|/ end=/|#/ contains=lfeMultilineComment,lfeTodo,lfeNote,@Spell

syn keyword lfeTodo FIXME TODO XXX contained
syntax match lfeNote /\CNOTE\ze:\?/ contained

syn cluster lfeNormal  add=lfeQuoted,lfeComment,lfeMultilineComment
syn cluster lfeQuotedOrNormal  add=lfeComment,lfeMultilineComment

" Synchronization and the wrapping up...
syntax sync match matchPlace grouphere NONE "^[^ \t]"
" ... i.e. synchronize on a line that starts at the left margin

if version >= 508 || !exists("did_lfe_syntax_inits")
  if version < 508
    let did_lfe_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink lfeSyntax             Statement
  HiLink lfeFunc               Function

  HiLink lfeString             String
  HiLink lfeChar               Character
  HiLink lfeBoolean            Boolean

  HiLink lfeNumber             Number
  HiLink lfeNumberError        Error
  HiLink lfeContainedNumberError Error

  HiLink lfeQuoted             Structure
  HiLink lfeQuotedStruc        Structure
  HiLink lfeSymbol             Structure

  HiLink lfeDelimiter          Delimiter
  HiLink lfeConstant           Constant

  HiLink lfeComment            Comment
  HiLink lfeMultilineComment   Comment
  HiLink lfeTodo               Todo
  HiLink lfeNote               SpecialComment
  HiLink lfeError              Error

  HiLink lfeExtSyntax          Type
  HiLink lfeExtFunc            PreProc
  delcommand HiLink
endif

let b:current_syntax = "lfe"
let &cpo = s:cpo

