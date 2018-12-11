*### DPCbasic.do
*### version 1.1.0
*### 2016-09-07
*### Nobuaki Michihata
*### Only for Stata 14 or later
*do "\\dbsvr\Public\Documents\StataProgramFiles\DPCbasic.do"

*### Changed history
* version 1.0.0
* 2015-12-04
* Nobuaki Michihata

* version 1.1.0
* 2016-09-01
* Nobuaki Michihata
* Do not overwrite original variables!
* Changed command names
* : dpccodecheck -> dpcdxcodeall, dpcnmcheck -> dpcdxnmall, dpcopecodecheck -> dpcopecodeall, dpcopenmcheck -> dpcopenmall
* : anyopecodecheck -> dpcanyopecode
* Added new commands (dpcdxcodemain,dpcdxcodepre,dpcdxcodepost),(dpcdxnmmain,dpcdxnmpre,dpcdxnmpost)
* dpclength内でも元々の日付変数は書き換えずにコピーして使用するようにした(元々の変数をコピーしてcを先頭に付加する)。
* Changed commands order

* Rules 2016-09-07 Updated
* 1. DPCに依存する特有のコマンドはdpcから始まるコマンド名とする
* 2. 元々ある変数は書き換えない。
* 3. 日付関連コマンドなどもともとある変数を書き換えるコマンドを使用する場合は変数をコピーして適応する。

************ 20 PROGRAMS 2016/09/07 *****************************
***** from 1 to 4 changed original variables   ******************
*1
capture program drop texttonum
program texttonum
syntax varlist
foreach i of local varlist {
gen `i'local = real(`i')
drop `i'
rename `i'local `i'
format `i' %20.0f
}
end

*2
capture program drop numtodate
program numtodate
syntax varlist
foreach i of local varlist {
gen `i'local = date(string(`i',"%8.0f"),"YMD")
drop `i'
rename `i'local `i'
format `i' %tdCCYYNNDD
}
end

*3
capture program drop texttodate
program texttodate
syntax varlist
foreach i of local varlist {
gen `i'local = date(string(real(`i'),"%8.0f"),"YMD")
drop `i'      
rename `i'local `i'
format `i' %tdCCYYNNDD
}
end

*4
capture program drop anythingtodate
program anythingtodate
syntax varlist
ds `varlist', not(format %tdCCYYNNDD)
ds `r(varlist)', has(type numeric)
capture numtodate `r(varlist)'
ds `varlist', has(type string)
capture texttodate `r(varlist)'
end
*****************************************************************

*5
capture program drop dpcdxcodeall
program dpcdxcodeall
syntax namelist (min=2)
local newvar = "`1'"
gen `1' = 0
gen `1'_suspect = 0
	foreach i of local namelist {
	local j= length("`i'")
capture {
   replace `newvar'         =1 if substr(dmain,1,`j') == "`i'" & ustrregexm(dmainnm,"疑")==0
   replace `newvar'_suspect =1 if substr(dmain,1,`j') == "`i'" & ustrregexm(dmainnm,"疑")==1
   replace `newvar'         =1 if substr(dadm, 1,`j') == "`i'" & ustrregexm(dadmnm, "疑")==0
   replace `newvar'_suspect =1 if substr(dadm, 1,`j') == "`i'" & ustrregexm(dadmnm, "疑")==1
   replace `newvar'         =1 if substr(dres1,1,`j') == "`i'" & ustrregexm(dres1nm,"疑")==0
   replace `newvar'_suspect =1 if substr(dres1,1,`j') == "`i'" & ustrregexm(dres1nm,"疑")==1
   replace `newvar'         =1 if substr(dres2,1,`j') == "`i'" & ustrregexm(dres2nm,"疑")==0
   replace `newvar'_suspect =1 if substr(dres2,1,`j') == "`i'" & ustrregexm(dres2nm,"疑")==1
   replace `newvar'         =1 if substr(dcin1,1,`j') == "`i'" & ustrregexm(dcin1nm,"疑")==0
   replace `newvar'_suspect =1 if substr(dcin1,1,`j') == "`i'" & ustrregexm(dcin1nm,"疑")==1
   replace `newvar'         =1 if substr(dcin2,1,`j') == "`i'" & ustrregexm(dcin2nm,"疑")==0
   replace `newvar'_suspect =1 if substr(dcin2,1,`j') == "`i'" & ustrregexm(dcin2nm,"疑")==1
   replace `newvar'         =1 if substr(dcin3,1,`j') == "`i'" & ustrregexm(dcin3nm,"疑")==0
   replace `newvar'_suspect =1 if substr(dcin3,1,`j') == "`i'" & ustrregexm(dcin3nm,"疑")==1
   replace `newvar'         =1 if substr(dcin4,1,`j') == "`i'" & ustrregexm(dcin4nm,"疑")==0
   replace `newvar'_suspect =1 if substr(dcin4,1,`j') == "`i'" & ustrregexm(dcin4nm,"疑")==1
   replace `newvar'         =1 if substr(dcc1, 1,`j') == "`i'" & ustrregexm(dcc1nm, "疑")==0
   replace `newvar'_suspect =1 if substr(dcc1, 1,`j') == "`i'" & ustrregexm(dcc1nm, "疑")==1
   replace `newvar'         =1 if substr(dcc2, 1,`j') == "`i'" & ustrregexm(dcc2nm, "疑")==0
   replace `newvar'_suspect =1 if substr(dcc2, 1,`j') == "`i'" & ustrregexm(dcc2nm, "疑")==1
   replace `newvar'         =1 if substr(dcc3, 1,`j') == "`i'" & ustrregexm(dcc3nm, "疑")==0
   replace `newvar'_suspect =1 if substr(dcc3, 1,`j') == "`i'" & ustrregexm(dcc3nm, "疑")==1
   replace `newvar'         =1 if substr(dcc4, 1,`j') == "`i'" & ustrregexm(dcc4nm, "疑")==0
   replace `newvar'_suspect =1 if substr(dcc4, 1,`j') == "`i'" & ustrregexm(dcc4nm, "疑")==1
}
}
tab `1' , missing
tab `1'_suspect , missing
end

*6
capture program drop dpcdxcodemain
program dpcdxcodemain
syntax namelist (min=2)
local newvar = "`1'"
gen `1' = 0
gen `1'_suspect = 0
	foreach i of local namelist {
	local j= length("`i'")
capture {
   replace `newvar'         =1 if substr(dmain,1,`j') == "`i'" & ustrregexm(dmainnm,"疑")==0
   replace `newvar'_suspect =1 if substr(dmain,1,`j') == "`i'" & ustrregexm(dmainnm,"疑")==1
   replace `newvar'         =1 if substr(dadm, 1,`j') == "`i'" & ustrregexm(dadmnm, "疑")==0
   replace `newvar'_suspect =1 if substr(dadm, 1,`j') == "`i'" & ustrregexm(dadmnm, "疑")==1
   replace `newvar'         =1 if substr(dres1,1,`j') == "`i'" & ustrregexm(dres1nm,"疑")==0
   replace `newvar'_suspect =1 if substr(dres1,1,`j') == "`i'" & ustrregexm(dres1nm,"疑")==1
   replace `newvar'         =1 if substr(dres2,1,`j') == "`i'" & ustrregexm(dres2nm,"疑")==0
   replace `newvar'_suspect =1 if substr(dres2,1,`j') == "`i'" & ustrregexm(dres2nm,"疑")==1
}
}
tab `1' , missing
tab `1'_suspect , missing
end

*7
capture program drop dpcdxcodepre
program dpcdxcodepre
syntax namelist (min=2)
local newvar = "`1'"
gen `1' = 0
gen `1'_suspect = 0
	foreach i of local namelist {
	local j= length("`i'")
capture {
   replace `newvar'         =1 if substr(dcin1,1,`j') == "`i'" & ustrregexm(dcin1nm,"疑")==0
   replace `newvar'_suspect =1 if substr(dcin1,1,`j') == "`i'" & ustrregexm(dcin1nm,"疑")==1
   replace `newvar'         =1 if substr(dcin2,1,`j') == "`i'" & ustrregexm(dcin2nm,"疑")==0
   replace `newvar'_suspect =1 if substr(dcin2,1,`j') == "`i'" & ustrregexm(dcin2nm,"疑")==1
   replace `newvar'         =1 if substr(dcin3,1,`j') == "`i'" & ustrregexm(dcin3nm,"疑")==0
   replace `newvar'_suspect =1 if substr(dcin3,1,`j') == "`i'" & ustrregexm(dcin3nm,"疑")==1
   replace `newvar'         =1 if substr(dcin4,1,`j') == "`i'" & ustrregexm(dcin4nm,"疑")==0
   replace `newvar'_suspect =1 if substr(dcin4,1,`j') == "`i'" & ustrregexm(dcin4nm,"疑")==1
}
}
tab `1' , missing
tab `1'_suspect , missing
end

*8
capture program drop dpcdxcodepost
program dpcdxcodepost
syntax namelist (min=2)
local newvar = "`1'"
gen `1' = 0
gen `1'_suspect = 0
	foreach i of local namelist {
	local j= length("`i'")
capture {
   replace `newvar'         =1 if substr(dcc1, 1,`j') == "`i'" & ustrregexm(dcc1nm, "疑")==0
   replace `newvar'_suspect =1 if substr(dcc1, 1,`j') == "`i'" & ustrregexm(dcc1nm, "疑")==1
   replace `newvar'         =1 if substr(dcc2, 1,`j') == "`i'" & ustrregexm(dcc2nm, "疑")==0
   replace `newvar'_suspect =1 if substr(dcc2, 1,`j') == "`i'" & ustrregexm(dcc2nm, "疑")==1
   replace `newvar'         =1 if substr(dcc3, 1,`j') == "`i'" & ustrregexm(dcc3nm, "疑")==0
   replace `newvar'_suspect =1 if substr(dcc3, 1,`j') == "`i'" & ustrregexm(dcc3nm, "疑")==1
   replace `newvar'         =1 if substr(dcc4, 1,`j') == "`i'" & ustrregexm(dcc4nm, "疑")==0
   replace `newvar'_suspect =1 if substr(dcc4, 1,`j') == "`i'" & ustrregexm(dcc4nm, "疑")==1
}
}
tab `1' , missing
tab `1'_suspect , missing
end

*9
capture program drop dpcdxnmall
program dpcdxnmall
syntax namelist (min=2)
local newvar = "`1'"
gen `1' = 0
gen `1'_suspect = 0
	foreach i of local namelist {
capture {
   replace `newvar'         =1 if ustrregexm(dmainnm,"`i'") == 1  & ustrregexm(dmainnm,"疑")==0
   replace `newvar'_suspect =1 if ustrregexm(dmainnm,"`i'") == 1  & ustrregexm(dmainnm,"疑")==1
   replace `newvar'         =1 if ustrregexm(dadmnm, "`i'") == 1  & ustrregexm(dadmnm, "疑")==0
   replace `newvar'_suspect =1 if ustrregexm(dadmnm, "`i'") == 1  & ustrregexm(dadmnm, "疑")==1
   replace `newvar'         =1 if ustrregexm(dres1nm,"`i'") == 1  & ustrregexm(dres1nm,"疑")==0
   replace `newvar'_suspect =1 if ustrregexm(dres1nm,"`i'") == 1  & ustrregexm(dres1nm,"疑")==1
   replace `newvar'         =1 if ustrregexm(dres2nm,"`i'") == 1  & ustrregexm(dres2nm,"疑")==0
   replace `newvar'_suspect =1 if ustrregexm(dres2nm,"`i'") == 1  & ustrregexm(dres2nm,"疑")==1
   replace `newvar'         =1 if ustrregexm(dcin1nm,"`i'") == 1  & ustrregexm(dcin1nm,"疑")==0
   replace `newvar'_suspect =1 if ustrregexm(dcin1nm,"`i'") == 1  & ustrregexm(dcin1nm,"疑")==1
   replace `newvar'         =1 if ustrregexm(dcin2nm,"`i'") == 1  & ustrregexm(dcin2nm,"疑")==0
   replace `newvar'_suspect =1 if ustrregexm(dcin2nm,"`i'") == 1  & ustrregexm(dcin2nm,"疑")==1
   replace `newvar'         =1 if ustrregexm(dcin3nm,"`i'") == 1  & ustrregexm(dcin3nm,"疑")==0
   replace `newvar'_suspect =1 if ustrregexm(dcin3nm,"`i'") == 1  & ustrregexm(dcin3nm,"疑")==1
   replace `newvar'         =1 if ustrregexm(dcin4nm,"`i'") == 1  & ustrregexm(dcin4nm,"疑")==0
   replace `newvar'_suspect =1 if ustrregexm(dcin4nm,"`i'") == 1  & ustrregexm(dcin4nm,"疑")==1
   replace `newvar'         =1 if ustrregexm(dcc1nm, "`i'") == 1  & ustrregexm(dcc1nm, "疑")==0
   replace `newvar'_suspect =1 if ustrregexm(dcc1nm, "`i'") == 1  & ustrregexm(dcc1nm, "疑")==1
   replace `newvar'         =1 if ustrregexm(dcc2nm, "`i'") == 1  & ustrregexm(dcc2nm, "疑")==0
   replace `newvar'_suspect =1 if ustrregexm(dcc2nm, "`i'") == 1  & ustrregexm(dcc2nm, "疑")==1
   replace `newvar'         =1 if ustrregexm(dcc3nm, "`i'") == 1  & ustrregexm(dcc3nm, "疑")==0
   replace `newvar'_suspect =1 if ustrregexm(dcc3nm, "`i'") == 1  & ustrregexm(dcc3nm, "疑")==1
   replace `newvar'         =1 if ustrregexm(dcc4nm, "`i'") == 1  & ustrregexm(dcc4nm, "疑")==0
   replace `newvar'_suspect =1 if ustrregexm(dcc4nm, "`i'") == 1  & ustrregexm(dcc4nm, "疑")==1
}
}
tab `1' , missing
tab `1'_suspect , missing
end

*10
capture program drop dpcdxnmmain
program dpcdxnmmain
syntax namelist (min=2)
local newvar = "`1'"
gen `1' = 0
gen `1'_suspect = 0
	foreach i of local namelist {
capture {
   replace `newvar'         =1 if ustrregexm(dmainnm,"`i'") == 1  & ustrregexm(dmainnm,"疑")==0
   replace `newvar'_suspect =1 if ustrregexm(dmainnm,"`i'") == 1  & ustrregexm(dmainnm,"疑")==1
   replace `newvar'         =1 if ustrregexm(dadmnm, "`i'") == 1  & ustrregexm(dadmnm, "疑")==0
   replace `newvar'_suspect =1 if ustrregexm(dadmnm, "`i'") == 1  & ustrregexm(dadmnm, "疑")==1
   replace `newvar'         =1 if ustrregexm(dres1nm,"`i'") == 1  & ustrregexm(dres1nm,"疑")==0
   replace `newvar'_suspect =1 if ustrregexm(dres1nm,"`i'") == 1  & ustrregexm(dres1nm,"疑")==1
   replace `newvar'         =1 if ustrregexm(dres2nm,"`i'") == 1  & ustrregexm(dres2nm,"疑")==0
   replace `newvar'_suspect =1 if ustrregexm(dres2nm,"`i'") == 1  & ustrregexm(dres2nm,"疑")==1
}
}
tab `1' , missing
tab `1'_suspect , missing
end

*11
capture program drop dpcdxnmpre
program dpcdxnmpre
syntax namelist (min=2)
local newvar = "`1'"
gen `1' = 0
gen `1'_suspect = 0
	foreach i of local namelist {
capture {
   replace `newvar'         =1 if ustrregexm(dcin1nm,"`i'") == 1  & ustrregexm(dcin1nm,"疑")==0
   replace `newvar'_suspect =1 if ustrregexm(dcin1nm,"`i'") == 1  & ustrregexm(dcin1nm,"疑")==1
   replace `newvar'         =1 if ustrregexm(dcin2nm,"`i'") == 1  & ustrregexm(dcin2nm,"疑")==0
   replace `newvar'_suspect =1 if ustrregexm(dcin2nm,"`i'") == 1  & ustrregexm(dcin2nm,"疑")==1
   replace `newvar'         =1 if ustrregexm(dcin3nm,"`i'") == 1  & ustrregexm(dcin3nm,"疑")==0
   replace `newvar'_suspect =1 if ustrregexm(dcin3nm,"`i'") == 1  & ustrregexm(dcin3nm,"疑")==1
   replace `newvar'         =1 if ustrregexm(dcin4nm,"`i'") == 1  & ustrregexm(dcin4nm,"疑")==0
   replace `newvar'_suspect =1 if ustrregexm(dcin4nm,"`i'") == 1  & ustrregexm(dcin4nm,"疑")==1
}
}
tab `1' , missing
tab `1'_suspect , missing
end

*12
capture program drop dpcdxnmpost
program dpcdxnmpost
syntax namelist (min=2)
local newvar = "`1'"
gen `1' = 0
gen `1'_suspect = 0
	foreach i of local namelist {
capture {
   replace `newvar'         =1 if ustrregexm(dcc1nm, "`i'") == 1  & ustrregexm(dcc1nm, "疑")==0
   replace `newvar'_suspect =1 if ustrregexm(dcc1nm, "`i'") == 1  & ustrregexm(dcc1nm, "疑")==1
   replace `newvar'         =1 if ustrregexm(dcc2nm, "`i'") == 1  & ustrregexm(dcc2nm, "疑")==0
   replace `newvar'_suspect =1 if ustrregexm(dcc2nm, "`i'") == 1  & ustrregexm(dcc2nm, "疑")==1
   replace `newvar'         =1 if ustrregexm(dcc3nm, "`i'") == 1  & ustrregexm(dcc3nm, "疑")==0
   replace `newvar'_suspect =1 if ustrregexm(dcc3nm, "`i'") == 1  & ustrregexm(dcc3nm, "疑")==1
   replace `newvar'         =1 if ustrregexm(dcc4nm, "`i'") == 1  & ustrregexm(dcc4nm, "疑")==0
   replace `newvar'_suspect =1 if ustrregexm(dcc4nm, "`i'") == 1  & ustrregexm(dcc4nm, "疑")==1
}
}
tab `1' , missing
tab `1'_suspect , missing
end

*13
capture program drop dpcopecodeall
program dpcopecodeall
syntax namelist (min=2)
local newvar = "`1'"
gen `1' = 0
	foreach i of local namelist {
	local j= length("`i'")
capture {
   replace `newvar'=1 if substr(opek1,1,`j') == "`i'"
   replace `newvar'=1 if substr(opek2,1,`j') == "`i'"
   replace `newvar'=1 if substr(opek3,1,`j') == "`i'"
   replace `newvar'=1 if substr(opek4,1,`j') == "`i'"
   replace `newvar'=1 if substr(opek5,1,`j') == "`i'"
}
}
tab `1' , missing
end

*14
capture program drop dpcopenmall
program dpcopenmall
syntax namelist (min=2)
local newvar = "`1'"
gen `1' = 0
	foreach i of local namelist {
	local j= length("`i'")
capture {
   replace `newvar'=1 if ustrregexm(openm1,"`i'") == 1
   replace `newvar'=1 if ustrregexm(openm2,"`i'") == 1
   replace `newvar'=1 if ustrregexm(openm3,"`i'") == 1
   replace `newvar'=1 if ustrregexm(openm4,"`i'") == 1
   replace `newvar'=1 if ustrregexm(openm5,"`i'") == 1
}
}
tab `1' , missing
end

*15
capture program drop dpcanyopecode
program dpcanyopecode
gen anyope = 0
capture {
   forvalues i= 1(1)5 {
   replace anyope =1 if opek`i' != ""
   replace anyope =0 if substr(opek`i' ,1,1) == " "
   replace anyope =0 if substr(opek`i' ,1,1) == "　"
   replace anyope =0 if substr(opek`i' ,1,4) == "K920"
   replace anyope =0 if substr(opek`i' ,1,4) == "K276"
}
}
tab anyope , missing
end

*16
*_start & _end　から使用期間 (lnof <length of> )、使用の有無 (yes)、入院何日目に使用したか (hospdy <hospital day>)という変数を作成
capture program drop dpclength
program dpclength
syntax namelist(min=2 max=2) [, Keepdtvarlists]
tokenize "`0'",parse(" ,")
capture drop c`2'_start c`2'_end
gen c`2'_start = `2'_start
gen c`2'_end   = `2'_end

anythingtodate c`2'_start c`2'_end
gen lnof`1' = c`2'_end - c`2'_start + 1
sum lnof`1', detail
gen yes`1'=0
replace yes`1'=1 if lnof`1' >=1
replace yes`1'=0 if lnof`1' ==.
tab yes`1', missing

capture confirm variable cdtadm
if _rc {
gen cdtadm = dtadm
anythingtodate cdtadm
}

gen hospdy`1'=.
replace hospdy`1'= c`2'_start - cdtadm + 1 if yes`1' == 1
sum hospdy`1', detail

if "`keepdtvarlists'" == "" {
drop c`2'_start
drop c`2'_end
}
end

*17
*PSmatch
capture program drop dpcpsmatch
program dpcpsmatch

qui {
syntax varlist , OUTcome(varlist) [CALiper(real 0)] [NOREPLacement]
if regexm("`0'","^(.+)\,.*") local varlistmodified = regexs(1) 
*di regexr("treat _Imale_* _Iagey_*","_I(.*?)_\*","\1")
local varlistoriginal= subinstr(subinstr("`varlistmodified'","_I","i.",.),"_*","",.)

// record sort order
tempvar caseorder
gen long `caseorder' = _n
capture drop forrandomsort
set seed 20151204
gen forrandomsort=runiform()
sort forrandomsort
local varlistcov = "`varlistmodified'"
tokenize `varlistcov'
local tvar="`1'"
macro shift
local varlistcov = "`*'"

local varlistoricov = "`varlistoriginal'"
tokenize `varlistoricov'
local tvar="`1'"
macro shift
local varlistoricov = "`*'"

di "varlistcov show"
di "original"
di "`varlistoriginal'"
di "`varlistmodified'"
di "cov"
di "`varlistcov'"
di "tvar"
di "`tvar'"
di "`varlistoricov'"

// Check that the command was valid, bail if it wasn't
tab `tvar'
  local levels = r(r)
  if `levels' > 2 {
    di as error "The first variable in the varlist must be a dichotomous exposure variable"
    exit
	}
tab `outcome'
  local levels = r(r)
  if `levels' > 2 {
    di as error "The outcome variable must be a dichotomous variable for dpcpsmatch"
    exit  
  }
di "xi:psmatch2 `varlistoriginal',logit"
xi:psmatch2 `varlistoriginal',logit
sum _pscore
}

local cal1 = r(sd)
lroc
local cal2=`cal1'*`caliper'
di "caliper= " `cal2'
di "C-statics=" r(area)
di "xi:psmatch2 `varlistoriginal', logit out(`outcome') cal(`cal2') `noreplacement'"
xi:psmatch2 `varlistoriginal', logit out(`outcome') cal(`cal2') `noreplacement'
di "* Unadjusted"
qui{
tab `tvar' `outcome', matcell(table)
local a= table[1,1]
local b= table[1,2]
local c= table[2,1]
local d= table[2,2]
}
di "xi:pbalchk `tvar' `varlistoricov'"
capture noisily xi:pbalchk `tvar' `varlistoricov'
csi `d' `b' `c' `a'     
preserve
expand _weight
drop if _weight ==.
di "* Adjusted"
qui{
tab `tvar' `outcome', matcell(table)
local a1= table[1,1]
local b1= table[1,2]
local c1= table[2,1]
local d1= table[2,2]
}
di "xi:pbalchk `tvar' `varlistoricov'"
capture noisily xi:pbalchk `tvar' `varlistoricov'
csi `d1' `b1' `c1' `a1'  
restore
capture drop foriptwcalc
gen foriptwcalc= 1/(`tvar'*_pscore+(1-`tvar')*(1-_pscore))
di "* IPTW"
qui{
tabulate `tvar' `outcome' [iweight=foriptwcalc],matcell(table2)
local a2= round(table2[1,1])
local b2= round(table2[1,2])
local c2= round(table2[2,1])
local d2= round(table2[2,2])
}
csi `d2' `b2' `c2' `a2'
sort `caseorder'  
drop forrandomsort
end

*18
* Standardized difference 連続変数の場合
capture program drop stddiffcont
program stddiffcont
syntax varlist
qui{
local tvar="`1'"
tokenize `varlist'
macro shift
local varlist ="`*'"
tab `tvar'
  local levels = r(r)
  if `levels' > 2 {
    di as error "The first variable in the varlist must be a dichotomous exposure variable"
    exit
	}
}
di "Variable : Mean in treated" _column(32) "Mean in Untreated" _column(52) "Standardized diff." _column(81) "95%CI "
di _dup(95) "-"
foreach i of varlist `varlist' {
qui {
sum `i' if `tvar' == 0
local n0 = r(N)
local tm0 = r(mean)
local tv0 = r(Var)
sum `i' if `tvar' == 1
local n1 = r(N)
local tm1 = r(mean)
local tv1 = r(Var)
local sdiff = (`tm0'-`tm1')/sqrt((`tv0' + `tv1')/2)
local sdsigma = sqrt((`n0'+`n1')/(`n0'*`n1')+`sdiff'^2/(2*(`n0'+`n1')))
}
di "`i'" _column(10) ": "`tm1' _column(32) `tm0' _column(52)  `sdiff' _column(72)   "(" `sdiff' -1.96 * `sdsigma' ", "`sdiff' + 1.96 * `sdsigma' ")"
}
di _dup(95) "-"
end
       
*19
* Standardized difference カテゴリカル変数の場合
capture program drop stddiffcat
program stddiffcat
syntax varlist
qui{
local tvar="`1'"
tokenize `varlist'
macro shift
local varlist ="`*'"
tab `tvar'
  local levels = r(r)
  if `levels' > 2 {
    di as error "The first variable in the varlist must be a dichotomous exposure variable"
    exit
	}
}
di "Variable : Mean in treated" _column(32) "Mean in Untreated" _column(52) "Standardized diff." _column(81) "95%CI "
di _dup(95) "-"
foreach i of varlist `varlist' {
qui {
sum `i' if `tvar' == 0
local n0 = r(N)
local tm0 = r(mean)
sum `i' if `tvar' == 1
local n1 = r(N)
local tm1 = r(mean)
local sdiff =(`tm0'-`tm1')/sqrt((`tm0'*(1-`tm0') + `tm1'*(1-`tm1'))/2)
local sdsigma = sqrt((`n0'+`n1')/(`n0'*`n1')+`sdiff'^2/(2*(`n0'+`n1')))
}
di "`i'" _column(10) ": "`tm1' _column(32) `tm0' _column(52)  `sdiff' _column(72)   "(" `sdiff' -1.96 * `sdsigma' ", "`sdiff' + 1.96 * `sdsigma' ")"
}
di _dup(95) "-"
end

*20
capture program drop dpcstart
program dpcstart
capture ds
local initialvarlist = `"`r(varlist)'"'
di "`initialvarlist'"

*>1.	dtadm, dtdisc, dtbirthをcdtadm, cdtdisc, cdtbirthにコピーし、文字列から日付に変更
capture drop cdtadm cdtdisc cdtbirth
gen cdtadm   = dtadm
gen cdtdisc  = dtdisc
gen cdtbirth = dtbirth
anythingtodate cdtadm cdtdisc cdtbirth 

*>2.	出生時体重（brthw）、出生時妊娠週数（brthwk）、身長（height）、体重（weight）をcから始まる同名変数にコピーし文字列から数値に変更
capture drop cbrthw cbrthwk cheight cweight
gen cbrthw  = brthw
gen cbrthwk = brthwk
gen cheight = height
gen cweight = weight

capture destring cbrthw , replace force
capture destring cbrthwk, replace force
capture destring cheight, replace force
capture destring cweight, replace force

*>3.	年齢（agey小数点以下切捨て, ageyd小数点以下残す）と入院期間（los）変数の作成
capture drop agey ageyd los
gen agey=int((cdtadm-cdtbirth)/365.25)
gen ageyd=(cdtadm-cdtbirth)/365.25
gen los=cdtdisc-cdtadm+1

*>4.	hospcd, ptidから同病院再入院患者を見つけるunique ID (uid, newid)の作成
capture drop uid dup_adm newid
gen uid= hospcd+ptid
duplicates tag uid, gen(dup_adm)
sort uid cdtdisc
by uid:gen newid = 1 if _n==1
replace newid = sum(newid)

*>5.	入院回数(nadmission)、最終入院(ladmission)変数の作成
capture drop nadmission ladmission
by uid:gen nadmission = 1 if _n==1
capture sum dup_adm
local m=r(max)+1
forvalues i=2(1)`m'{
by uid:replace nadmission = `i' if _n==`i'
}
gen ladmission=0
by uid:replace ladmission=1 if _n==_N

*>6.	死亡退院(death)変数の作成
capture drop death
gen death=0
capture replace death=1 if outcome== "6" | outcome== "7"
capture replace death=1 if outcome== 6 | outcome== 7

*>7.	性別(sex)変数からmale (male==1, female==0)の作成
capture drop male
gen male=.
capture replace male=0 if sex=="2"
capture replace male=0 if sex==2
capture replace male=1 if sex=="1"
capture replace male=1 if sex==1

*>8.	最後に変数順の調整
order `initialvarlist'
end
************ END OF PROGRAMS *****************************

