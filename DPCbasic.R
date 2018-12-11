library(tidyverse)
##  *16
##  *_start & _end　から使用期間 (lnof <length of> )、使用の有無 (yes)、入院何日目に使用したか (hospdy <hospital day>)という変数を作成
##  capture program drop dpclength
##  program dpclength
##  syntax namelist(min=2 max=2) [, Keepdtvarlists]
##  tokenize "`0'",parse(" ,")
##  capture drop c`2'_start c`2'_end
##  gen c`2'_start = `2'_start
##  gen c`2'_end   = `2'_end
##  
##  anythingtodate c`2'_start c`2'_end
##  gen lnof`1' = c`2'_end - c`2'_start + 1
##  sum lnof`1', detail
##  gen yes`1'=0
##  replace yes`1'=1 if lnof`1' >=1
##  replace yes`1'=0 if lnof`1' ==.
##  tab yes`1', missing
##  
##  capture confirm variable cdtadm
##  if _rc {
##  gen cdtadm = dtadm
##  anythingtodate cdtadm
##  }
##  
##  gen hospdy`1'=.
##  replace hospdy`1'= c`2'_start - cdtadm + 1 if yes`1' == 1
##  sum hospdy`1', detail
##  
##  if "`keepdtvarlists'" == "" {
##  drop c`2'_start
##  drop c`2'_end
##  }
##  end

#dpclength <- function(newvar,originalvar){
#}

##  capture program drop anythingtodate
##  program anythingtodate
##  syntax varlist
##  ds `varlist', not(format %tdCCYYNNDD)
##  ds `r(varlist)', has(type numeric)
##  capture numtodate `r(varlist)'
##  ds `varlist', has(type string)
##  capture texttodate `r(varlist)'
##  end


