*! version 1.0.2  20sep2019  Ben Jann & Simon Seiler

program mindex, eclass
    version 11
    if replay() {
        if "`e(cmd)'" != "mindex" error 301
        Display `0'
        exit
    }
    local version : di "version " string(_caller()) ":"
    `version' Check_vceprefix_fw `0'
    if "`exit'" != "" {
        ereturn local cmdline `"mindex `0'"'
        exit
    }
    Check_vceprefix `0'
    `version' _vce_parserun mindex, noeqlist : `00'
    if "`s(exit)'" != "" {
        ereturn local cmdline `"mindex `0'"'
        exit
    }
    Estimate `0'
    if `"`e(generate)'"'!="" {
        describe `e(generate)'
    }
    ereturn local cmdline `"mindex `0'"'
end

program Check_vceprefix_fw, eclass
    version 11
    syntax anything(id="varlist") [if] [in] [fw iw pw aw] [, vce(str) ///
        Generate Generate2(passthru) * ]
    if `"`weight'"'!="fweight" exit
    if `"`vce'"'=="" exit
    Parse_vceprefix `vce'
    if "`vceprefix'"=="" exit
    if "`generate'`generate2'"!="" {
        di as err "{bf:generate()} not allowed with {bf:vce(`vceprefix')}"
        exit 198
    }
    local N = _N
    preserve
    di as txt "(temporarily expanding data for {bf:`vceprefix'})"
    tempname FW newobs
    qui gen double `FW' `exp'
    qui expand `FW', generate(`newobs')
    local version : di "version " string(_caller()) ":"
    `version' mindex `anything' `if' `in', vce(`vce') `options'
    qui keep if `newobs'==0
    mata: restore_and_promote_esample() // assuming sort order did not change!
    c_local exit exit
end

program Check_vceprefix
    _parse comma lhs 0 : 0
    syntax [, vce(str) CLuster(passthru) Generate Generate2(passthru) NOSE * ]
    local options `generate' `generate2' `options'
    if `"`vce'"'!="" {
        Parse_vceprefix `vce'
        local options vce(`vce') `options'
    }
    if "`vceprefix'"!="" {
        if "`generate'`generate2'"!="" {
            di as err "{bf:generate()} not allowed with {bf:vce(`vceprefix')}"
            exit 198
        }
        local nose nose
    }
    else {
        local options `cluster' `options'
    }
    c_local 00 `lhs', `nose' `options'
end

program Parse_vceprefix
    gettoken vce : 0, parse(" ,")
    local l = max(4, strlen(`"`vcetype'"')) 
    if      `"`vce'"'==substr("bootstrap", 1, `l') c_local vceprefix "bootstrap"
    else if `"`vce'"'==substr("jackknife", 1, `l') c_local vceprefix "jackknife"
    else c_local vceprefix
end

program Estimate, eclass
    // syntax
    gettoken depvar 0 : 0, parse(" ,(")
    _fv_check_depvar `depvar'
    gettoken tmp : 0, match(par)
    local simple = "`par'"!="("
    if `simple' {
        local syntax [varlist(numeric fv default=none)] /*
            */ [if] [in] [fw iw pw aw] [, /*
            */ over(varname numeric) Total /*
            */ contrast DECompose split REFgroup(str) /*
            */ CONTrols(varlist numeric fv) cmd(str) /*
            */ vce(passthru) CLuster(passthru) /*svy SUBpop(passthru)*/ /*
            */ NOCOLLapse COLLapse
    }
    else {
        local syntax anything(id="varlist") /*
            */ [if] [in] [fw iw pw aw] [, /*
            */ vce(passthru) CLuster(passthru) ROBUST /*svy SUBpop(passthru)*/ /*
            */ COLLapse
    }
    syntax `syntax' /*
        */ force NOSE /* 
        */ Generate Generate2(name) Replace /*
        */ NOIsily noHeader * ]
    if `simple' {
        local fvars `varlist'
        local varlist
        local fvars: list fvars | controls
        local rvars `controls'
        if `"`cmd'"'==""   local cmd mlogit
        if `"`fcmd'"'==""  local fcmd `"`cmd'"'
        if `"`rcmd'"'==""  local rcmd `"`cmd'"'
        if "`split'"!="" local decompose decompose
        if "`decompose'"!="" {
            local contrast contrast
            if "`over'"==""{
                di as err "{bf:decompose} requires {bf:over()}"
                exit 198
            }
            if `"`controls'"'!="" {
                di as err "{bf:decompose} not supported with {bf:controls()}"
                exit 498
            }
            if `"`cmd'"'!="mlogit" {
                di as err "{bf:decompose} only supported for {bf:mlogit}"
                exit 498
            }
            ParseRefgroup, ref(`refgroup') `total' // returns iref or lref
        }
        else if "`contrast'"!="" {
            if "`over'"==""{
                di as err "{bf:contrast} requires {bf:over()}"
                exit 198
            }
            ParseRefgroup, ref(`refgroup') `total' // returns iref or lref
        }
        else local iref .
        if "`total'"!="" {
            if "`over'"==""{
                di as err "{bf:total} only allowed with {bf:over()}"
                exit 198
            }
            local TOTAL .t
        }
        if "`nose'"!="" {
            foreach opt in vce cluster {
                if `"``opt''"'!="" {
                    di as err "{bf:nose} and {bf:`opt'()} not both allowed"
                    exit 198
                }
            }
        }
    }
    else {
        ParseEqs `anything' // returns fvars fcmd fopts rvars rcmd ropts mvars mcmd mopts
        local iref .
        if `"`fcmd'"'==""  local fcmd mlogit
        if `"`rcmd'"'==""  local rcmd mlogit
        if `"`mcmd'"'==""  local mcmd regress
    }
    fvrevar `fvars' `rvars' `mvars', list   // get base names of variables
    local xvars `"`r(varlist)'"'
    if "`generate2'"!="" local generate generate
    if "`generate'"!="" {
        if "`generate2'"=="" local generate2 "_M"
        if "`replace'"=="" confirm new variable `generate2'
    }
    if `"`vce'`cluster'`robust'"'!="" {
        Parse_vce, `vce' `cluster' `robust' // returns vce and clustvar
    }
    if `simple' _get_diopts diopts, `options'
    else        _get_diopts diopts options, `options'
    local diopts `diopts' `header'
    if "`weight'"!="" {
        local wgt "[`weight'`exp']"
    }
    if "`collapse'"!="" {
        if "`nocollapse'"!="" {
            di as err "{bf:collapse} and {bf:nocollapse} not both allowed"
            exit 198
        }
        if `"`wgt'`svy'"'!="" {
            di as err "{bf:collapse} not allowed with weights" // " or svy"
            exit 198
        }
    }
    
    // determine whether computations can be done on tabular data (much
    // faster than running mlogit)
    if "`force'"=="" {
        local rquick 1
        if `"`rcmd'"'!="mlogit" local rquick 0
        else if `"`rvars'"'!="" local rquick 0
        else if `"`ropts'"'!="" local rquick 0
        local fquick `rquick' // fquick requires rquick
        if `"`fcmd'"'!="mlogit" local fquick 0
        else if `"`fopts'"'!="" local fquick 0
        if `fquick' {
            fvrevar `fvars', list
            local tmp `"`r(varlist)'"'
            if `: list sizeof tmp'!=1 local fquick 0
            if `fquick' {
                if `"`fvars'"'!=`"i.`tmp'"' local fquick 0
                else {
                    local fvars0 `"`fvars'"'
                    local fvars `"`tmp'"'
                }
            }
        }
        if `rquick' & `fquick' {
            if `"`collapse'`nocollapse'`wgt'`svy'"'=="" {
                // ok to collapse data; specify -nocollapse- to prevent this
                local collapse collapse
            }
        }
    }
    else {
        local rquick 0
        local fquick 0
    }
    
    // determine whether M variables need to be generated
    local Mgen    1 // main M
    local Mtotgen 1 // M of total
    local Mcfgen  1 // counterfactual Ms
    if "`nose'"!="" {
        if `fquick' {
            if `simple' & "`generate'"=="" local Mgen 0
            local Mtotgen 0
        }
    }
    if `fquick' local Mcfgen  0
    
    // mark sample, count obs
    marksample touse
    local allvars `touse' `depvar' `xvars' `over' `clustvar'
    markout `allvars'
    if "`svy'"!="" svymarkout `touse'
    _nobs `touse' `wgt'
    local N = r(N)
    
    // collapse
    if "`collapse'"!="" {
        if "`generate'"=="" local sortindex
        else {
            tempvar sortindex
            qui gen long `sortindex' = _n
        }
        preserve
        tempvar Nobs
        sort `allvars' `sortindex'
        qui by `allvars': gen long `Nobs' = _N if _n==1 & `touse'
        qui keep if `Nobs'<.
        local wgt "[fweight=`Nobs']"
    }
    
    // collect outcomes and over-groups
    foreach v in depvar over {
        if "``v''"=="" continue
        capt assert (``v''==abs(int(``v''))) if `touse'
        if _rc {
            di as err "negative or noninteger values not allowed in {it:`v'}"
            exit 498
        }
        qui levelsof ``v'' if `touse', local(`v'_levels)
        local `v'_k: list sizeof `v'_levels
    }
    
    // over(): set up container for group sizes, determine reference for decomp
    if "`over'"=="" {
        local over_k 1
        local over_levels .
    }
    else {
        tempname _N
        mat `_N' = J(`over_k', 1, .)
        mat rown `_N' = `over_levels'
        if "`lref'"!="" {
            local iref: list posof "`lref'" in over_levels
            if `iref'==0 {
                di as err "refgroup(): `over'==`lref' does not exist"
                exit 198
            }
        }
        else if `iref'<. {
            if `iref'==0 { // total is reference
                local iref = `over_k' + 1 
            }
            else {
                if `iref'>`over_k' {
                    di as err "refgroup(): over-group #`iref' does not exist"
                    exit 198
                }
                local lref: word `iref' of `over_levels'
            }
        }
    }
    
    // computation based on tabular data: collect predictor levels
    if `fquick' {
        qui levelsof `fvars' if `touse', local(xlevels)
    }
    
    // compute M
    if `simple' {
        tempname bvec
        mat `bvec' = J(1, `over_k' + ("`total'"!=""), .)
    }
    if `Mgen' {
        tempvar M
        local Mvar `M'
        qui gen double `Mvar' = .
    }
    else local Mvar
    local models
    local i 0
    if "`noisily'"=="" di as txt "Estimating outcome models " _c
    foreach l in `over_levels' `TOTAL' {
        local ++i
        if `simple' local BOPT bvec(`bvec') bpos(`i')
        if `l'<. {
            qui `noisily' di _n as txt "-> `over'==`l'"
            _nobs `touse' if `over'==`l' `wgt' // get group size
            mat `_N'[`i', 1] = r(N)
            local andover " & `over'==`l'"
        }
        else {
            local andover ""
            if `l'==.t {
                qui `noisily' di _n as txt "-> total"
                if `Mtotgen' {
                    tempvar Mtot
                    local Mvar `Mtot'
                    qui gen double `Mvar' = .
                }
                else local Mvar
                if `simple' local BOPT bvec(`bvec') bpos(`i')
            }
        }
        if "`decompose'"!="" { // hold on to estimated models for decomposition
            tempname model
            local msave msave(`model')
            local models `models' `model'
        }
        EstimateM if `touse'`andover' `wgt', mvar(`Mvar') ///
            depvar(`depvar') outcomes(`depvar_levels') ///
            fcmd(`fcmd') fvars(`fvars') fopts(`fopts') ///
            rcmd(`rcmd') rvars(`rvars') ropts(`ropts') ///
            rquick(`rquick') fquick(`fquick') xlevels(`xlevels') ///
            `BOPT' `noisily' `msave'
    }
    if "`noisily'"=="" di as txt " done."
    
    // decomposition
    if "`decompose'"!="" {
        qui `noisily' di ""
        di as txt "Fitting counterfactual distributions " _c
        local bpos = colsof(`bvec')
        mat `bvec' = `bvec', ///
            J(1, (`over_k'-1 + ("`total'"!=""))*cond("`split'"!="",6,2), .)
        // get reference distribution and model
        local m1ref: word `iref' of `models'
        if `iref'<=`over_k' local ifref " & `over'==`lref'"
        else                local ifref
        if `fquick' {
            tempname p q pref qref
            mata: st_matrix("`pref'", colsum(st_matrix("`m1ref'")))
            mata: st_matrix("`qref'", rowsum(st_matrix("`m1ref'")))
            local QOPTREF q(`qref')
            local XLEVELS xvar(`fvars') xlevels(`xlevels')
        }
        else {
            tempname p pref
            GetProb `depvar' if `touse'`ifref' `wgt', p(`pref') ///
                outcomes(`depvar_levels')
            local QOPT
            local QOPTREF
            local XLEVELS
        }
        // compute counterfactual M
        if `Mcfgen' {
            local McfAr
            tempname McfA
            qui gen `McfA' = .
            if "`split'"!="" {
                local McfBr
                local McfCr
                tempname McfB McfC
                qui gen `McfB' = .
                qui gen `McfC' = .
            }
        }
        local MCF   A
        local POPT  p
        local POPTr pref
        local MOPT  m1ref
        local MOPTr m1
        if "`split'"!="" {
            local MCF   `MCF'   B     C
            local POPT  `POPT'  pref  pref
            local POPTr `POPTr' p     p
            local MOPT  `MOPT'  m1ref m1
            local MOPTr `MOPTr' m1    m1ref
        }
        local i 0
        foreach l in `over_levels' `TOTAL' {
            local ++i
            if `i'==`iref' continue
            local m1: word `i' of `models'
            if `l'<. local andover " & `over'==`l'"
            else     local andover ""
            if `fquick' {
                mata: st_matrix("`p'", colsum(st_matrix("`m1'")))
                mata: st_matrix("`q'", rowsum(st_matrix("`m1'")))
                local QOPT q(`q')
            }
            else {
                GetProb `depvar' if `touse'`andover' `wgt', p(`p') ///
                    outcomes(`depvar_levels')
            }
            local j 0
            foreach mcf of local MCF {
                local ++j
                // comparison group
                local ++bpos
                if `Mcfgen' {
                    if `l'<. local Mvar `Mcf`mcf''
                    else { // comparison group is total: need extra variable
                        tempname Mvar
                        qui gen `Mvar' = .
                        local Mcf`mcf' `Mcf`mcf'' `Mvar'
                    }
                }
                else local Mvar
                local popt: word `j' of `POPT'
                local mopt: word `j' of `MOPT'
                CounterfactualM `fquick' if `touse'`andover' `wgt', ///
                    mvar(`Mvar') depvar(`depvar') outcomes(`depvar_levels') ///
                    `XLEVELS' p(``popt'') m(``mopt'') `QOPT' ///
                    bvec(`bvec') bpos(`bpos')
                // reference group
                local ++bpos
                if `Mcfgen' {
                    tempname Mvar
                    qui gen `Mvar' = .
                    local Mcf`mcf'r `Mcf`mcf'r' `Mvar'
                }
                else local Mvar
                local popt: word `j' of `POPTr'
                local mopt: word `j' of `MOPTr'
                CounterfactualM `fquick' if `touse'`ifref' `wgt', ///
                    mvar(`Mvar') depvar(`depvar') outcomes(`depvar_levels') ///
                    `XLEVELS' p(``popt'') m(``mopt'') `QOPTREF' ///
                    bvec(`bvec') bpos(`bpos')
            }
            mat drop `p'
        }
        di as txt " done."
    }
    
    // results
    if `simple' {
        tempname b
        if "`over'"=="" {
            mat rename `bvec' `b'
            mat coln `b' = _cons
        }
        else {
            mata: Fillin_b("`b'", st_matrix("`bvec'"), "`over'", ///
                tokens(st_local("over_levels")), "`total'"!="", `iref', ///
                "`decompose'"!="", "`split'"!="")
        }
        if "`nose'"=="" {
            tempname V
            Compute_V `wgt', touse(`touse') `vce' b(`bvec') over(`over') ///
                levels(`over_levels') `total' iref(`iref') `decompose'  ///
                `split' n(`N') nn(`_N') m(`M' `Mtot') ///
                mcfa(`McfA')   mcfb(`McfB')   mcfc(`McfC') ///
                mcfar(`McfAr') mcfbr(`McfBr') mcfcr(`McfCr')
            mat `V' = e(V)
            if "`decompose'"!="" {
                /* this is needed because no variances are computed for the 
                decomposition components */
                local k = colsof(`b') - colsof(`V')
                mata: st_matrix(st_local("V"), ///
                    blockdiag(st_matrix(st_local("V")), J(`k', `k', 0)))
            }
            mat coleq `V' = ""
            mat coln `V' = `: colfullnames `b''
            mat roweq `V' = ""
            mat rown `V' = `: colfullnames `b''
            if "`clustvar'"!="" local N_clust = e(N_clust)
            local e_vce `"`e(vce)'"'
            local e_vcetype `"`e(vcetype)'"'
        }
    }
    else {
        local mopts `mopts' `vce' `options'
        qui `mcmd' `M' `mvars' if `touse' `wgt', `mopts'
    }
    
    // undo collapse
    if "`collapse'"!="" {
        local wgt
        if "`generate'"=="" {
            restore
        }
        else {
            tempfile tmp
            keep `sortindex' `M'
            qui save `tmp', replace
            restore
            merge 1:1 `sortindex' using `tmp', nogenerate noreport
            sort `allvars' `sortindex'
            qui by `allvars': replace `M' = `M'[1]
        }
    }
    
    // returns
    if `simple' {
        eret post `b' `V' `wgt', depname(`depvar') obs(`N') esample(`touse')
    }
    else if "`collapse'"!="" {
        ereturn repost, esample(`touse')
        eret local wtype ""
        eret local wexp ""
    }
    eret local title "M-index analysis"
    if `simple' {
        eret local vcetype `"`e_vcetype'"'
        eret local vce `"`e_vce'"'
        if "`clustvar'"!="" {
            eret scalar N_clust = `N_clust'
            eret local clustvar "`clustvar'"
        }
    }
    eret local rtable = cond(`rquick', "rtable", "")
    eret local ropts `"`ropts'"'
    eret local rvars `"`rvars'"'
    eret local rcmd `"`rcmd'"'
    eret local ftable = cond(`fquick', "ftable", "")
    eret local fopts `"`fopts'"'
    if `fquick' {
        local fvars `"`fvars0'"'
    }
    eret local fvars `"`fvars'"'
    eret local fcmd `"`fcmd'"'
    if !`simple' {
        eret local mopts `"`mopts'"'
        eret local mvars `"`mvars'"'
        eret local mcmd `"`mcmd'"'
    }
    eret local force `"`force'"'
    eret local collapse "`collapse'"
    if "`over'"!="" {
        if "`contrast'`decompose'`split'"!="" {
            eret scalar irefgroup = `iref'
            eret local refgroup "`lref'"
        }
        eret local split "`split'"
        eret local decompose "`decompose'"
        eret local contrast "`contrast'"
        eret local total "`total'"
        eret local over_levels "`over_levels'"
        eret local over "`over'"
        eret scalar N_over = `over_k'
        eret matrix _N = `_N'
    }
    eret local depvar "`depvar'"
    eret local cmd "mindex"
    
    // generate
    if "`generate'"!="" {
        capt confirm new variable `generate2'
        if _rc==1 exit 1 // user hit -break-
        else if _rc drop `generate2'
        rename `M' `generate2'
        lab var `generate2' "Local M-index"
        eret local generate "`generate2'"
    }
    
    // display
    Display, `diopts'
end

program ParseEqs
    local eqnames full reduced mindex
    forv i = 1/3 {
        gettoken eq 0 : 0, match(par)
        if "`par'"!="(" {
            di as err `"'`eq'' found where '(' expected"'
            exit 198
        }
        if strpos(`"`eq'"', ":") {  // get "eqname:" if existing
            gettoken eqnm eq : eq, parse(":")
            gettoken colon eq : eq, parse(":")
            local found 0
            foreach nm of local eqnames {
                local l = strlen(`"`eqnm'"')
                if `"`eqnm'"'==substr("`nm'", 1, `l') {
                    local eqnm "`nm'"
                    local found 1
                    continue, break
                }
            }
            if !`found' {
                di as err `"'`eqnm':' not allowed"'
                exit 198
            }
        }
        else gettoken eqnm eqname : eqnames
        local eqnames: list eqnames - eqnm
        local eqnm = substr("`eqnm'", 1, 1)
        ParseEq `eq'
        c_local `eqnm'vars `varlist'
        c_local `eqnm'cmd  `"`cmd'"'
        c_local `eqnm'opts `options'
        if `"`0'"'=="" continue, break
    }
    if `"`0'"'!="" {
        di as err `"'`0'' found where nothing expected"'
        exit 198
    }
end

program ParseEq
    syntax [varlist(numeric fv default=none)] [, cmd(str) * ]
    c_local varlist `varlist'
    c_local cmd `"`cmd'"'
    c_local options `options'
end

program ParseRefgroup
    syntax [, ref(str) total ]
    if `"`ref'"'=="" {
        if "`total'"=="" c_local iref 1
        else             c_local iref 0
        exit
    }
    if `"`ref'"'=="." {
        if "`total'"=="" {
            di as err "{bf:refgroup(.)} only allowed with {bf:total}"
            exit 198
        }
        c_local iref 0
        exit
    }
    if substr(`"`ref'"',1,1)=="#" {
        local i = substr(`"`ref'"',2,.)
        capt confirm integer number `i'
        if _rc==0 capt assert (`i'>0)
        if _rc {
            di as err `"refgroup(): '`ref'' not allowed"'
            exit 198
        }
        c_local iref `i'
        exit
    }
    capt confirm integer number `ref'
    if _rc==0 capt assert (`ref'>=0)
    if _rc {
        di as err `"refgroup(): '`ref'' not allowed"'
        exit 198
    }
    c_local lref `ref'
end

program Parse_vce
    syntax [, vce(str) CLuster(varname) robust ]
    if `"`vce'"'!="" {
        if "`cluster'"!="" {
            di as err "only one of {bf:cluster()} and {bf:vce()} allowed"
            exit 198
        }
        if "`robust'"!="" {
            di as err "only one of {bf:robust} and {bf:vce()} allowed"
            exit 198
        }
        _parse comma lhs rhs : vce
        gettoken vcetype args : lhs
        if `"`vcetype'"'==substr("cluster", 1, max(2, strlen(`"`vcetype'"'))) {
            capt n confirm numeric variable `args'
            if _rc {
                di as err "error in option {bf:vce()}"
                exit 198
            }
            if `: list sizeof args'>1 {
                di as err "too many variables specified"
                di as err "error in option {bf:vce()}"
                exit 198
            }
            local cluster `args'
        }
        c_local vce vce(`vce')
        c_local clustvar `cluster'
        exit
    }
    if "`cluster'"!="" {
        c_local vce vce(cluster `cluster')
        c_local clustvar `cluster'
        exit
    }
    if "`robust'"!="" {
        c_local vce vce(robust)
        c_local clustvar ""
    }
end

program EstimateM
    syntax if [fw iw pw aw], depvar(str) outcomes(str) ///
        fcmd(str) rcmd(str) fquick(str) rquick(str) [ mvar(str) xlevels(str) ///
        fvars(str) fopts(str) rvars(str) ropts(str) msave(str) NOIsily ///
        bvec(str) bpos(str) ]
    if "`weight'" != "" local wgt "[`weight'`exp']"
    marksample touse
    tempname pr pr0
    // reduced model
    qui `noisily' di as txt _n "Restricted model"
    if `rquick' {
        qui `noisily' di as txt "(no model needed; using observed proportions)"
        if !`fquick' {
            GetProb `depvar' if `touse' `wgt', p(`pr0') outcomes(`outcomes') `noisily'
        }
    }
    else {
        qui `noisily' `rcmd' `depvar' `rvars' if `touse' `wgt', `ropts'
        qui gen `pr0' = .
        foreach o of local outcomes {
            qui predict double `pr' if `touse' & `depvar'==`o', pr outcome(`o') 
            qui replace `pr0' = `pr' if `pr'<.
            drop `pr'
        }
    }
    if "`noisily'"=="" di as txt "." _c
    // full model
    qui `noisily' di as txt _n "Full model"
    if `fquick' {
        qui `noisily' di as txt "(no model needed; using observed proportions)"
        GetProb `fvars' `depvar' if `touse' `wgt', p(`pr') ///
            outcomes(`outcomes') xlevels(`xlevels') `noisily'
    }
    else {
        qui `noisily' `fcmd' `depvar' `fvars' if `touse' `wgt', `fopts'
    }
    if "`noisily'"=="" di as txt "." _c
    // compute M
    if `fquick' { // (implies rquick)
        if "`bvec'"!="" {
            mata: GetM(st_matrix("`pr'"))
            mat `bvec'[1,`bpos'] = r(M)
        }
        if "`mvar'"!="" {
            mata: Fillin_M("`mvar'", "`depvar'", "`fvars'", "`touse'", ///
                strtoreal(tokens(st_local("outcomes"))), ///
                strtoreal(tokens(st_local("xlevels"))), ///
                st_matrix("`pr'"))
        }
        if "`msave'"!="" {
            matrix rename `pr' `msave'
        }
        exit
    }
    if `rquick' {
        local i 0
        foreach o of local outcomes {
            local ++i
            qui predict double `pr' if `touse' & `depvar'==`o', pr outcome(`o') 
            qui replace `mvar' = cond(`pr'==0, 0, ln(`pr')) ///
                    - cond(`pr0'[1,`i']==0, 0, ln(`pr0'[1,`i'])) if `pr'<.
            drop `pr'
        }
    }
    else {
        foreach o of local outcomes {
            qui predict double `pr' if `touse' & `depvar'==`o', pr outcome(`o') 
            qui replace `mvar' = cond(`pr'==0, 0, ln(`pr')) ///
                               - cond(`pr0'==0, 0, ln(`pr0')) if `pr'<.
            drop `pr'
        }
    }
    if "`bvec'"!="" {
        Mean `mvar' if `touse' `wgt'
        mat `bvec'[1,`bpos'] = r(mean)
    }
    if "`msave'"!="" {
        _est hold `msave'
        drop `msave' // remove e(sample)
    }
end

program Mean
    syntax varname [if] [fw iw pw aw]
    if "`weight'"=="pweight" local weight aweight
    su `varlist' `if' [`weight'`exp'], meanonly
end

program CounterfactualM
    gettoken rake 0 : 0
    if `rake' {
        CounterfactualM_rake `0'
    }
    else {
        CounterfactualM_mlogit `0'
    }
    di as txt "." _c
end

program CounterfactualM_rake
    syntax if [fw iw pw aw], depvar(str) outcomes(str) ///
        xvar(str) xlevels(str) p(str) m(str) q(str) ///
        [ mvar(str) bvec(str) bpos(str) ]
    tempvar pr
    mata: EqualMarg("`pr'", "`m'", st_matrix("`q'"), st_matrix("`p'"))
    if "`bvec'"!="" {
        mata: GetM(st_matrix("`pr'"))
        mat `bvec'[1,`bpos'] = r(M)
    }
    if "`mvar'"!="" {
        marksample touse
        mata: Fillin_Mcf("`mvar'", "`xvar'", "`touse'", ///
            strtoreal(tokens(st_local("xlevels"))), ///
            st_matrix("`pr'"), st_matrix("`p'"))
    }
end

program CounterfactualM_mlogit
    syntax if [fw iw pw aw], mvar(str) depvar(str) outcomes(str) p(str) m(str) ///
        [ bvec(str) bpos(str) ]
    if "`weight'"=="pweight" local wgt "[aweight`exp']"
    else if "`weight'"!=""   local wgt "[`weight'`exp']"
    marksample touse
    // get counterfactual model
    _est unhold `m'
    _est hold `m', copy
    local nout = e(k_out)
    local ibaseout = e(ibaseout)
    // transform marginal distribution to coefficients
    tempname target
    ProbToCoefs `target' `nout' `ibaseout' `p'
    // update model intercepts until target distribution is reached
    tempvar pr current
    mat `current' = J(1,`nout', .)
    local iter 0
    local converged 0
    while (`iter'<100) {
        forv i=1/`nout' {
            qui predict double `pr' if `touse', pr outcome(#`i')
            su `pr' if `touse' `wgt', meanonly
            mat `current'[1,`i'] = r(mean)
            drop `pr'
        }
        ProbToCoefs `current' `nout' `ibaseout'
        if mreldif(`current',`target')<1e-7 {
            local converged 1
            continue, break
        }
        local ++iter
        UpdateConstants `target' `current' `nout' `ibaseout'
    }
    if `converged'==0 {
        di as err "counterfactual distribution algorithm did not converge"
        exit 499
    }
    // compute counterfactual m
    qui replace `mvar' = 0 if `touse'
    forv i=1/`nout' {
        qui predict double  `pr' if `touse', pr outcome(#`i')
        qui replace `mvar' = `mvar' + `pr' * (cond(`pr'==0, 0, ln(`pr')) ///
            - cond(`p'[1,`i']==0, 0, ln(`p'[1,`i']))) if `touse'
        drop `pr'
    }
    if "`bvec'"!="" {
        Mean `mvar' if `touse' `wgt'
        mat `bvec'[1,`bpos'] = r(mean)
    }
end

program GetProb
    syntax varlist if [fw iw pw aw], p(str) outcomes(str) [ xlevels(str) NOIsily ]
    if "`weight'"=="pweight" local weight aweight
    tempname Q R C
    if `: list sizeof varlist'>1 {
        qui `noisily' tab `varlist' `if' [`weight'`exp'], ///
            matcell(`Q') matcol(`C') matrow(`R')
        mata: CopyProb(1) // because some categories may be empty
    }
    else {
        qui `noisily' tab `varlist' `if' [`weight'`exp'], ///
            matcell(`Q') matrow(`R')
        mata: CopyProb(0) // because some categories may be empty
    }
end

program ProbToCoefs
    args b nout ibaseout p
    if "`p'"!="" {
        mat `b' = `p'
    }
    forv i=1/`nout' {
        if `i'==`ibaseout' continue
        mat `b'[1,`i'] = ln(`b'[1,`i']) - ln(`b'[1,`ibaseout'])
    }
    mat `b'[1,`ibaseout'] = 0
end

program UpdateConstants, eclass
    args target current nout ibaseout
    tempname b
    mat `b' = e(b)
    local eqnames `"`e(eqnames)'"'
    forv i=1/`nout' {
        gettoken eq eqnames : eqnames
        if `i'==`ibaseout' continue
        local c = colnumb(`b', "`eq':_cons")
        mat `b'[1,`c'] = `b'[1,`c'] + (`target'[1,`i'] - `current'[1,`i'])
    }
    eret repost b = `b'
end

program Compute_V
    syntax [fw iw pw aw], touse(str) b(str) [ vce(passthru) over(str) ///
        levels(str) total iref(str) decompose split n(str) nn(str) ///
        m(str) mcfa(str) mcfb(str) mcfc(str) mcfar(str) mcfbr(str) mcfcr(str) ]
    if "`weight'"!="" local wgt "[`weight'`exp']"
    // no over()
    if "`over'"=="" {
        qui mean `m' if `touse' `wgt', `vce'
        exit
    }
    // // over() without total or decomposition
    // if `iref'>=. & "`total'"=="" {
    //     qui mean `m' if `touse' `wgt', over(`over') `vce'
    //     exit
    // }
    // get group sizes (sum of weights)
    local k: list sizeof levels
    if inlist("`weight'", "", "fweight") {
        local W `n'
        local _W `nn'
    }
    else {
        Mean `touse' `wgt' if `touse'
        local W = r(sum_w)
        tempname _W
        mat `_W' = J(`k', 1, .)
        local i 0
        foreach l of local levels {
            local ++i
            Mean `touse' `wgt' if `touse' & `over'==`l'
            mat `_W'[`i',1] = r(sum_w)
        }
    }
    // over(): transform main M variable to influence functions
    local Mvar: word 1 of `m'
    local i 0
    foreach l of local levels {
        local ++i
        tempname IF`i'
        qui gen double `IF`i'' = cond(`over'==`l', (`Mvar'-`b'[1,`i']) * (`W'/`_W'[`i',1]), 0) if `touse'
        local IFs `IFs' `IF`i''
    }
    if "`total'"!="" {
        local ++i
        local IF`i': word 2 of `m'
        qui replace `IF`i'' = (`IF`i''-`b'[1,`i']) if `touse'
        local IFs `IFs' `IF`i''
    }
    // over() with total, without decomposition
    if `iref'>=. {
        qui mean `IFs' if `touse' `wgt', `vce'
        exit
    }
    // contrast, without decomposition
    forv ii=1/`=`k' + ("`total'"!="")' {
        if `ii'==`iref' {
            if `iref'<=`k' { // ref is not total
                tempname IF
                qui gen double `IF' = 0 if `touse'
                local IFs `IFs' `IF'
            }
            continue
        }
        tempname IF
        qui gen double `IF' = `IF`ii'' - `IF`iref'' if `touse'
        local IFs `IFs' `IF'
    }
    if "`decompose'"=="" {
        qui mean `IFs' if `touse' `wgt', `vce'
        exit
    }
    // decomposition: do not compute IFs because the SEs would not be valid
    qui mean `IFs' if `touse' `wgt', `vce'
end

program Display
    syntax [, noHeader * ]
    if "`header'"=="" {
        _coef_table_header, nomodeltest
        di ""
        local model `e(rcmd)' `e(depvar)' `e(rvars)'
        if `"`e(ropts)'"'!="" {
            local model `e(rcmd)' `e(depvar)' `e(rvars)', `e(ropts)'
        }
        di as txt "Reduced model: `model'"
        local model `e(fcmd)' `e(depvar)' `e(fvars)'
        if `"`e(fopts)'"'!="" {
            local model `e(fcmd)' `e(depvar)' `e(fvars)', `e(fopts)'
        }
        di as txt "Full model:    `model'"
        if `"`e(over)'"'!="" {
            di as txt "Over:          `e(over)'"
        }
    }
    di ""
    _coef_table, depname(M-index) `options'
end

version 11
mata:
mata set matastrict on

void restore_and_promote_esample()
{
    string scalar  touse
    real colvector esample
    
    touse = st_tempname()
    stata("qui gen byte "+touse+" = e(sample)")
    esample = st_data(., touse)
    stata("restore")
    st_store(., st_addvar("byte", touse), esample)
    stata("ereturn repost, esample("+touse+")")
}

void CopyProb(real scalar twoway)
{
    real scalar      i, ii, n, nn
    real matrix      P, PP
    real rowvector   C, CC
    real colvector   R, RR
    // algorithm assumes that tabulated categories are a subset of 
    // "outcomes" (and "xlevels"); returns error if assumption is violated

    // setup
    if (twoway) {
        P = st_matrix(st_local("Q")) / st_numscalar("r(N)")
        C = st_matrix(st_local("C"))
        R = st_matrix(st_local("R"))
        RR = strtoreal(tokens(st_local("xlevels")))'
    }
    else {
        P = st_matrix(st_local("Q"))' / st_numscalar("r(N)")
        C = st_matrix(st_local("R"))'
    }
    CC = strtoreal(tokens(st_local("outcomes")))
    // assign rows
    if (twoway) {
        nn = rows(RR)
        PP = J(nn, cols(P), 0)
        i = 1; n = rows(P)
        for (ii=1; ii<=nn; ii++) {
            if (i>n)          break 
            if (R[i]!=RR[ii]) continue
            PP[ii,] = P[i,]
            i++
        }
        if (i!=n+1) {
            display("{err}inconsistent data; this should never happen")
            exit(499)
        }
        swap(P, PP)
    }
    // assign columns
    nn = cols(CC)
    PP = J(rows(P), nn, 0)
    i = 1; n = cols(P)
    for (ii=1; ii<=nn; ii++) {
        if (i>n)          break 
        if (C[i]!=CC[ii]) continue
        PP[,ii] = P[,i]
        i++
    }
    if (i!=n+1) {
        display("{err}inconsistent data; this should never happen")
        exit(499)
    }
    st_matrix(st_local("p"), PP)
}

void GetM(real matrix P)
{
    real matrix    lnP
    real rowvector lnP0
    
    lnP0 = editmissing(ln(colsum(P)), 0)
    lnP  = editmissing(ln(P :/ rowsum(P)), 0)
    st_numscalar("r(M)", sum(P:*(lnP:-lnP0)))
}

void Fillin_M(string scalar Mnm, string scalar Ynm, string scalar Xnm, 
    string scalar touse, real vector Yvals, real vector Xvals, real matrix P)
{
    real scalar     i, offset
    real rowvector  minmax, map, lnP0
    real colvector  M, Y, X
    real matrix     lnP

    // create Y mapping
    minmax = minmax(Yvals); i = length(Yvals)
    Y = st_data(., Ynm, touse)
    if (minmax!=(1,i)) {
        offset = 1 - minmax[1]
        map = J(1, minmax[2]+offset, .)
        map[Yvals:+offset] = (1..i)
        Y = map[Y:+offset]
    }
    // create X mapping
    minmax = minmax(Xvals); i = length(Xvals)
    X = st_data(., Xnm, touse)
    if (minmax!=(1,i)) {
        offset = 1 - minmax[1]
        map = J(1, minmax[2]+offset, .)
        map[Xvals:+offset] = (1..i)
        X = map[X:+offset]
    }
    // compute M
    lnP0 = editmissing(ln(colsum(P)), 0)
    lnP = editmissing(ln(P :/ rowsum(P)), 0)
    i = rows(Y)
    M = J(i, 1, 0)
    for (; i; i--) M[i] = lnP[X[i], Y[i]] - lnP0[Y[i]]
    st_store(., Mnm, touse, M)
}

void Fillin_Mcf(string scalar Mnm, string scalar Xnm, 
    string scalar touse, real vector Xvals, 
    real matrix P1, real vector P0)
{
    real scalar     i, offset
    real rowvector  minmax, map, lnP0
    real colvector  M, X
    real matrix     P, lnP1
    
    // create X mapping
    minmax = minmax(Xvals); i = length(Xvals)
    X = st_data(., Xnm, touse)
    if (minmax!=(1,i)) {
        offset = 1 - minmax[1]
        map = J(1, minmax[2]+offset, .)
        map[Xvals:+offset] = (1..i)
        X = map[X:+offset]
    }
    // compute M
    P    = P1 :/ rowsum(P1)
    lnP1 = editmissing(ln(P), 0)
    lnP0 = editmissing(ln(P0), 0)
    i = rows(X)
    M = J(i, 1, 0)
    for (; i; i--) M[i] = sum(P[X[i],.]:*(lnP1[X[i],.] - lnP0))
    st_store(., Mnm, touse, M)
}

void EqualMarg(string scalar pr, string scalar pr0, real colvector v, real rowvector h)
{
    real scalar    i, c
    real matrix    p
    real colvector r
    
    p = st_matrix(pr0)
    i = c = 0
    r = rowsum(p)
    while (++i) {
        p = p :* (v :/ r)
        p = p :* (h :/ colsum(p))
        r = rowsum(p)
        if (mreldif((r',colsum(p)), (v',h)) < 1e-15) {
            c = 1
            break
        }
        if (i>500) break
    }
    if (!c) {
        display("{err}raking algorithm did not converge")
        exit(499)
    }
    st_matrix(pr, p)
}

void Fillin_b(string scalar bnm, real rowvector bvec, string scalar over,
    string rowvector levels, real scalar total, real scalar r, 
    real scalar decomp, real scalar split)
{
    real scalar    i, j, jj, k, l, c1, c1r, c2, c2r, c3, c3r
    real rowvector b
    string matrix  cstripe, coefs
    
    k = length(levels) + total
    coefs = levels' :+ ("." + over)
    if (total) coefs = coefs \ "Total"
    if (r>=.) { // no contrast/decomposition
        cstripe = (J(k, 1, ""), coefs)
        st_matrix(bnm, bvec)
        st_matrixcolstripe(bnm, cstripe)
        return
    }
    cstripe = (J(k, 1, "Level"), coefs)
    if (r<k) {
        l = k
        // coefs[r] = levels[r] + "o." + over
    }
    else {
        l = k - 1
        coefs = coefs[|1\l|]
    }
    cstripe = cstripe \ (J(l, 1, "Contrast"), coefs)
    if (decomp) { // decomposition
        cstripe = cstripe \ ((J(l, 1, "Internal") \ J(l, 1, "Marginal")),
            J(2, 1, coefs))
        if (split) { // subdivide contribution of margins
            cstripe = cstripe \ ((J(l, 1, "Marginal_Y") \ J(l, 1, "Marginal_X")), 
                J(2, 1, coefs))
        }
    }
    b = J(1, rows(cstripe), 0)
    b[|1\k|] = bvec[|1\k|]
    jj = k
    for (i=1; i<=k; i++) {
        if (i==r) {
            if (i<k) jj++ // ref is not total
            continue
        }
        b[++jj] = bvec[i] - bvec[r]
    }
    if (decomp==0) {
        st_matrix(bnm, b)
        st_matrixcolstripe(bnm, cstripe)
        return
    }
    j = k
    for (i=1; i<=k; i++) {
        if (i==r) {
            if (i<k) jj++ // ref is not total
            continue
        }
        if (i==r) continue // r is reference group
        c1  = ++j // i with internal structure of r
        c1r = ++j // r with internal structure of i
        b[++jj] = sum(bvec[(i, c1 , c1r, r)] :* J(1,2,(.5, -.5)))
        b[jj+l] = sum(bvec[(i, c1r, c1 , r)] :* J(1,2,(.5, -.5)))
        if (split) {
            c2  = ++j   // i with internal structure and Y-margins of r
            c2r = ++j   // r with internal structure and Y-margins of i
            c3  = ++j   // i with internal structure and Y-margins of r
            c3r = ++j   // r with internal structure and Y-margins of i
            b[jj+2*l] = sum(bvec[(i, c3 , c3r, r, c2r, c1r, c1, c2 )] :* J(1,4,(.25, -.25)))
            b[jj+3*l] = sum(bvec[(i, c2r, c2 , r, c3 , c1r, c1, c3r)] :* J(1,4,(.25, -.25)))
        }
    }
    st_matrix(bnm, b)
    st_matrixcolstripe(bnm, cstripe)
}

end

