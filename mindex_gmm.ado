*! version 1.0.0  22nov2019  Ben Jann & Simon Seiler

program mindex_gmm
    version 11
    _parse comma lhs 0 : 0
    syntax [, cmd(str) * ]
    if `"`cmd'"'=="mlogit" {
        mindex_gmm_mlogit `lhs', `options'
        exit
    }
    di as err `"vce(gmm): '`cmd'' not supported"'
    exit 499
end

program mindex_gmm_mlogit
    syntax varlist if [fw aw iw pw], at(name) /// 
        depvar(str) nout(str) out(str) ibase0(str) ibase1(str) ///
        [ derivatives(varlist) ]
    
    // residual equations
    quietly {
        local eq 0
        // compute mlogit probabilities and residuals
        // (m=0: restricted model, m=1: extended model)
        foreach m in 0 1 {
            local ibase `ibase`m''
            // obtain xb for each equation
            forv i = 1/`nout' {
                if `i'==`ibase' continue
                local ++eq
                tempvar xb`m'_`i'
                matrix score double `xb`m'_`i'' = `at' `if', eq(#`eq')
            }
            // compute denominator
            tempvar den`m'
            local den `den`m''
            gen double `den' = 1 `if'
            forv i = 1/`nout' {
                if `i'==`ibase' continue
                local xb `xb`m'_`i''
                replace `den' = /* from mlogit_p.ado: 
                    */ cond(`xb'<. & exp(`xb')>=., /*
                    */ cond(`den'<0,`den'-1,-1), `den'+exp(`xb')) `if'
            }
            // compute probability for each outcome
            forv i = 1/`nout' {
                tempname p`m'_`i'
                local p `p`m'_`i''
                local xb `xb`m'_`i''
                if `i'==`ibase' {
                    gen double `p' = /* from mlogit_p.ado: 
                        */ cond(`den'>0,1/`den',0) `if'
                }
                else {
                    gen double `p' = /* from mlogit_p.ado: 
                        */ cond(`den'>0,exp(`xb')/`den', /* 
                        */ cond(exp(`xb')<.,0,cond(`den'==-1,1,.))) `if'
                }
            }
            // mlogit residual equations
            forv i = 1/`nout' {
                if `i'==`ibase' continue
                local o: word `i' of `out'
                gettoken resid varlist : varlist
                replace `resid' = (`depvar'==`o') - `p`m'_`i'' `if'
            }
            // compute probability of observed outcome (needed for m-index)
            tempvar p`m'
            gen double `p`m'' = . `if'
            forv i = 1/`nout' {
                local o: word `i' of `out'
                replace `p`m'' = `p`m'_`i'' `if' & `depvar'==`o'
            }
        }
        // m-index residual equations
        local ++eq
        tempname M
        matrix score double `M' = `at' `if', eq(#`eq')
        gettoken resid varlist : varlist
        replace `resid' = (ln(`p1') - ln(`p0')) - `M' `if'
    }
    
    // derivatives
    if "`derivatives'"=="" exit
    quietly {
        // restricted model residual equations
        forv i = 1/`nout' {
            if `i'==`ibase0' continue
            // - restricted model parameter equations
            forv ii = 1/`nout' {
                if `ii'==`ibase0' continue
                gettoken deriv derivatives : derivatives
                if `i' == `ii' {
                    replace `deriv' = `p0_`ii'' * (`p0_`ii'' - 1) `if'
                }
                else {
                    replace `deriv' = `p0_`i'' * `p0_`ii'' `if'
                }
            }
            // - extended model parameter equations
            forv ii = 1/`nout' {
                if `ii'==`ibase1' continue
                gettoken deriv derivatives : derivatives
                // (derivative is zero)
            }
            // - m-index model equation
            gettoken deriv derivatives : derivatives
            // (derivative is zero)
        }
        // extended model residual equations
        forv i = 1/`nout' {
            if `i'==`ibase1' continue
            // - restricted model parameter equations
            forv ii = 1/`nout' {
                if `ii'==`ibase0' continue
                gettoken deriv derivatives : derivatives
                // (derivative is zero)
            }
            // - extended model parameter equations
            forv ii = 1/`nout' {
                if `ii'==`ibase1' continue
                gettoken deriv derivatives : derivatives
                if `i' == `ii' {
                    replace `deriv' = `p1_`ii'' * (`p1_`ii'' - 1) `if'
                }
                else {
                    replace `deriv' = `p1_`i'' * `p1_`ii'' `if'
                }
            }
            // - m-index model equation
            gettoken deriv derivatives : derivatives
            // (derivative is zero)
        }
        // m-index residual equations
        // - restricted model parameter equations
        forv i = 1/`nout' {
            if `i'==`ibase0' continue
            gettoken deriv derivatives : derivatives
            local o: word `i' of `out'
            replace `deriv' = `p0_`i'' - (`depvar'==`o') `if'
        }
        // - extended model parameter equations
        forv i = 1/`nout' {
            if `i'==`ibase1' continue
            gettoken deriv derivatives : derivatives
            local o: word `i' of `out'
            replace `deriv' = (`depvar'==`o') - `p1_`i'' `if'
        }
        // - m-index model equation
        gettoken deriv derivatives : derivatives
        replace `deriv' = -1 `if'
    }
end

