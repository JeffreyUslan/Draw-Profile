****************************************************************************************************
* Name: WindowDescribe
* Description: Describes the windows
* Author(s): JD
* Date: ?
* Modified: ?
* Bonus Info: ?
****************************************************************************************************

program define WindowDescribe, rclass
syntax [anything(name=var)], [id(string) choice(integer 0) iter(integer 3) interval(string) graph_off time_var(string)]

	tempfile herp dee derp dset
	 save `dset', replace
	
	if `choice'==0 {
		*choice REALLY should be specified
		if strlen("`id'") {
			quietly WindowFind `var', id(`id') graph_off interval(`interval')
		}
		else {
			quietly WindowFind `var', graph_off interval(`interval')
		}
		local choice `r(choice)' 
	}
	
	if !strlen("`interval'") | "`interval'"=="daily" {
		local sec_of =60*60*24
		local gen_totsecs gen totsecs = 3600*hours + 60*minutes + seconds
		local interval day
	}
	else if "`interval'"=="weekly" {
		local sec_of =60*60*24*7
		local gen_totsecs gen totsecs = 86400*dow +3600*hours + 60*minutes + seconds
		local interval week
	}
	

	
	* munge up a site
	use `dset', clear
	capture ds , has(format tc)
	if _rc {
		display "There is no time variable in this dataset"
		exit
	}
	local time_var `r(varlist)'
	
	collapse `var', by(`time_var' `id')
	
	sort `id' `time_var' 
	
	 gen `var'_dur=1 if `var' & !missing(`var')
	 replace `var'_dur=1 if `var'_dur[_n-1]==1 & `var'_dur[_n+1]==1
	 replace `var'_dur=0 if missing(`var'_dur)

	capture generate int Draws=1 if `var'_dur>0 & !missing(`var'_dur) & `var'_dur[_n-1]==0
	replace Draws=0 if missing(Draws)
	
	keep `var' `time_var' Draws `id'



	
	gen str test=string(`time_var',"%tc")
		gen str stime	=substr(test,11,8)
		gen shours 		= substr(stime, 1, 2) 
		gen hours 		= real(shours) 
		gen sminutes	= substr(stime, 4, 2)
		gen minutes 	= real(sminutes)
		gen sseconds 	= substr(stime, 7, 2)
		gen seconds 	= real(sseconds)
		
			gen test2=substr(test,1,9)
			gen double date=date(test2,"DMY")
			gen week=week(date)
			gen dow=dow(date)

		
		`gen_totsecs'
		if "`interval'"=="week" {
			replace hours=24*dow+hours
		}
		preserve
			collapse Draws, by(hours)
			sort Draws hours
			local low_hour=hours[1]
		restore 
		
		*shifting the `interval' 
			local time_shift=3600*`low_hour'
			replace totsecs=totsecs-`time_shift'
			gen day=dofc(`time_var')
		replace `interval'=`interval'+1 if totsecs<0
			replace totsecs=totsecs+`sec_of' if totsecs<0
			quietly sum `interval'
			if strlen("`id'") {
				replace `interval'=`id'+`interval'/(`r(max)'+1)
			}
			
		egen `interval'_draws=sum(Draws), by(`interval')
		replace totsecs=1 if totsecs==0
		drop if `interval'_draws==0
		keep `var' Draws totsecs `interval'_draws `interval'

		save `derp', replace

	
*Using recomended window number determine window size
	tempfile banana
	use `derp', clear
	sort `interval' totsecs
	gen tmp=totsecs-totsecs[_n-1]
	sum tmp if tmp>00, d
	local min_span=`r(p50)'
	local window_length=`sec_of'/`choice'
	gen window_num=ceil(totsecs/`window_length')
	
	*we care about the draw here not the length of draw
	egen sec_min=min(totsecs) if Draws, by(`interval' window_num)
	egen sec_max=max(totsecs) if Draws, by(`interval' window_num)
	egen sec_mean=median(totsecs) if Draws, by(`interval' window_num)
	gen sec_span=sec_max-sec_min
	replace sec_span=sec_span+tmp
	
	
	collapse  `interval'_draws sec_span sec_mean (sum) Draws `var', by(window_num `interval')
	local cmd twoway (hist totsecs if Draws, yaxis(1) width(900)) 
		forv i = 1/`choice'{
			sum sec_mean if window_num==`i'
				local win_`i'_loc_mean =`r(mean)'
				local win_`i'_loc_sd =`r(sd)'
			local cmd `cmd' (function normalden(x,`win_`i'_loc_mean',`win_`i'_loc_sd'), ra(1 `sec_of') yaxis(2))
		}
	use `derp', clear
	if !strlen("`graph_off'") {
		`cmd' , name(hist_it_0, replace) legend(off)
	}
	
	quietly sum `var' if `var'>0
	local skip_thresh =`r(min)'*2
	local skip 0
	forv j =1/`iter' {
		*be wary overlapping windows end fenceposts
		*use `derp', clear
		
		forv i = 1/`choice'{
					local skipper: list skip & i
					if wordcount("`skipper'")>0{
						if `skipper'==`i'{
							continue
						 }
					}
		*local flip_`i' 0
		*should we vary the window length as windows are ommited? It does not currently do so
			local low_end_`i'=`win_`i'_loc_mean'-(`window_length'/2)
			local high_end_`i'=`win_`i'_loc_mean'+(`window_length'/2)
			
			*fencepost checks
			
			if `i'>1 {
				local last_i=`i'-1
				if `low_end_`i''<`high_end_`last_i''{
					local this=`low_end_`i''+((`high_end_`last_i''-`low_end_`i'')/2)
					local high_end_`last_i' =`this'
					local low_end_`i'=`this'+1
				}
			}
			
		
			
			di `low_end_`i''
			di `high_end_`i''
			
		}
		forv i = 1/`choice'{
					local skipper: list skip & i
					if wordcount("`skipper'")>0{
						if `skipper'==`i'{
							continue
						 }
					}
		* attempting to let windows wrap around midnight causes problems...


					gen window_num_`i'=`i' if totsecs>=`low_end_`i'' & totsecs<=`high_end_`i''
					gen bla1=1 if window_num_`i'==`i' & window_num_`i'[_n-1]==.
					gen bla2=sum(bla1)
					replace bla2=. if window_num_`i'==.
					egen sec_min_`i'=min(totsecs) if Draws & window_num_`i'==`i', by(`interval')
					egen sec_max_`i'=max(totsecs) if Draws &  window_num_`i'==`i', by(`interval')
					egen sec_mean_`i'=median(totsecs) if Draws, by(bla2)
					replace sec_mean_`i'=. if missing(bla2)
					gen sec_span_`i'=sec_max_`i'-sec_min_`i'
					
				
				drop bla1 bla2
				*adjust to time gran
				replace sec_span_`i'=sec_span_`i'+tmp
		}
		collapse  `interval'_draws sec_span* sec_mean* (sum) Draws `var', by(window_num* `interval')
		local cmd twoway (hist totsecs if Draws, yaxis(1) width(900)) 
			forv i = 1/`choice'{
					local skipper: list skip & i
					if wordcount("`skipper'")>0{
						if `skipper'==`i'{
							continue
						 }
					}
					*drop a window if the `var' is insignificant
						  sum `var' if window_num_`i'==`i'
								 if `r(mean)'<`skip_thresh'{
									local skip `skip' `i'
								 }
					 
						sum sec_mean_`i' if window_num_`i'==`i'
						local win_`i'_loc_mean =`r(mean)'
						if `win_`i'_loc_mean'>`sec_of'{
							local win_`i'_loc_mean =`win_`i'_loc_mean'-`sec_of'
						}
						local win_`i'_loc_sd =`r(sd)'

				local cmd `cmd' (function normalden(x,`win_`i'_loc_mean',`win_`i'_loc_sd'), ra(1 `sec_of') yscale(off) yaxis(2))
			}
			
			save `banana', replace
				use `derp', clear
			if !strlen("`graph_off'") {
				`cmd' , name(hist_it_`j', replace) subtitle("shifted `low_hour' hours") legend(off)
			}
			
	}
	
	if !strlen("`graph_off'") {
		*graph export ../graphs/Draw_windows/Site_`site'_draw_window.png, replace
	}
	use `banana', clear
	
	gen Span=.
	gen Interval=.
	forv i = 1/`choice'{
					local skipper: list skip & i
					if wordcount("`skipper'")>0{ 
						if `skipper'==`i'{
							continue
						 }
					}
		replace Span=sec_span_`i' if missing(Span)
		replace Interval=window_num_`i' if missing(Interval)
	}
	
						if wordcount("`skip'")>1{
							local choice=`choice'-wordcount("`skip'")+1
						 }
						 
	local skippy 0
	forv i = 1/`choice'{
					local skipper: list skip & i
					if wordcount("`skipper'")>0{
						if `skipper'==`i'{
							local skippy=`skippy'+1
						 }
					}
					local goku=`i'+`skippy'
					*unshift the time
					replace sec_mean_`goku'=sec_mean_`goku'+`time_shift'
					replace sec_mean_`goku'=sec_mean_`goku'-`sec_of' if sec_mean_`goku'>`sec_of'
		quietly ci sec_mean_`goku', level(90)
			return scalar int_`i'_mean=round(`r(mean)'/60,.1)
		quietly ci sec_span_`goku', level(90)
			return scalar int_`i'_span=round(`r(mean)'/60,.1)
		quietly ci `var' if Interval==`goku', level(90)
			return scalar int_`i'_`var'=round(`r(mean)',.01)
	}
	

	
	if !strlen("`graph_off'") {
		graph box Span, over(Interval) name(window_widths, replace) nooutside
		
		egen Win_`var'=sum(`var') , by(`interval' Interval)
		graph box Win_`var', over(Interval) name(window_`var', replace) nooutside
	}

	return scalar choice =`choice'


end


