****************************************************************************************************
* Name: hpwhWindow
* Description: ?
* Author(s): JD
* Date: ?
* Modified: ?
* Bonus Info: ?
****************************************************************************************************

program define hpwhWindow, rclass
syntax [anything(name=site)], [choice(integer 1) iter(integer 3) graph_off]


capture {
clear
cdx hpwhF
tempfile herp dee derp

*Set up graph dataset
gen windows=.
gen mean_Draw=.
gen se_Draw=.
gen upper_Draw=.
gen lower_Draw=.
gen window_retention=.
save `herp', replace


* munge up a site
hpwhReshape `site', no
sort readTime
	gen Flow2=sum(Flow)
	gen Flow3=floor(Flow2)
	gen Flow4=Flow3-Flow3[_n-1]
	replace Flow=Flow4
	drop Flow2 Flow3 Flow4
gen time_diff=readTime-readTime[_n-1]
	 drop if time_diff==0 | time_diff==300000
		 gen Flow_dur=1 if Flow & !missing(Flow)
	 replace Flow_dur=1 if Flow_dur[_n-1]==1 & Flow_dur[_n+1]==1
	 replace Flow_dur=0 if missing(Flow_dur)
	capture generate int Draws=1 if Flow_dur>0 & !missing(Flow_dur) & Flow_dur[_n-1]==0
	replace Draws=0 if missing(Draws)
	keep Flow Draws readTime

	gen str test=string(readTime,"%tc")
		gen str stime=substr(test,11,8)
		gen shours = substr(stime, 1, 2) 
		gen hours = real(shours) 
		gen sminutes = substr(stime, 4, 2)
		gen minutes = real(sminutes)
		gen sseconds = substr(stime, 7, 2)
		gen seconds = real(sseconds)
		gen totsecs = 3600*hours + 60*minutes + seconds
		preserve
			collapse Draws, by(hours)
			sort Draws hours
			local low_hour=hours[1]
		restore 
		
		
			*shifting the day 3 hours
			local time_shift=3600*`low_hour'
			replace totsecs=totsecs-`time_shift'
		gen day=dofc(readTime)
		replace day=day+1 if totsecs<0
			replace totsecs=totsecs+86400 if totsecs<0
		egen daily_draws=sum(Draws), by(day)
		replace totsecs=1 if totsecs==0
		drop if daily_draws==0
		keep Flow Draws totsecs daily_draws day

		save `derp', replace

	
*Using recomended window number determine window size
	tempfile banana
	use `derp', clear
	sort day totsecs
	gen tmp=totsecs-totsecs[_n-1]
	sum tmp if tmp>00, d
	local min_span=`r(p50)'
	quietly sum totsecs
	local window_length=86400/`choice'
	gen window_num=ceil(totsecs/`window_length')
	
	egen sec_min=min(totsecs) if Draws, by(day window_num)
	egen sec_max=max(totsecs) if Draws, by(day window_num)
	egen sec_mean=median(totsecs) if Draws, by(day window_num)
	gen sec_span=sec_max-sec_min
	replace sec_span=sec_span+`min_span'
	*replace sec_span=0 if missing(sec_span)
	
	collapse  daily_draws sec_span sec_mean (sum) Draws Flow, by(window_num day)
	local cmd twoway (hist totsecs if Draws, yaxis(1) width(900)) 
		forv i = 1/`choice'{
			sum sec_mean if window_num==`i'
				local win_`i'_loc_mean =`r(mean)'
				local win_`i'_loc_sd =`r(sd)'
			*sum sec_span if window_num==`i'
			*	local win_`i'_span_mean =`r(mean)'
			*	local win_`i'_span_sd =`r(sd)'
			local cmd `cmd' (function normalden(x,`win_`i'_loc_mean',`win_`i'_loc_sd'), ra(1 86400) yaxis(2))
		}
	use `derp', clear
	if !strlen("`graph_off'") {
		`cmd' , name(hist_it_0, replace)
	}
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
					egen sec_min_`i'=min(totsecs) if Draws & window_num_`i'==`i', by(day)
					egen sec_max_`i'=max(totsecs) if Draws &  window_num_`i'==`i', by(day)
					egen sec_mean_`i'=median(totsecs) if Draws, by(bla2)
					replace sec_mean_`i'=. if missing(bla2)
					gen sec_span_`i'=sec_max_`i'-sec_min_`i'
					
				
				drop bla1 bla2
				*adjust to time gran
				replace sec_span_`i'=sec_span_`i'+`min_span'
		}
		collapse  daily_draws sec_span* sec_mean* (sum) Draws Flow, by(window_num* day)
		local cmd twoway (hist totsecs if Draws, yaxis(1) width(900)) 
			forv i = 1/`choice'{
					local skipper: list skip & i
					if wordcount("`skipper'")>0{
						if `skipper'==`i'{
							continue
						 }
					}
					*drop a window if the Flow is insignificant
						  sum Flow if window_num_`i'==`i'
								 if `r(mean)'<1.5{
									local skip `skip' `i'
								 }
					 
						sum sec_mean_`i' if window_num_`i'==`i'
						local win_`i'_loc_mean =`r(mean)'
						if `win_`i'_loc_mean'>86400{
							local win_`i'_loc_mean =`win_`i'_loc_mean'-86400
						}
						local win_`i'_loc_sd =`r(sd)'
				*sum sec_span_`i' if window_num_`i'==`i'
				*	local win_`i'_span_mean =`r(mean)'
				*	local win_`i'_span_sd =`r(sd)'
				local cmd `cmd' (function normalden(x,`win_`i'_loc_mean',`win_`i'_loc_sd'), ra(1 86400) yaxis(2))
			}
			
			save `banana', replace
				use `derp', clear
			if !strlen("`graph_off'") {
				`cmd' , name(hist_it_`j', replace) title("Site `site'") subtitle("shifted `low_hour' hours")
			}
			
	}
	if !strlen("`graph_off'") {
		graph export ../graphs/Draw_windows/Site_`site'_draw_window.png, replace
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
					replace sec_mean_`goku'=sec_mean_`goku'-86400 if sec_mean_`goku'>86400
		quietly ci sec_mean_`goku', level(90)
			return scalar int_`i'_mean=`r(mean)'
			return scalar int_`i'_mean_lb=`r(lb)'
			return scalar int_`i'_mean_ub=`r(ub)'
		
		quietly ci sec_span_`goku', level(90)
			return scalar int_`i'_span=`r(mean)'
			return scalar int_`i'_span_lb=`r(lb)'
			return scalar int_`i'_span_ub=`r(ub)'
		quietly ci Flow if Interval==`goku', level(90)
			return scalar int_`i'_Flow=`r(mean)'
			return scalar int_`i'_Flow_lb=`r(lb)'
			return scalar int_`i'_Flow_ub=`r(ub)'
	}
	

	
	if !strlen("`graph_off'") {
		graph box Span, over(Interval) name(window_widths, replace) nooutside
		
		egen Win_Flow=sum(Flow) , by(day Interval)
		graph box Win_Flow, over(Interval) name(window_flow, replace) nooutside
	}

	return scalar choice =`choice'
}

end


