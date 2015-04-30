****************************************************************************************************
* Name: WindowFind
* Description: finds number of windows of use
* Author(s): JD
* Date: ?
* Modified: ?
* Bonus Info: ?
****************************************************************************************************

program define WindowFind, rclass
syntax [anything(name=var)], [id(string) graph_off time_var(string) interval(string)]


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
	
	tempfile herp dee derp dset
	 save `dset', replace
	*Set up graph dataset
	clear
	gen windows=.
	gen mean_Draw=.
	gen window_retention=.
	save `herp', replace


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

	* effectivly a loadshape	
	if !strlen("`graph_off'") {
		hist totsecs if Draws, name(Draw_hist, replace) width(900)
	}
	

		collapse (sum) `var' Draws, by(`interval')
		sum Draws, d
		local Draw_iter =`r(p75)'
		local Draw_max =`r(p99)'
		local Draw_mean =`r(p50)'
		
	*loop through number of windows

	forv windows=1/`Draw_iter'{
		local window_length=`sec_of'/`windows'
		use `derp', clear
		gen window_num=ceil(totsecs/`window_length')
		collapse  `interval'_draws (sum) Draws `var', by(window_num `interval')
		gen draw_proportion=Draws/`interval'_draws
		
			
		preserve
			gen window_count=window_num if Draws
			egen intervals=count(window_count), by(`interval')
			collapse intervals, by(`interval')
			ci intervals, level(90)
			local window_retention =`r(mean)'
		restore
		
		quietly drop if Draws==0
		collapse draw_proportion, by(window_num)
		
		quietly ci draw_proportion, level(90)
		local mean_Draw =`r(mean)'
		
		use `herp', clear
		quietly moreobs 1
		quietly replace windows =`windows' in l
		quietly replace mean_Draw=`mean_Draw' in l
		quietly replace window_retention=`window_retention' in l
		quietly save `herp', replace
		
	}
		
		gen this=windows/`Draw_max'
		quietly sum window_retention
		local res=ceil(`r(max)')
		gen choice=abs(1-mean_Draw/this)
		sort choice
		local choice=round(window_retention[1])
		
		sort windows
	if !strlen("`graph_off'") {
		local cmd twoway 
		local cmd `cmd' (line mean_Draw windows if windows<=`res') (line this window_retention if this<1),
		local cmd `cmd' legend( label(1 "Draw") label(2 "Window"))
		local cmd `cmd' xtitle("Windows") ytitle("Proportion") title("Site: `site'")  name(Blarg, replace)
		local cmd `cmd' subtitle("`choice' windows are ideal, shifted `low_hour' hours")
		`cmd'
	}	
		return scalar choice =`choice'
	


end
