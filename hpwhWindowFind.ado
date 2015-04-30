****************************************************************************************************
* Name: hpwhWindowFind
* Description: ?
* Author(s): JD
* Date: ?
* Modified: ?
* Bonus Info: ?
****************************************************************************************************

program define hpwhWindowFind, rclass
syntax [anything(name=site)], [graph_off]


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

	* effectivly a loadshape	
	if !strlen("`graph_off'") {
		hist totsecs if Draws, name(Draw_hist, replace) width(900)
	}
	

		collapse (sum) Flow Draws, by(day)
		sum Draws
		local Draw_max =`r(max)'
		local Draw_mean =`r(mean)'
		ci Draws, level(90)
		
	*loop through number of windows

	forv windows=1/`Draw_max'{
		local window_length=86400/`windows'
		use `derp', clear
		gen window_num=ceil(totsecs/`window_length')
		collapse  daily_draws (sum) Draws Flow, by(window_num day)
		gen draw_proportion=Draws/daily_draws
		
			
		preserve
			gen window_count=window_num if Draws
			egen intervals=count(window_count), by(day)
			collapse intervals, by(day)
			ci intervals, level(90)
			local window_retention =`r(mean)'
		restore
		
		quietly drop if Draws==0
		collapse draw_proportion, by(window_num)
		
		ci draw_proportion, level(90)
		local mean_Draw =`r(mean)'
		local se_Draw =`r(se)'
		local upper_Draw =`r(ub)'
		local lower_Draw =`r(lb)'
		
		use `herp', clear
		quietly moreobs 1
		quietly replace windows =`windows' in l
		quietly replace mean_Draw=`mean_Draw' in l
		quietly replace se_Draw=`se_Draw' in l
		quietly replace upper_Draw=`upper_Draw' in l
		quietly replace lower_Draw=`lower_Draw' in l
		quietly replace window_retention=`window_retention' in l
		quietly save `herp', replace
		
	}
		replace upper_Draw=. if upper_Draw>1
		replace lower_Draw=. if lower_Draw<0
	if !strlen("`graph_off'") {
		twoway (line upper_Draw windows) (line lower_Draw windows) (line mean_Draw windows) if windows<`Draw_mean', name(window_capture, replace)
		twoway (line window_retention windows), name(window_retentions, replace)
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
	
}

end
