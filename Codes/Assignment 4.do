* Assignment 4: RD
* Replication of Hansen article (2015): "Punishment and Deterrence: Evidence from Drunk Driving"
* Juan Martín Londoño
* juanma.londono@gmail.com
* Caual Inference and Research Design
* 14/06/20
********************************************************************************

clear all				// Cleanning memory
set more off			// Continuous results
cd "D:\Desktop\Universidad\15° Semestre\Causal Inference and Research Design\Assigments\Assignment 4\RDD"

********************************************************************************

* Program to replicate of Hansen article (2015): "Punishment and Deterrence: Evidence from Drunk Driving"

*********************************
* Input
*********************************
use "D:\Desktop\Universidad\15° Semestre\Causal Inference and Research Design\Assigments\Assignment 4\RDD\Data\Input\hansen_dwi.dta", clear


*********************************
* Procedures
*********************************


* 3. Create a dummy equaling 1 if bac1>= 0.08 and 0 otherwise (BAC: blood alcohol content)
*********************************
gen dbac = 1 if bac1 >= 0.08
replace dbac = 0 if bac1 < 0.08


* 4. Checking in Raw Data if manpulation takes place: whether pple can manipulate their BAC.
*********************************
rddensity bac1, all c(0.08) plot graph_options(graphregion(color(white)) title("McCray Test") xtitle("BAC") ytitle("Density"))
graph export Figures\McCrayTest.png, replace


* 5. Recreate Table 2 but only white male, age and accident (acc) as dependent variables.
**********************************
eststo: reg white bac1 dbac dbac#c.bac1 if bac1 >= 0.03 & bac1 <= 0.13, robust
eststo: reg male bac1 dbac dbac#c.bac1 if bac1 >= 0.03 & bac1 <= 0.13, robust  
eststo: reg aged bac1 dbac dbac#c.bac1 if bac1 >= 0.03 & bac1 <= 0.13, robust 
eststo: reg acc bac1 dbac dbac#c.bac1 if bac1 >= 0.03 & bac1 <= 0.13, robust 
	// Don't forget c. for continuous variables


esttab using "D:\Desktop\Universidad\15° Semestre\Causal Inference and Research Design\Assigments\Assignment 4\RDD\Tables\Table2.tex", se label title(Regression Discontinuity Estimates for the Effect of Exceeding BAC Thresholds on Predetermined Characteristics table\label{tab1}) replace

eststo clear


* 6. Recreate Table 2 but only white male, age and accident (acc) as dependent variables.
**********************************

* Accident
cmogram acc bac1 if bac1 <= 0.4, cut(0.08) scatter title("Panel A. Accident.") line(0.08) qfitci graphopts(xtitle("BAC") ytitle("Mean of Accident") graphregion(color(white)))
graph export Figures\panelAsq.png, replace
graph save Figures\panelAsq, replace
cmogram acc bac1 if bac1 <= 0.4, cut(0.08) scatter title("Panel A. Accident.") line(0.08) lfitci graphopts(xtitle("BAC") ytitle("Mean of Accident") graphregion(color(white)))
graph export Figures\panelAlm.png, replace
graph save Figures\panelAlm, replace

* Male
cmogram male bac1 if bac1 <= 0.4, cut(0.08) scatter title("Panel B. Male.") line(0.08) qfitci graphopts(xtitle("BAC") ytitle("Mean of Male") graphregion(color(white)))
graph export Figures\panelBsq.png, replace
graph save Figures\panelBsq, replace
cmogram male bac1 if bac1 <= 0.4, cut(0.08) scatter title("Panel B. Male.") line(0.08) lfitci graphopts(xtitle("BAC") ytitle("Mean of Male") graphregion(color(white)))
graph export Figures\panelBlm.png, replace
graph save Figures\panelBlm, replace

* Age
cmogram aged bac1 if bac1 <= 0.4, cut(0.08) scatter title("Panel C. Age.") line(0.08)   qfitci graphopts(xtitle("BAC") ytitle("Mean of Age") graphregion(color(white)))
graph export Figures\panelCsq.png, replace
graph save Figures\panelCsq, replace
cmogram aged bac1, cut(0.08) scatter title("Panel C. Age.") line(0.08) lfitci graphopts(xtitle("BAC") ytitle("Mean of Age") graphregion(color(white)))
graph export Figures\panelClm.png, replace
graph save Figures\panelClm, replace

* White
cmogram white bac1 if bac1 <= 0.4, cut(0.08) scatter title("Panel D. White.") line(0.08) qfitci graphopts(xtitle("BAC") ytitle("Mean of White") graphregion(color(white)))
graph export Figures\panelDsq.png, replace
graph save Figures\panelDsq, replace
cmogram white bac1 if bac1 <= 0.4, cut(0.08) scatter title("Panel D. White.") line(0.08) lfitci graphopts(xtitle("BAC") ytitle("Mean of White") graphregion(color(white))) 
graph export Figures\panelDlm.png, replace
graph save Figures\panelDlm, replace

* Combining
gr combine Figures\panelAlm.gph Figures\panelBlm.gph Figures\panelClm.gph Figures\panelDlm.gph, title("Linear Fit with Confidence Intervals") graphregion(color(white))
graph export Figures\panel_lm.png, replace
graph save Figures\panel_lm, replace
gr combine Figures\panelAsq.gph Figures\panelBsq.gph Figures\panelCsq.gph Figures\panelDsq.gph, title("Quadratic Fit with Confidence Intervals") graphregion(color(white))
graph export Figures\panel_sq.png, replace
graph save Figures\panel_sq, replace

gr combine Figures\panel_lm.gph Figures\panel_sq.gph, graphregion(color(white))
graph save Figures\panel_controls, replace
graph export Figures\panel_control.png, replace


* 7. Estimate equation (1) in Hansen's paper with recidivism (recid) as the outcome. Your table should have three columns and two A and B panels associated with the different bandwidths: a. Column 1: control for the bac1 linearly, b. Column 2: interact bac1 with cutoff linearly and c. Column 3: interact bac1 with cutoff linearly and as a quadratic.
**********************************

* a.
eststo: reg recidivism dbac bac1 aged white male acc if bac1 >= 0.03 & bac1 <= 0.13, robust

* b.
eststo: reg recidivism dbac bac1 c.bac1#dbac aged white male acc if bac1 >= 0.03 & bac1 <= 0.13, robust

*c. 
*gen bac1_sq = bac1^2
eststo: reg recidivism dbac bac1 dbac#c.bac1 dbac#c.bac1_sq aged white male acc if bac1 >= 0.03 & bac1 <= 0.13, robust


esttab using "D:\Desktop\Universidad\15° Semestre\Causal Inference and Research Design\Assigments\Assignment 4\RDD\Tables\Table3.tex", se label title(Regression Discontinuity Estimates for the Effect of Exceeding the 0.08 BAC Thresholds on Recidivism table\label{tab1}) replace

eststo clear
