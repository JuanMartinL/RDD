* Assignment 4: RD
* Replication of Hansen article (2015): "Punishment and Deterrence: Evidence from Drunk Driving"
* Juan Martín Londoño
* juanma.londono@gmail.com
* Caual Inference and Research Design
* 14/06/20
********************************************************************************

clear all				// Cleanning memory
set more off			// Continuous results

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


* 5. Recreate Table 2 but only white male, age and accident (acc) as dependent variables.
**********************************
regress white bac1 dbac dbac#bac1 if bac1 >= 0.03 & bac1 <= 0.13, robust

