Angrist and Lavy (1999)

Using Maimonides' Rule to Estimate the Effect of Class Size on Student Achievement

Notes: These programs produce Tables II-V in the published paper. The program mmoulton_post.do implements a Moulton (1986) clustering adjustment for OLS and 2SLS and is used by the other .do files. 

These are STATA translations of the original SAS programs. The switch in software generates slightly different RMSEs.

Programs: 

    * AngristLavy_Table2.do creates Table II in the published paper
    * AngristLavy_Table3.do creates Table III in the published paper
    * AngristLavy_Table4.do creates Table IV in the published paper
    * AngristLavy_Table5.do creates Table V in the publilshed paper
    * mmoulton_post.do implements OLS Moulton corrections - this file should be run in conjunction with the files that create the tables

Data:

    * final4.dta contains data for 4th graders
    * final5.dta contains data for 5th graders 