PROC IMPORT OUT=Kings 
            DATAFILE= "H:\My Documents\Deshka R, Chinook and Coho\2016\Deshka16, KS_data_for_SAS.xls" 
            DBMS=EXCEL REPLACE;
     SHEET="Age-Sex data$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;
PROC IMPORT OUT=KingsSex 
            DATAFILE= "H:\My Documents\Deshka R, Chinook and Coho\2016\Deshka16, KS_data_for_SAS.xls" 
            DBMS=EXCEL REPLACE;
     SHEET="Sex only data$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;

proc print data=Kings;
run;
/* Do Chi-Sq and Likelihood Ratio tests on full data set (AgeSex*Time) */
ods rtf file='H:\My Documents\Deshka R, Chinook and Coho\2016\Deshka16 KS_results.rtf';
proc freq data=Kings;
weight Count;
tables Time*AgeSex Time*Age/chisq nocol norow nopercent;
run;
/* Do Chi-Sq and Likelihood Ratio tests on reduced data set*/
proc freq data=Kings;
where AgeSex not in ("F1_1" "F1_2" "M1_4");
weight Count;
tables Time*AgeSex Time*Age/chisq nocol norow;
run;
/* Do Chi-Sq and Likelihood Ratio test on Sex vs. Time data set (N=386) */
proc freq data=KingsSex;
weight Count;
tables Time*Sex/chisq nocol norow nopercent;
run;
ods rtf close; run;
