$PROBLEM Simple PREDPP model

$INPUT ID=SUBJ X DV

$DATA ../data/pred_data1.csv
IGN=@

$THETA 
.1 ; intercept
1  ; slope

$OMEGA 0.1

$SIGMA .2

$PRED
intercept=THETA(1)+ETA(1)
slope=THETA(2)
Y=intercept+slope*X+EPS(1)

$EST METHOD=1 INTER PRINT=20

$TABLE
slope FILE = pred030_res.txt
