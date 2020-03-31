#!/bin/bash

directory=$(pwd)

#ultimo mes para pronostico
declare -a gamma_sa=("30" "60" )

gamma_salength=${#gamma_sa[@]}
  
  #horizonte
  declare -a gamma_ss=("55" "75" "95")
  gamma_sslength=${#gamma_ss[@]}

    
    echo  `date`
    for (( j=1; j<${gamma_sslength}+1; j++ ));
    do
    fore=${gamma_ss[$j-1]}
    
    ###################################
    ### Parallel version
    for ID in ${gamma_sa[*]}; do echo $ID; done  | xargs -I{} --max-procs 20 bash -c "
      echo $fore {};
      Rscript /home/fou/Desktop/DSenCDMX/COVID-ODE/COVID19/Main.R $fore {}"
    echo "Exit code for xargs = $?"
    ###################################
    
    done    
    echo  `date`
