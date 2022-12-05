#!/bin/bash

#SBATCH --job-name=ABM9a__group_
#SBATCH --partition=jrw0107_std  #general   #jrw0107_std #(up to 192 but spread over all 20) so 192/20
#SBATCH -N 1
#SBATCH -n 1
#SBATCH -t 300:00:00
#SBATCH --mail-type=END
#SBATCH --mail-user=gfl0003@auburn.edu
#SBATCH --mem=16000M

module load R/4.2.1

cd /scratch/glamka/ABM_run.11.14.22_9a/_group_  #alter this to either be in scratch or home dir

Rscript Cover.R  #note, Rscript might not be the call command

### #SBATCH -mem=[0,35]
