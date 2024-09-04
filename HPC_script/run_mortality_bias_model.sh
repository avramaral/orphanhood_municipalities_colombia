#!/bin/sh
#PBS -l walltime=48:00:00
#PBS -l select=1:ncpus=4:ompthreads=4:mem=4gb

REPO_PATH="/rds/general/user/aribeir2/home/orphanhood"

module load anaconda3/personal
source activate STANEnv

cd $REPO_PATH
Rscript mortality_bias_stan.R
