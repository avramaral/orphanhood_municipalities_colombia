#!/bin/sh
#PBS -l walltime=07:59:00
#PBS -l select=1:ncpus=4:ompthreads=4:mem=20gb

REPO_PATH="/rds/general/user/aribeir2/home/orphanhood_municipalities_colombia"

module load anaconda3/personal
source activate STANEnv

cd $REPO_PATH
Rscript R/process_draws.R
