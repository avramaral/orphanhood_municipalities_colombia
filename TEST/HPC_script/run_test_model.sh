#!/bin/sh
#PBS -l walltime=23:59:00
#PBS -l select=1:ncpus=4:ompthreads=4:mem=40gb

module load anaconda3/personal
source activate STANEnv

Rscript TEST/test_model.R

