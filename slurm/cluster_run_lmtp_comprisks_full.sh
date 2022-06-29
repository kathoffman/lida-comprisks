#!/bin/bash
#SBATCH --job-name=comprisk_full
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=40G
#SBATCH --partition=panda
#SBATCH --array=1-8
echo "$SLURM_ARRAY_TASK_ID"

source ~/.bashrc
spack load -r /bxc56dm
R CMD BATCH 'run_lmtp_comprisks_full.R'
