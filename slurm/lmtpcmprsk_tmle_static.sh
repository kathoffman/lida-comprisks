#!/bin/bash
# Job name:
#SBATCH --job-name=lmtpcmprsk_tmle_static
#
# Working directory:
#SBATCH --chdir=/global/home/users/nhejazi/
#
# Account:
#SBATCH --account=co_biostat
#
# Quality of Service:
#SBATCH --qos=biostat_savio3_normal
#
# Partition:
#SBATCH --partition=savio3
#
# Processors (1 node = 20 cores):
#SBATCH --nodes=1
#SBATCH --exclusive
#
# Wall clock limit ('0' for unlimited):
#SBATCH --time=72:00:00
#
# Mail type:
#SBATCH --mail-type=all
#
# Mail user:
#SBATCH --mail-user=nhejazi@berkeley.edu
#
# Job output:
#SBATCH --output=slurm.out
#SBATCH --error=slurm.out
#
## Command(s) to run:
module load openblas/0.2.20 gcc/6.3.0 r/4.0.3 r-packages/default
cd ~/lmtpcmprsk_aki/

R CMD BATCH --no-save --no-restore \
  '--args int_type=static est_type=tmle' \
  R/run_lmtp.R logs/run_lmtp_tmle_static.Rout
