#!/bin/bash

#SBATCH --time=6:00:00   # walltime
#SBATCH --ntasks=2   # number of processor cores (i.e. tasks)
#SBATCH --nodes=1   # number of nodes
#SBATCH --mem-per-cpu=2048M   # memory per CPU core
#SBATCH -J "Retry end of 2015"   # job name
#SBATCH --mail-user=jcurtis210@gmail.com   # email address
#SBATCH --mail-type=BEGIN
#SBATCH --mail-type=END

# Compatibility variables for PBS. Delete if not needed.


# Set the max number of threads to use for programs using OpenMP. Should be <= ppn. Does nothing if the program doesn't use OpenMP.
export OMP_NUM_THREADS=$SLURM_CPUS_ON_NODE

# LOAD MODULES, INSERT CODE, AND RUN YOUR PROGRAMS HERE
module load r/3/3
Rscript NCAAFAnalysis.R
