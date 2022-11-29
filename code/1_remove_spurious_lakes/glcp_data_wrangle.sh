#!/bin/bash

# Submit this script with: sbatch <this-filename>

#SBATCH --time=7-00:00:00   						# walltime - Max time of 7 days
#SBATCH --ntasks=1   								# number of processor cores (i.e. tasks)
#SBATCH --nodes=1   								# number of nodes
#SBATCH --mem-per-cpu=1G   							# memory per CPU core
#SBATCH --cpus-per-task=20                    		### Number of threads per task (OMP threads)
#SBATCH -J "REMOVE_SPURIOUS_LAKES"   				# job name
#SBATCH --mail-user=rmcclure@carnegiescience.edu   	# email address
#SBATCH -o clean_glcp.out            				### File in which to store job output
#SBATCH --cpus-per-task=32                   		### Number of threads per task (OMP threads)
#SBATCH --get-user-env                       		### Import your user environment setup
#SBATCH --verbose                            		### Increase informational messages
#SBATCH --mem=1024000          			     		### Amount of memory in MB

# Notify at the beginning, end of job and on failure.
#SBATCH --mail-type=BEGIN
#SBATCH --mail-type=END
#SBATCH --mail-type=FAIL

## /SBATCH -p general # partition (queue)
## /SBATCH -o slurm.%N.%j.out # STDOUT
## /SBATCH -e slurm.%N.%j.err # STDERR

# LOAD MODULES, INSERT CODE, AND RUN YOUR PROGRAMS HERE

cd "/central/groups/carnegie_poc/rmcclure/glcp_analysis/code/1_remove_spurious_lakes"
echo
echo "--- We are now in $PWD, running an R script ..."
echo

# Load R on compute node
module load gcc/9.2.0
module load R/4.2.1

Rscript --vanilla D1_glcp_select_summarize.R