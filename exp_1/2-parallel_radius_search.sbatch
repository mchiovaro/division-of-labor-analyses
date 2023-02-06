#!/bin/bash

#SBATCH --job-name=slurm_exp_1                # Name of job
#SBATCH --exclusive                           # Request exclusive use of the node so that we can use more memory
#SBATCH --output=slurm_output/out_exp_1.out   # Output file
#SBATCH --error=slurm_output/err_exp_1.err    # Error file
#SBATCH --array=1-111300159%1000             # Number of times we want to run the file (dyad-round-radius combos; observations in radius_grid_search in R script)
#SBATCH --ntasks=1                            # Scripts per job
#SBATCH --time=01:00:00                       # Set run time

# save our array ID as an environmental variable
export SLURM_ARRAY_TASK_ID

# load the modules we need
module load gcc/11.2.0
module load r/4.2.0

# print to output for confirmation that it's started
echo $SLURM_ARRAY_TASK_ID": Running SLURM task"

# run the program
Rscript $HOME/dissertation/exp_1/2-parallel_radius_search.R

# print to output for confirmation that it's ended
echo $SLURM_ARRAY_TASK_ID ": Job done"
