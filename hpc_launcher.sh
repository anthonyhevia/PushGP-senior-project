#PBS -N push-gp-semantics-lexicase-uniform
#PBS -l nodes=1:ppn=8
#PBS -l mem=20GB
#PBS -q una-new
#PBS -m n
#PBS -j oe
#PBS -l walltime=72:00:00
#PBS -r y
#PBS -o /home/ahevia/HPC-logs/
#PBS -t 0-49
# This last line is the range of the number of jobs to start, inclusive

# NOTE: Directions for use are at the bottom of this file.

NP=`cat $PBS_NODEFILE | wc -l`

# Standard output of runs will be printed to files in whatever directory you put here:
DIRECTORY="/home/ahevia/push-gp/results/semantics-lexicase-uniform/"

# This is the clojure command that is run. You can supply extra command line
# arguments after push411.core/main
CLOJURECMD="/usr/local/bin/clojure -X push411.core/main '"\""{:selection :semantics-lexicase}"\""'"

echo "Running on node(s):"
cat $PBS_NODEFILE
echo ""
echo "Changing to directory $PBS_O_WORKDIR"
cd $PBS_O_WORKDIR

echo "Making directory and copying this folder into it: $DIRECTORY"
mkdir -p "$DIRECTORY/"

if [ $PBS_ARRAYID = "0" ]
then
   cp -r $PBS_O_WORKDIR "$DIRECTORY/push411"
fi


echo -n "Executing program at: "
date
echo ""

echo "clojure command: $CLOJURECMD"
echo ""

time $CLOJURECMD 2>&1 > "$DIRECTORY/run${PBS_ARRAYID}.txt"

echo ""
echo -n "Finished program at: "
date
echo ""

## PBS Options
# -M - this should be your email address
# -m - this says you will receive an email when a job begins (b), ends (e), or aborts (a). If you don't want emails, remove this line
# -o - this is the directory where the HPC log files (not the Clojush log files) will end up. Can be the same place as the Clojush log files if you want.
# -t - allows you to start multiple jobs with one qsub. This is a range of values that will tell how many jobs to start, in an "array". You likely want to leave this as-is.

## Submitting jobs
# To submit a job, at the command line use:
# qsub hpc_launcher.run
# which submits this file.

## MONITORING
# You can monitor your jobs with the qstat command. There are many useful options; here are a few:
# -t - lists each job in an "array"/range of jobss, instead of the job as a whole
# -n - lists which node each job is running on
# -u USERNAME - lists only jobs submitted by USERNAME
#
# You can delete one of your jobs by using:
# qdel JOBID
# If your job is an array, you will need:
# qdel JOBID[]
#
