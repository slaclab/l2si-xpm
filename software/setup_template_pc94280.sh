# Setup environment
#source /afs/slac/g/reseng/rogue/v3.7.0/setup_rogue.sh
source /u1/anaconda3/etc/profile.d/conda.sh
conda activate rogue_5.9.3

# Python Package directories
export EPIXROGUE_DIR=${PWD}/python
export SURF_DIR=${PWD}/../../firmware/submodules/surf/python

# Setup python path
export PYTHONPATH=${PWD}/python:${EPIXROGUE_DIR}:${SURF_DIR}:${PYTHONPATH}
