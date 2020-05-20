#!/bin/bash
MODEL_IN="$1"
MODEL_OUT="${MODEL_IN%-data.RData}-result.RData"
MODEL_LOCK="${MODEL_IN}.lock"

if [[ ! -e "$MODEL_IN" ]]; then
  echo "cannot find model file $MODEL_IN"
  exit 1
fi

if [[ -e "$MODEL_OUT" ]]; then
  echo "model $MODEL_IN already processed"
  exit 0
fi

if [[ -e "$MODEL_LOCK" ]]; then
  echo "model $MODEL_IN is locked"
  exit 0
else
  echo $(hostname) $(date) > "${MODEL_LOCK}"
fi

if [[ $(hostname) == "bpc" ]]; then
  export PATH="/home/vrichter/install/opt/R/bin/:$PATH"
  if [[ "$USE_GPU" == "" ]]; then
    export KERAS_PYTHON="/home/vrichter/sync/projects/publications/diss/scripts/.venv_keras/bin/python"
  else
    export KERAS_PYTHON="/home/vrichter/sync/projects/publications/diss/scripts/.venv_keras_bionic_gpu/bin/python"
    export LD_LIBRARY_PATH="/vol/csra/releases/xenial/lsp-csra-0.15/lib64/:$LD_LIBRARY_PATH"
  fi
elif [[ $(hostname) == "scu" ]]; then
  export USE_GPU=1
  export PATH="/homes/vrichter/install/opt/R/bin/:$PATH"
  export KERAS_PYTHON="/homes/vrichter/sync/projects/publications/diss/scripts/.venv_keras_bionic_gpu/bin/python"
  export LD_LIBRARY_PATH="/vol/csra/releases/xenial/lsp-csra-0.15/lib64/:$LD_LIBRARY_PATH"
else
  if lsb_release -c | grep -q bionic; then
    export PATH="/homes/vrichter/install/opt/R-bionic/bin/:$PATH"
    export KERAS_PYTHON="/homes/vrichter/sync/projects/publications/diss/scripts/.venv_keras_netboot/bin/python"
  else
    export PATH="/homes/vrichter/install/opt/R/bin/:$PATH"
    export KERAS_PYTHON="/homes/vrichter/sync/projects/publications/diss/scripts/.venv_keras_netboot_xenial/bin/python"
  fi
fi

export CALCULATE_NONDRAFT="ON"
export WRITE_OUT="ON"
export TRAIN_MODEL="$MODEL_IN"

Rscript ../scripts/study-role-eval.R

rm "$MODEL_LOCK"
