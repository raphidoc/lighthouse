#!/bin/bash
# Script to install lighthouse, Cops, Riop, RspectroAbs, ASD, HyperocR
# and their dependencies.

# Set CRAN mirror
mirror="https://muug.ca/mirror/cran/"

# uncomment the above line to install devtools
R -e "if (!require(devtools)) install.packages(\"devtools\", repos = \"$mirror\")"

echo -e  "\nCops"
GitRemote="git@srscm03.uqar.ca:mabr0002/Cops.git"
DIR=~/R/Cops
if [ -d "$DIR" ]; then
  ### Take action if $DIR exists ###
  echo "Directory ${DIR} exist, checking for update..."
  cd $DIR
  git fetch
  LOCAL=$(git rev-parse @)
  REMOTE=$(git rev-parse @{u})
  if [ $LOCAL != $REMOTE ]; then
    echo "Pulling from ${GitRemote}"
    /usr/bin/git pull
    cd ~
    echo
    R -e "devtools::install(\"$DIR\", repos = \"$mirror\")"
    echo
  else
    echo "Alredy up to date"
  fi

else
  ###  Control will jump here if $DIR does NOT exists ###
  echo "Cloning ${GitRemote} in ${DIR}"
  /usr/bin/git clone $GitRemote $DIR
  echo
  R -e "devtools::install(\"$DIR\", repos = \"$mirror\")"
  echo
fi


echo -e  "\nRiops"
GitRemote="git@srscm03.uqar.ca:mabr0002/Riops.git"
DIR=~/R/Riops
if [ -d "$DIR" ]; then
  ### Take action if $DIR exists ###
  echo "Directory ${DIR} exist, checking for update..."
  cd $DIR
  git fetch
  LOCAL=$(git rev-parse @)
  REMOTE=$(git rev-parse @{u})
  if [ $LOCAL != $REMOTE ]; then
    echo "Pulling from ${GitRemote}"
    /usr/bin/git pull
    cd ~
    echo
    R -e "devtools::install(\"$DIR\", repos = \"$mirror\")"
    echo
  else
    echo "Alredy up to date"
  fi

else
  ###  Control will jump here if $DIR does NOT exists ###
  echo "Cloning ${GitRemote} in ${DIR}"
  /usr/bin/git clone $GitRemote $DIR
  echo
  R -e "devtools::install(\"$DIR\", repos = \"$mirror\")"
  echo
fi

echo -e  "\nRspectroAbs"
GitRemote="git@srscm03.uqar.ca:mabr0002/RspectroAbs.git"
DIR=~/R/RspectroAbs
if [ -d "$DIR" ]; then
  ### Take action if $DIR exists ###
  echo "Directory ${DIR} exist, checking for update..."
  cd $DIR
  git fetch
  LOCAL=$(git rev-parse @)
  REMOTE=$(git rev-parse @{u})
  if [ $LOCAL != $REMOTE ]; then
    echo "Pulling from ${GitRemote}"
    /usr/bin/git pull
    cd ~
    echo
    R -e "devtools::install(\"$DIR\", repos = \"$mirror\")"
    echo
  else
    echo "Alredy up to date"
  fi

else
  ###  Control will jump here if $DIR does NOT exists ###
  echo "Cloning ${GitRemote} in ${DIR}"
  /usr/bin/git clone $GitRemote $DIR
  echo
  R -e "devtools::install(\"$DIR\", repos = \"$mirror\")"
  echo
fi

echo -e "\nHyperocR"
GitRemote="git@srscm03.uqar.ca:mabr0002/HyperocR.git"
DIR=~/R/HyperocR
if [ -d "$DIR" ]; then
  ### Take action if $DIR exists ###
  echo "Directory ${DIR} exist, checking for update..."
  cd $DIR
  git fetch
  LOCAL=$(git rev-parse @)
  REMOTE=$(git rev-parse @{u})
  if [ $LOCAL != $REMOTE ]; then
    echo "Pulling from ${GitRemote}"
    /usr/bin/git pull
    cd ~
    echo
    R -e "devtools::install(\"$DIR\", repos = \"$mirror\")"
    echo
  else
    echo "Alredy up to date"
  fi

else
  ###  Control will jump here if $DIR does NOT exists ###
  echo "Cloning ${GitRemote} in ${DIR}"
  /usr/bin/git clone $GitRemote $DIR
  echo
  R -e "devtools::install(\"$DIR\", repos = \"$mirror\")"
  echo
fi

echo -e "\nASD"
GitRemote="git@srscm03.uqar.ca:mabr0002/asd.git"
DIR=~/R/ASD
if [ -d "$DIR" ]; then
  ### Take action if $DIR exists ###
  echo "Directory ${DIR} exist, checking for update..."
  cd $DIR
  git fetch
  LOCAL=$(git rev-parse @)
  REMOTE=$(git rev-parse @{u})
  if [ $LOCAL != $REMOTE ]; then
    echo "Pulling from ${GitRemote}"
    /usr/bin/git pull
    cd ~
    echo
    R -e "devtools::install(\"$DIR\", repos = \"$mirror\")"
    echo
  else
    echo "Alredy up to date"
  fi

else
  ###  Control will jump here if $DIR does NOT exists ###
  echo "Cloning ${GitRemote} in ${DIR}"
  /usr/bin/git clone $GitRemote $DIR
  echo
  R -e "devtools::install(\"$DIR\", repos = \"$mirror\")"
  echo
fi

echo -e "\nlighthouse"
GitRemote="git@srscm03.uqar.ca:mabr0002/lighthouse.git"
DIR=~/R/lighthouse
if [ -d "$DIR" ]; then
  ### Take action if $DIR exists ###
  echo "Directory ${DIR} exist, checking for update..."
  cd $DIR
  git fetch
  LOCAL=$(git rev-parse @)
  REMOTE=$(git rev-parse @{u})
  if [ $LOCAL != $REMOTE ]; then
    echo "Pulling from ${GitRemote}"
    /usr/bin/git pull
    cd ~
    echo
    R -e "devtools::install(\"$DIR\", repos = \"$mirror\")"
    echo
  else
    echo "Alredy up to date"
  fi

else
  ###  Control will jump here if $DIR does NOT exists ###
  echo "Cloning ${GitRemote} in ${DIR}"
  /usr/bin/git clone $GitRemote $DIR
  echo
  R -e "devtools::install(\"$DIR\", repos = \"$mirror\")"
  echo
fi
