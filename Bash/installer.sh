#!/bin/bash
# Script to install lighthouse, Cops, Riop, RspectroAbs, ASD, HyperocR
# and their dependencies.

# lighthouse
/usr/bin/git clone https://github.com/raphidoc/lighthouse.git ~R/lighthouse
/usr/bin/git fetch
/usr/bin/git pull

# Cops
/usr/bin/git clone https://github.com/raphidoc/Cops.git ~R/Cops
/usr/bin/git fetch
/usr/bin/git pull

# Riops
/usr/bin/git clone https://github.com/raphidoc/Riops.git ~R/Riops
/usr/bin/git fetch
/usr/bin/git pull

# RspectroAbs
/usr/bin/git clone https://github.com/raphidoc/RspectroAbs.git ~R/RspectroAbs
/usr/bin/git fetch
/usr/bin/git pull

# HyperocR
/usr/bin/git clone https://github.com/raphidoc/HyperocR.git ~R/HyperocR
/usr/bin/git fetch
/usr/bin/git pull

# ASD
/usr/bin/git clone https://github.com/raphidoc/ASD.git ~R/ASD
/usr/bin/git fetch
/usr/bin/git pull
