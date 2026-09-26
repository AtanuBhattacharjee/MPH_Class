#!/usr/bin/env bash
# Push this folder to a new GitHub repository.
# 1. Create an EMPTY public repository on github.com named session02-frequency-app
#    (no README, no .gitignore, no licence).
# 2. Replace YOUR-GITHUB-USERNAME below, then run from inside this folder:
#       bash push_to_github.sh

GH_USER="YOUR-GITHUB-USERNAME"
REPO="session02-frequency-app"

git init
git add app.R manifest.json hypertension-dataset.xlsx README.md .gitignore
git commit -m "Session 2 frequency distributions Shiny app"
git branch -M main
git remote add origin "https://github.com/${GH_USER}/${REPO}.git"
git push -u origin main
