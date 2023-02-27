#!/usr/bin/env bash

branch=$(git rev-parse --abbrev-ref HEAD)
git checkout main
git pull
git checkout cs-unm-machine
git pull
git merge main -m 'Update cs-unm-machine branch'
git push
git checkout usb-stick
git pull
git merge main -m 'Update usb-stick branch'
git push
git checkout no-evil-layer
git pull
git merge main -m 'Update no-evil-layer branch'
git push
git checkout macos
git pull
git merge main -m 'Update macos branch'
git push
git checkout no-evil-layer-macos
git pull
git merge macos -m 'Update no-evil-layer-macos branch'
git push
git checkout $branch
git checkout god-layer
git pull
git merge main -m 'Update god-layer'
git push

