#!/bin/bash
set -e
stack build
stack exec -- hackmanager readme
