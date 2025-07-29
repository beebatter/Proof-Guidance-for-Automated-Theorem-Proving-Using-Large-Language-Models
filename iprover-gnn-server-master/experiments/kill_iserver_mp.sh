#!/bin/bash
for x in `ps xa | grep "iserver_mp.py" | cut -d'p' -f1`; do kill -9 $x; done
