#!/bin/bash
#!/usr/bin/env sh

#Remember to set this file to executable with   chmod +x filename

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while [pgrep -xc polybar -le 0]; do sleep 1; done

# Launch bar1 and bar2
#polybar example &

polybar zyphbar -c /home/zyphicx/.config/polybar/config

echo "Bars launched..."

