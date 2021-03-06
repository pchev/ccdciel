#!/bin/bash

function dopng {
  # $1 input svg dir
  # $2 output png dir
  # $3 icon size

  mkdir -p $2
  end=${#iconlist[@]}
  ((end--))
  for i  in $(seq 0 $end)
    do
         fsvg=$1/${iconlist[i]}.svg
         fpng=$2/${iconlist[i]}.png
         inkscape -w $3 -h $3 -o $fpng $fsvg > /dev/null 2>&1
         if [[ $? != 0 ]] 
            then  echo Error:  $i ${iconlist[i]}
         fi
    done 
}


iconlist=(adjust bullseye capture center clipping-info focus hist histdec pin plan video inf invert flip-horizontal flip-vertical show-images)

dopng svg/daylight icons/daylight 22
dopng svg/night-vision icons/night-vision 22

iconlist=(add remove arrow-up arrow-down)

dopng svg/daylight icons/daylight 12
dopng svg/night-vision icons/night-vision 12

