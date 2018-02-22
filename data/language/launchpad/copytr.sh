# Copy the translation from download to source tree

cd download

for f in $(ls -1 ccdciel/ccdciel-*.po)
 do 
 fg="${f#*-}"
 echo cp $f ../../ccdciel.$fg 
 cp $f ../../ccdciel.$fg
done
