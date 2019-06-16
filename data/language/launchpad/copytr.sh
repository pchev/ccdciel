# Copy the translation from download to source tree

cd download

for f in $(ls -1 ccdciel/ccdciel-*.po)
 do 
 fg="${f#*-}"
 echo cp $f ../../ccdciel.$fg 
 cp $f ../../ccdciel.$fg
done

for f in $(ls -1 ccdciel/ccdciel_hints-*.po)
 do 
 fg="${f#*-}"
 echo cp $f ../../ccdciel_hints.$fg 
 cp $f ../../ccdciel_hints.$fg
done
