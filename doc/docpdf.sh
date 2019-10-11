#!/bin/bash
#
#  script to make the pdf documentation 
#  
#  Requirement:
#    - run getdoc.sh and copydoc.sh in the wikidoc directory 
#    - install wkhtmltopdf from http://code.google.com/p/wkhtmltopdf/
#

langs='en'
suffix='ccdciel'

rm doc_*.pdf

cd wiki_doc

# main loop
for lang in $langs; do

echo $lang

# main index order
grep '<li class="level."><div class="li"> <a href="' $lang/documentation/start.html \
     | sed 's/<li class="level."><div class="li"> <a href="//'| cut -d\" -f1 \
     | awk '{printf ("'$lang'/documentation/%s \n", $1)}' > fl.txt

# remove external links
sed -i '/http:/ d' fl.txt
sed -i '/https:/ d' fl.txt

# insert title, first page and doc index
sed -i '1 i '$lang'\/documentation\/00_title.html\n'$lang'\/start.html\n'$lang'\/features.html\n'$lang'\/documentation\/start.html' fl.txt

# insert pages in File menu
grep '<li class="level1"><div class="li"> <a href="' $lang/documentation/file.html      | sed 's/<li class="level1"><div class="li"> <a href="//'| cut -d\" -f1      | awk '{printf ("'$lang'/documentation/%s \n", $1)}' > fl1.txt
sed -i '/\/file\.html/r./fl1.txt' fl.txt
rm fl1.txt

# insert pages in Edit menu
grep '<li class="level1"><div class="li"><a href="' $lang/documentation/edit.html      | sed 's/<li class="level1"><div class="li"><a href="//'| cut -d\" -f1      | awk '{printf ("'$lang'/documentation/%s \n", $1)}' > fl1.txt
sed -i '/\/edit\.html/r./fl1.txt' fl.txt
rm fl1.txt

# insert pages in Tools menu
grep '<li class="level1"><div class="li"><a href="' $lang/documentation/tools.html      | sed 's/<li class="level1"><div class="li"><a href="//'| cut -d\" -f1      | awk '{printf ("'$lang'/documentation/%s \n", $1)}' > fl1.txt
sed -i '/\/tools\.html/r./fl1.txt' fl.txt
rm fl1.txt

# insert INDI link on same row as ASCOM 
if [ -f "$lang/documentation/indi.html" ]; then
  sed -i '/\/ascom.html/ a '$lang'\/documentation\/indi.html' fl.txt
fi

# insert pages not in index 
if [ -f "$lang/documentation/right_click_menu.html" ]; then
  sed -i '/\/right_click_menu.html/ a '$lang'\/documentation\/photometry.html' fl.txt
fi
if [ -f "$lang/documentation/ccdciel_status.html" ]; then
  sed -i '/\/ccdciel_status.html/ a '$lang'\/documentation\/server.html \n'$lang'\/documentation\/proxy.html' fl.txt
fi
if [ -f "$lang/documentation/ccdscript.html" ]; then
  sed -i '/\/ccdscript.html/ a '$lang'\/documentation\/script_example.html \n'$lang'\/documentation\/script_reference.html' fl.txt
fi

# remove duplicates
cp fl.txt fl1.txt
uniq fl1.txt fl.txt
rm fl1.txt

# all in one line
sed -i ':a;N;$!ba;s/\n/ /g' fl.txt 
# read the list of file
fl=$(<fl.txt)

# create title page

dt='Edited: '$(LC_ALL=C date '+%B %d %Y')
t='CCDciel'
lastv='Last version is available from the wiki at'
l='Users documentation'
tocl='Table of Content'
l='English documentation' 

cat > $lang/documentation/00_title.html << EOF
<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8" />
</head>
<body>
<center>
<br/><br/><br/><br/><br/><br/><br/><br/>
<H1> $t </H1>
<br/><br/>
<b>$l</b>
<br/><br/>
$dt
<br/><br/>
$lastv <br/>
<a href="http://www.ap-i.net/ccdciel/$lang/documentation/start">http://www.ap-i.net/ccdciel/$lang/documentation/start</a>
<br/><br/>
<br/><br/>
<br/><br/>
<br/><br/>
</center>
</body>
</html>
EOF

# toc
wkhtmltopdf --dump-default-toc-xsl > toc.xsl
sed -i "s/Table of Content/$tocl/g" toc.xsl

# create pdf
wkhtmltopdf --quiet --dpi 96 --enable-toc-back-links  --enable-external-links --enable-internal-links --footer-right '[page]' $fl toc --xsl-style-sheet toc.xsl  tmp.pdf

# fix for anchor bug : http://code.google.com/p/wkhtmltopdf/issues/detail?id=463
#gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.7 -dColorImageResolution=300 -dGrayImageResolution=300 -dNOPAUSE -dQUIET -dBATCH -sOutputFile=../doc_${suffix}_${lang}.pdf tmp.pdf
cp tmp.pdf ../doc_${suffix}_${lang}.pdf

# cleanup
rm tmp.pdf fl.txt toc.xsl $lang/documentation/00_title.html

# end main loop
done


