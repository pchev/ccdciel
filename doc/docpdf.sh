#!/bin/bash
#
#  script to make the pdf documentation 
#  
#  Requirement:
#    - install wkhtmltopdf from https://wkhtmltopdf.org/
#    - run getdoc.sh and docpdf.sh in this directory 
#

langs='en'
suffix='ccdciel'

rm doc_*.pdf

cd wiki_doc

# main loop
for lang in $langs; do

echo $lang

# main index order
cat <<EOF > fl.txt
$lang/documentation/00_title.html
$lang/start.html
$lang/features.html
$lang/documentation/start.html
$lang/documentation/the_program_screen.html
$lang/documentation/connecting_the_equipment.html
$lang/documentation/global_configuration.html
$lang/documentation/focusing.html
$lang/documentation/simple_capture.html
$lang/documentation/plate_solving.html
$lang/documentation/automated_sequence.html
$lang/documentation/video_tutorial.html
$lang/documentation/faq.html
$lang/documentation/edit.html
$lang/documentation/display.html
$lang/documentation/tools.html
$lang/documentation/right_click_menu.html
$lang/documentation/photometry.html
$lang/documentation/file.html
$lang/documentation/help.html
$lang/documentation/status_bar.html
$lang/documentation/ccdciel_status.html
$lang/documentation/server.html
$lang/documentation/ccdscript.html
$lang/documentation/script_example.html
$lang/documentation/jsonrpc_reference.html
$lang/documentation/script_reference.html
$lang/documentation/cmdline.html
$lang/documentation/image_preprocessing.html
$lang/documentation/installation_on_ubuntu.html
$lang/documentation/installation_on_linux.html
$lang/documentation/installation_on_windows.html
$lang/documentation/installation_on_mac_os_x.html
$lang/documentation/hyperleda_database.html
$lang/documentation/ascom.html
$lang/documentation/indi.html
$lang/documentation/install_python.html
$lang/documentation/astap.html
$lang/documentation/astrometry.net.html
$lang/documentation/windows_subsystem_for_linux.html
$lang/documentation/star_locator_elbrus.html
$lang/documentation/platesolve_2.html
$lang/documentation/proxy.html
EOF

# insert pages in Edit menu
grep '<li class="level1"><div class="li"><a href="' $lang/documentation/edit.html      | sed 's/<li class="level1"><div class="li"><a href="//'| cut -d\" -f1      | awk '{printf ("'$lang'/documentation/%s \n", $1)}' > fl1.txt
sed -i '/\/edit\.html/r./fl1.txt' fl.txt
rm fl1.txt

# insert pages in Display menu
grep '<li class="level1"><div class="li"><a href="' $lang/documentation/display.html      | sed 's/<li class="level1"><div class="li"><a href="//'| cut -d\" -f1      | awk '{printf ("'$lang'/documentation/%s \n", $1)}' > fl1.txt
sed -i '/\/display\.html/r./fl1.txt' fl.txt
rm fl1.txt

# insert pages in Tools menu
grep '<li class="level1"><div class="li"> <a href="' $lang/documentation/tools.html      | sed 's/<li class="level1"><div class="li"> <a href="//'| cut -d\" -f1      | awk '{printf ("'$lang'/documentation/%s \n", $1)}' > fl1.txt
sed -i '/\/tools\.html/r./fl1.txt' fl.txt
rm fl1.txt

# insert pages in File menu
grep '<li class="level1"><div class="li"> <a href="' $lang/documentation/file.html      | sed 's/<li class="level1"><div class="li"> <a href="//'| cut -d\" -f1      | awk '{printf ("'$lang'/documentation/%s \n", $1)}' > fl1.txt
sed -i '/\/file\.html/r./fl1.txt' fl.txt
rm fl1.txt

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
tocl='Table of Content'
l='Program documentation' 

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


