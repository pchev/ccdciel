#!/bin/bash

rm -rf doc wiki_doc

wget -E -p -np -nH --cut-dirs=1 -X */*detail,*/*export,*/playground -R *do=index* -P doc --restrict-file-names=windows -k https://www.ap-i.net/ccdciel/en/start https://www.ap-i.net/ccdciel/en/start --header="X-DokuWiki-Do: export_xhtml"

wget -E -r -p -np -nH --timeout=15 --cut-dirs=1 -X */*detail,*/*export,*/playground -R *do=index* -P doc --cache=off --restrict-file-names=windows -k  https://www.ap-i.net/ccdciel/en/documentation/start https://www.ap-i.net/ccdciel/en/documentation/start --header="X-DokuWiki-Do: export_xhtml"

rm doc/robots.txt

mkdir wiki_doc
cp -R -p doc/* wiki_doc/
cp wiki.css wiki_doc/lib/exe/
cd wiki_doc/lib/exe
css=$(ls -1 css.php*|head -1)
rm $css
mv wiki.css $css
cd -
