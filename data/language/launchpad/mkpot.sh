# Update the template on launchpad

mkdir translation
mkdir translation/ccdciel
rm translation/ccdciel/*

echo cp ../ccdciel.po translation/ccdciel/ccdciel.pot

echo 'msgid ""'> translation/ccdciel/ccdciel.pot
echo 'msgstr ""'>> translation/ccdciel/ccdciel.pot
echo '"PO-Revision-Date: 2018-02-22 16:05+0100\n"'>> translation/ccdciel/ccdciel.pot
echo '"Last-Translator: Patrick Chevalley <pch@ap-i.net>\n"'>> translation/ccdciel/ccdciel.pot
echo '"Project-Id-Version: ccdciel.en\n"'>> translation/ccdciel/ccdciel.pot
echo '"POT-Creation-Date: \n"'>> translation/ccdciel/ccdciel.pot
echo '"Language-Team: French <kde-i18n-doc@kde.org>\n"'>> translation/ccdciel/ccdciel.pot
echo '"MIME-Version: 1.0\n"'>> translation/ccdciel/ccdciel.pot
echo '"Content-Type: text/plain; charset=UTF-8\n"'>> translation/ccdciel/ccdciel.pot
echo '"Content-Transfer-Encoding: 8bit\n"'>> translation/ccdciel/ccdciel.pot
echo '"X-Generator: Lokalize 1.2\n"'>> translation/ccdciel/ccdciel.pot
echo '"Language: fr\n"'>> translation/ccdciel/ccdciel.pot
echo '"Plural-Forms: nplurals=2; plural=(n > 1);\n"'>> translation/ccdciel/ccdciel.pot

cat ../ccdciel.po >> translation/ccdciel/ccdciel.pot

cd translation
tar cvf ../ccdcielpot.tar ccdciel/ccdciel.pot
cd -
