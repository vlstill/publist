
all : build test


build : site
	./site build

site : site.hs publist.cabal .cabal-sandbox
	cabal install --force-reinstalls
	touch site

rebuild : site
	./site rebuild

.cabal-sandbox :
	cabal sandbox init

test : build
	rsync -avc --del --progress _site/ antea:public_html/staging

deploy : build
	rsync -avc --del --progress _site/ antea:public_html --exclude tmp --exclude norway --exclude staging --exclude dev --exclude doc

run : deploy
	-nohup firefox https://paradise.fi.muni.cz/~xstill < /dev/null > /dev/null 2> /dev/null &
