#!/usr/bin/env bash

function check_cmd() {
	if [ -z `which $1` ]; then
		echo "$1 could not be found." >&2
		echo "Please ensure that $2 is installed and $1 is in your PATH." >&2
		if [ -n "$3" ]; then
			echo "$3" >&2
		fi
		exit 1
	fi	
}

check_cmd pf Pamflet 'See http://pamflet.databinder.net/'
check_cmd mmd MultiMarkdown 'See http://fletcherpenney.net/multimarkdown/'
check_cmd sbt SBT 'See http://www.scala-sbt.org/'
check_cmd scala Scala 'See http://www.scala-lang.org/'

pushd $(cd ${0%/*}/.. && pwd -P) >/dev/null
trap 'popd >/dev/null' EXIT

SCALAV=`fgrep 'scalaVersion := ' ./build.sbt | sed -E 's/.+"(.+)"/\1/'`

rm -rf ./target/doc/*
mkdir -p ./target/doc

sbt doc &
SBT=$!

pf ./doc ./target/doc
# Uncomment to use offline version, only difference being
# the manifest and its usage in the <html> tag
# mv ./target/doc/offline/*.* ./target/doc
rm -r ./target/doc/offline

TABLES=`egrep -l '^[-—]+(\|[-—]+)+' ./target/doc/*.html`
for f in $TABLES; do
	./scripts/doc_fix -m 'mmd --nonotes' $f
done

wait $SBT
mv ./target/scala-$SCALAV/api ./target/doc/
