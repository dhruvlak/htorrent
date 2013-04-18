htorrent
========

Group15-htorrent
Contents:
  The folder htorrent contains the following files:
	tracker.hs
	client.hs
	.git
	dist	
	Setup.hs
	htorrent.cabal

How to run:
	$cd htorrent
	$cabal configure
	$cabal build
	$cabal install --prefix=$HOME --user

Run tracker:
	$htorrent –tracker
	
Run client:
	$htorrent-client
	
	To connect to tracker:
		htorrent>connect
	To seed a file:
		htorrent>seed
	To download a file:
		htorrent>download
	To refresh file list:
		htorrent>refresh
	To close client:
		htorrent>close
