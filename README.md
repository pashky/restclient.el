# restclient.el

This is a tool to manually explore and test HTTP REST webservices. Runs queries from a plain-text query sheet, displays results as a pretty-printed XML, JSON and even images.

# Usage

Deploy restclient.el into your site-lisp as usual, require it and make a text file with queries. C-c C-c runs the query under the cursor.

Query file example:

	# -*- restclient -*-
	#
	# Sync test
	#
	POST http://somehost.com/webservic/user/aba/sync?session=1331294083924
	Authorization: token=dfsdsffffffffffffff234435lkjsdfl
	X-HTTP-Method: PUT

	<?xml version="1.0" encoding="UTF-8"?><Update id="716762662"><items></items></Update>

	#
	# User details
	#
	GET http://91.206.1.172/webservices/user/aba?sessionId=1330619005027
	Authorization: token=dfsdsffffffffffffff234435lkjsdfl

	#
	#
	GET http://api.twitter.com/1/statuses/user_timeline.json?include_entities=true&include_rts=true&screen_name=twitterapi&count=2
	#
	#
	GET http://img.yandex.net/i/www/logo.png


Lines starting with `#` are considered comments AND also act as separators. Yes, that means you can't post shell script as PUT entity. But I don't care. 

HTTPS and image display requires additional dll's on windows (libtls, libpng, libjpeg etc), which are not in the emacs distribution.
