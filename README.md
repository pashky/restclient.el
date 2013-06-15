# restclient.el

This is a tool to manually explore and test HTTP REST webservices.
Runs queries from a plain-text query sheet,
displays results as a pretty-printed XML, JSON and even images.

![](http://i.imgur.com/QtCID.png)

# Usage

Deploy `restclient.el` into your site-lisp as usual,
`(require 'restclient)` and prepare a text file with queries.

`restclient-mode` is a major mode which does a bit of highlighting
and supports a few additional keypresses:

- `C-c C-c`: runs the query under the cursor, tries to pretty-print the response (if possible)
- `C-c C-r`: same, but doesn't do anything with the response, just shows the buffer

Query file example:

        # -*- restclient -*-
        #
        # Gets user timeline, formats JSON, shows response status and headers underneath
        #
        #
        GET http://api.twitter.com/1/statuses/user_timeline.json?screen_name=twitterapi&count=2
        #
        # XML is supported - highlight, pretty-print
        #
        GET http://www.redmine.org/issues.xml?limit=10

        #
        # It can even show an image!
        #
        GET http://upload.wikimedia.org/wikipedia/commons/6/63/Wikipedia-logo.png
        #
        # A bit of json GET, you can pass headers too
        #
        GET http://jira.atlassian.com/rest/api/latest/issue/JRA-9
        User-Agent: Emacs24
        Accept-Encoding: application/xml

        #
        # Post works too, entity just goes after an empty line. Same is for PUT.
        #
        POST https://jira.atlassian.com/rest/api/2/search
        Content-Type: application/json

        {
                "jql": "project = HSP",
                "startAt": 0,
                "maxResults": 15,
                "fields": [
                        "summary",
                        "status",
                        "assignee"
                ]
        }
        #
        # And delete, will return not-found error...
        #
        DELETE https://jira.atlassian.com/rest/api/2/version/20


Lines starting with `#` are considered comments AND also act as separators.

HTTPS and image display requires additional dll's on windows (libtls, libpng, libjpeg etc), which are not in the emacs distribution.

# In-buffer variables

You declare a variable like this:

    :myvar = the value

After the var is declared, you can use it in the URL, the header values
and the body.

    # Some generic vars

    :my-auth = 319854857345898457457

    # Update a user's name

    :user-id = 7
    :the-name = Neo

    PUT http://localhost:4000/users/:user-id/
    Authorization: :my-auth

    { "name": ":the-name" }

Warning: If you include var declarations as part of the request, in
the body or headers, it will be sent along.

Instead, place them above your calls or in separate sections. Like in
the example above.

# Known issues

- Comment lines `#` act as end of enitity. Yes, that means you can't post shell script or anything with hashes as PUT/POST entity. I'm fine with this right now,
but may use more unique separator in future.
- I'm not sure if it handles different encodings, I suspect it won't play well with anything non-ascii. I'm yet to figure it out.

# License

Public domain, do whatever you want (apart from json-reformat.el thing, which is not mine)

# Author

Pavel Kurnosov <pashky@gmail.com>

