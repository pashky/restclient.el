# restclient.el

This is a tool to manually explore and test HTTP REST webservices.
Runs queries from a plain-text query sheet,
displays results as a pretty-printed XML, JSON and even images.

![](http://i.imgur.com/QtCID.png)

# Usage

You can easily install `restclient` using `package.el` from [MELPA](http://melpa.org/).

Alternatively, deploy `restclient.el` into your site-lisp as usual,
then add `(require 'restclient)` to your Emacs start-up file.

Once installed, you can prepare a text file with queries.

`restclient-mode` is a major mode which does a bit of highlighting
and supports a few additional keypresses:

- `C-c C-c`: runs the query under the cursor, tries to pretty-print the response (if possible)
- `C-c C-r`: same, but doesn't do anything with the response, just shows the buffer
- `C-c C-v`: same as `C-c C-c`, but doesn't switch focus to other window
- `C-c C-p`: jump to the previous query
- `C-c C-n`: jump to the next query
- `C-c C-.`: mark the query under the cursor
- `C-c C-u`: copy query under the cursor as a curl command

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

More examples can be found in the `examples` directory.

# In-buffer variables

You declare a variable like this:

    :myvar = the value

or like this:

    :myvar := (some (artbitrary 'elisp)

In second form, the value of variable is evaluated as Emacs Lisp form immediately. Evaluation of variables is done from top to bottom. Only one one-line form for each variable is allowed, so use `(progn ...)` and some virtual line wrap mode if you need more. There's no way to reference earlier declared _restclient_ variables, but you can always use `setq` to save state.

After the var is declared, you can use it in the URL, the header values
and the body.

    # Some generic vars

    :my-auth = 319854857345898457457

    # Update a user's name

    :user-id = 7
    :the-name := (format "%s %s %d" 'Neo (md5 "The Chosen") (+ 100 1))

    PUT http://localhost:4000/users/:user-id/
    Authorization: :my-auth

    { "name": ":the-name" }

Warning: If you include var declarations as part of the request, in
the body or headers, it will be sent along.

Instead, place them above your calls or in separate sections. Like in
the example above.

And be careful of what you put in that elisp. No security checks are done, so it can format your hardrive. If there's a parsing or evaluation error, it will tell you in the minibuffer.

# Customization

There are several variables available to customize `restclient` to your liking.

### restclient-log-request

__Default: t__

Determines whether restclient logs to the \*Messages\* buffer.

If non-nil, restclient requests will be logged. If nil, they will not be.

### restclient-same-buffer-response

__Default: t__

Re-use same buffer for responses or create a new one each time.

If non-nil, re-use the buffer named by `rest-client-buffer-response-name` for all requests.

If nil, generate a buffer name based on the request type and url, and increment it for subsequent requests.

For example, `GET http://example.org` would produce the following buffer names on 3 subsequent calls:
- `*HTTP GET http://example.org*`
- `*HTTP GET http://example.org*<2>`
- `*HTTP GET http://example.org*<3>`

### restclient-same-buffer-response-name

__Default: \*HTTP Response\*__

Name for response buffer to be used when `restclient-same-buffer-response` is true.

### restclient-inhibit-cookies

__Default: nil__

Inhibit restclient from sending cookies implicitly.

# Known issues

- Comment lines `#` act as end of entity. Yes, that means you can't post shell script or anything with hashes as PUT/POST entity. I'm fine with this right now,
but may use more unique separator in future.
- I'm not sure if it handles different encodings, I suspect it won't play well with anything non-ascii. I'm yet to figure it out.
- Variables usages are not highlighted

# License

Public domain, do whatever you want.

# Author

Pavel Kurnosov <pashky@gmail.com>
