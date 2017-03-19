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
- `C-c C-g`: start a [helm](https://emacs-helm.github.io/helm/) session with sources for variables and requests (if helm is available, of course)
- `C-c n n`: narrow to region of current request (including headers)

Query file example:

    # -*- restclient -*-
    #
    # Gets  all Github APIs, formats JSON, shows response status and headers underneath.
    # Also sends a User-Agent header, because the Github API requires this.
    #
    GET https://api.github.com
    User-Agent: Emacs Restclient

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
    Accept-Encoding: compress, gzip

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

Variables can be multiline too:

    :myvar = <<
    Authorization: :my-auth
    Content-Type: application/json
    User-Agent: SomeApp/1.0
    #

or

    :myvar := <<
    (some-long-elisp
        (code spanning many lines)
    #

`<<` is used to mark a start of multiline value, the actual value is starting on the next line then. The end of such variable value is the same comment marker `#` and last end of line doesn't count, same is for request bodies.

After the var is declared, you can use it in the URL, the header values
and the body.

    # Some generic vars

    :my-auth = 319854857345898457457
    :my-headers = <<
    Authorization: :my-auth
    Content-Type: application/json
    User-Agent: SomeApp/1.0
    #
    
    # Update a user's name

    :user-id = 7
    :the-name := (format "%s %s %d" 'Neo (md5 "The Chosen") (+ 100 1))

    PUT http://localhost:4000/users/:user-id/
    :my-headers

    { "name": ":the-name" }

# File uploads

Restclient now allows to specify file path to use as a body, like this:

    POST http://httpbin.org/post
    Content-type: text/plain

    < /etc/passwd
    
### Caveats:

- Multiline variables can be used in headers or body. In URL too, but it doesn't make sense unless it was long elisp expression evaluating to simple value.
- Yet same variable cannot contain both headers and body, it must be split into two and separated by empty line as usual.
- Variables now can reference each other, substitution happens in several passes and stops when there's no more variables. Please avoid circular references. There's customizable safeguard of maximum 10 passes to prevent hanging in this case, but it will slow things down.
- Variable declaration only considered above request line.
- Be careful of what you put in that elisp. No security checks are done, so it can format your hardrive. If there's a parsing or evaluation error, it will tell you in the minibuffer.
- Elisp variables can evaluate to values containing other variable references, this will be substituted too. But you cannot substitute parts of elisp expressions.

# Customization

There are several variables available to customize `restclient` to your liking. Also, all font lock faces are now customizable in `resclient-faces` group too.

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
- Variable usages are not highlighted
- Due to a [bug](http://debbugs.gnu.org/cgi/bugreport.cgi?bug=17976) in
  Emacs/url.el, some GET requests to `localhost` might fail. As a workaround you
  can use `127.0.0.1` instead of `localhost` until this is fixed.

# History

- _01/Aug/2016_ Added ability to narrow to region
- _06/Apr/2016_ Helm sources for variables and requests added.
- _06/Apr/2016_ File uploads! See upstairs for syntax.
- _06/Apr/2016_ Added customizable faces for all syntax highlighting, so it can be used in themes.
- _05/Apr/2016_ Added ability to declare multi-line variables (e.g. set of headers repeated for each request) and substitute variable values recursively.
- _25/Mar/2015_ Chop last newline from request body. If you really need to send one, just add one more, otherwise url-encoded POSTs will fail.
- _15/Jun/2013_ Added support for variables.

# Related 3rd party packages

- [company-restclient](https://github.com/iquiw/company-restclient): It provides auto-completion for HTTP methods and headers in restclient-mode. Completion source is given by know-your-http-well.
- [ob-restclient](https://github.com/alf/ob-restclient.el): An extension to restclient.el for emacs that provides org-babel support.
- [restclient.vim](https://github.com/bounceme/restclient.vim): Brings the restclient to vim! Responses display in vim's internal pager.

# License

Public domain, do whatever you want.

# Author

Pavel Kurnosov <pashky@gmail.com>
